# ---------------------------------------
# Assess fit scores
# Chris Hoover Feb 2021
# ---------------------------------------
library(tidyverse)
library(data.table)

# Get CA & SF data
load(here::here("data", "get", "got", "CA_SF_data2021-02-10.Rdata"))

# Utils to grab simulation/fit files
sims_root <- here::here("Scratch")

get_sim <- function(sim){
  # Get sim file  
  sim_folder <- here::here(sims_root, sim)
  sim <- readRDS(paste0(sim_folder,"/",list.files(sim_folder)[1]))
  
  return(sim)
}

# Get input pars for reference dates
sim1 <- get_sim(1)
input_pars  <- sim1$input_pars
LEMMAABMv4::unpack_list(input_pars)

# Plot vars
par(mar = c(3,2,2,0.1),
    mgp = c(1.5,0.5,0))

# Compare daily hospitalizations ----------------
jpeg(here::here("Scratch", "hosp_sims_100sims_bta028.jpg"),
     height = 4, width = 7, units = "in", res = 100)

#Plot of observed data with range due to PUI
plot(x    = sf_hosp$Date[which(sf_hosp$Date <= t.end)], 
     y    = sf_hosp$HOSP_tot[which(sf_hosp$Date <= t.end)], 
     type = "h", 
     lwd  = 1, 
     col  = "black",
     ylab = "Hospitalizations")

  segments(x0    = sf_hosp$Date[which(sf_hosp$Date <= t.end)], 
           x1    = sf_hosp$Date[which(sf_hosp$Date <= t.end)],
           y0    = sf_hosp$HOSP_tot[which(sf_hosp$Date <= t.end)],
           y0    = sf_hosp$HOSP_max[which(sf_hosp$Date <= t.end)],
           col   = "grey50")
  
  points(x    = sf_hosp$Date[which(sf_hosp$Date <= t.end)], 
         y    = sf_hosp$HOSP_tot[which(sf_hosp$Date <= t.end)],
         pch  = 16)
  
  points(x    = sf_hosp$Date[which(sf_hosp$Date <= t.end)], 
         y    = sf_hosp$HOSP_max[which(sf_hosp$Date <= t.end)],
         pch  = 16)

# Add sims  
  for(s in 1:100){
    hosp_sim <- get_fit(s)$hosp
    lines(hosp_sim$Date, hosp_sim$N, col = "red", lwd = 0.5)
  }
  
dev.off()  


# Compare weekly deaths -------------------------
best_dths_sim <- as_tibble(get_sim(best_dths)$agents[state == "D", c("id", "sex", "age", "race", "t_death")]) %>% 
  mutate(
    tod = as.Date(as.character(zoo::as.Date.numeric(t_death)))
  ) %>% 
  group_by(tod) %>% 
  summarise(n_d_sim = n())

jpeg(here::here("Plots", "LHS_Calibration", "Fits2", "dths_sims_best.jpg"),
     height = 4, width = 7, units = "in", res = 100)

plot(x    = sf_case$Date[which(sf_case$Date <= t.end)], 
     y    = sf_case$cum_death[which(sf_case$Date <= t.end)], 
     type = "h", 
     lwd  = 1, 
     col  = "grey50",
     ylab = "Sumulative Deaths")

lines(best_dths_sim$tod, best_dths_sim$n_d_sim, col = "red")

for(s in best10_norm){
  dths_sim <- as_tibble(get_sim(s)$agents[state == "D", c("id", "sex", "age", "race", "t_death")]) %>% 
    mutate(
      tod = as.Date(as.character(zoo::as.Date.numeric(t_death)))
    ) %>% 
    group_by(tod) %>% 
    summarise(n_d_sim = n())
  
  lines(dths_sim$tod, dths_sim$n_d_sim, col = "blue")
}

legend("topleft", bty = "n",
       lty = 1, col = c("red", "blue"),
       legend = c("Best fit", "Top10 Overall"),
       cex = 0.7)

dev.off()  


# Compare cumulative Dec 1 deaths by race ---------------------
# State database only has non-hispanic white, non-hispanic black, hispanic, and other, so condense to match
best_dths_race_sim <- get_fit(best_dths_race)$dths_race %>% 
  mutate(Race_Eth = case_when(race2 == 1 ~ "White only",
                              race2 == 2 ~ "Black only",
                              race2 == 8 ~ "Hispanic_Latinx",
                              race2 == 9 ~ "Other")) %>% 
  rename("Sim" = n_dths,
         "Obs" = deaths) %>% 
  pivot_longer(Sim:Obs)

best_dths_race_sim %>% 
  ggplot() +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16)) +
  geom_bar(aes(x = Race_Eth, y = value, fill = name),
           stat = "identity", position = "dodge") +
  labs(x = "Race/Ethnicity",
       y = "Dec1 Deaths",
       fill = "",
       title = "Cumulative Deaths by Race fit")

ggsave(here::here("Plots", "LHS_Calibration", "Fits2", "dths_race_sims_best.jpg"),
       units="in", width = 5, height = 3)

# Compare cumulative Dec 1 confirmed cases by race ----------------------
best_case_race_sim <- get_fit(best_case_race)$case_race %>% 
  rename("Sim" = n_sim,
         "Obs" = n_obs) %>% 
  pivot_longer(Sim:Obs) %>% 
  mutate(Race_Eth = case_when(race == 1 ~ "White only",
                              race == 2 ~ "Black only",
                              race == 3 ~ "Am. Indian/AK Native",
                              race == 4 ~ "Asian only",
                              race == 5 ~ "HI/Pac Islander",
                              race == 6 ~ "Other",
                              race == 7 ~ "Two or more",
                              race == 8 ~ "Hispanic/Latinx"))

best_case_race_sim %>% 
  ggplot() +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 16)) +
  geom_bar(aes(x = Race_Eth, y = value, fill = name),
           stat = "identity", position = "dodge") +
  labs(x = "Race/Ethnicity",
       y = "Dec1 Cases",
       fill = "",
       title = "Cumulative Cases by Race fit")

ggsave(here::here("Plots", "LHS_Calibration", "Fits2", "case_race_sims_best.jpg"),
       units="in", width = 7, height = 5)

# Compare CT Cases by month ----------------------
best_ct_cases_sim <- get_fit(best_ct_cases)$ct_cases %>% 
  mutate(mse = (n_sim - n_obs)^2)

best_ct_cases_sim %>% 
  ggplot(aes(x = MO_YR, y = mse)) +
    geom_violin() +
    geom_jitter(aes(col = as.factor(ct)),
                shape = 16, position = position_jitter(0.2)) +
    theme_classic() +
    theme(legend.position = "none")
    
ggsave(here::here("Plots", "LHS_Calibration", "Fits2", "ct_cases_sims_best.jpg"),
       units="in", width = 7, height = 5)



# Compare confirmed cases and test positivity ----------------------

jpeg(here::here("Plots", "LHS_Calibration", "Fits2", "best10_confirmed.jpg"),
     height = 4, width = 7, units = "in", res = 100)

plot(x    = sf_test$Date[which(sf_hosp$Date <= t.end)], 
     y    = sf_test$pos[which(sf_hosp$Date <= t.end)], 
     type = "h", 
     lwd  = 1, 
     col  = "black",
     ylab = "Test positive %")

for(s in best10_norm){
  tests_sim <- get_sim(s)$linelist_tests
  
  tests_sum_by_date <- tests_sim %>% 
    group_by(Date) %>% 
    summarise(n_tests = n(),
              n_pos = sum(test_pos),
              per_pos = n_pos/n_tests)
  
  lines(tests_sum_by_date$Date, tests_sum_by_date$n_pos, col = "coral")
}

dev.off()  


jpeg(here::here("Plots", "LHS_Calibration", "Fits2", "best10_test_pct.jpg"),
     height = 4, width = 7, units = "in", res = 100)

plot(x    = sf_test$Date[which(sf_hosp$Date <= t.end)], 
     y    = sf_test$pct[which(sf_hosp$Date <= t.end)], 
     type = "h", 
     lwd  = 1, 
     col  = "black",
     ylab = "Test positive %")

for(s in best10_norm){
  tests_sim <- get_sim(s)$linelist_tests
  
  tests_sum_by_date <- tests_sim %>% 
    group_by(Date) %>% 
    summarise(n_tests = n(),
              n_pos = sum(test_pos),
              per_pos = n_pos/n_tests)
  
  lines(tests_sum_by_date$Date, tests_sum_by_date$per_pos, col = "blue")
}

dev.off()  


# Look at all sims hospitalizations in first wave -------------------
jpeg(here::here("Plots", "LHS_Calibration", "Fits2", "first_wave_hosp.jpg"),
     height = 4, width = 7, units = "in", res = 100)

plot(x    = sf_hosp$Date[which(sf_hosp$Date <= as.Date("2020-06-15"))], 
     y    = sf_hosp$HOSP_tot[which(sf_hosp$Date <= as.Date("2020-06-15"))], 
     type = "h", 
     lwd  = 1, 
     col  = "grey50",
     ylab = "Hospitalizations",
     main = "First wave hospitalizations")

for(s in 1:1000){
  hosp_sim <- get_fit(s)$hosp
  # Filter for sims fitting first wave criteria
  if(hosp_sim$N[hosp_sim$Date == as.Date("2020-05-01")] > hosp_sim$N[hosp_sim$Date == as.Date("2020-06-01")]){
    lines(hosp_sim$Date[which(hosp_sim$Date <= as.Date("2020-06-15"))], 
          hosp_sim$N[which(hosp_sim$Date <= as.Date("2020-06-15"))], 
          col = "lightblue", lwd = 0.5)    
  }

}

dev.off()  

# Look at all sims hospitalizations in second wave -------------------
scndwavewinners <- numeric()

jpeg(here::here("Plots", "LHS_Calibration", "Fits2", "second_wave_hosp.jpg"),
     height = 4, width = 7, units = "in", res = 100)

plot(x    = sf_hosp$Date[which(sf_hosp$Date >= as.Date("2020-06-15") & sf_hosp$Date <= t.end)], 
     y    = sf_hosp$HOSP_tot[which(sf_hosp$Date >= as.Date("2020-06-15") & sf_hosp$Date <= t.end)], 
     type = "h", 
     lwd  = 1, 
     col  = "grey50",
     ylab = "Hospitalizations",
     main = "Second wave hospitalizations")

for(s in 1:1000){
  hosp_sim <- get_fit(s)$hosp
  
  # Filter for second wave criteria
  if((hosp_sim$N[hosp_sim$Date == as.Date("2020-08-01")] - hosp_sim$N[hosp_sim$Date == as.Date("2020-07-01")]) > 20 &
     (hosp_sim$N[hosp_sim$Date == as.Date("2020-08-01")] - hosp_sim$N[hosp_sim$Date == as.Date("2020-11-01")]) > 20){
    
    scndwavewinners <- c(scndwavewinners, s)
    
    lines(hosp_sim$Date[which(hosp_sim$Date >= as.Date("2020-06-15") & hosp_sim$Date <= t.end)], 
          hosp_sim$N[which(hosp_sim$Date >= as.Date("2020-06-15") & hosp_sim$Date <= t.end)], 
          col = "blue")
    
  }  
}

dev.off()  

cat(scndwavewinners)