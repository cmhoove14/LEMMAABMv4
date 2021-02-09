# ---------------------------------------
# Assess fit scores
# Chris Hoover Feb 2021
# ---------------------------------------
library(tidyverse)

fits <- readRDS(here::here("data", "processed", "LHS_Fits1_summary.rds"))

# Sims with best fits to individual categories and best overall
best_hosp      <- fits$sim[which.min(fits$hosp_mse)]
best_dths      <- fits$sim[which.min(fits$dths_mse)]
best_dths_race <- fits$sim[which.min(fits$dths_race_mse)]
best_ct_cases  <- fits$sim[which.min(fits$ct_cases_mse)]
best_case_race <- fits$sim[which.min(fits$case_race_mse)]
best_overall   <- fits$sim[which.min(fits$overall_z)]
best10         <- fits$sim[which(fits$overall_z <= quantile(fits$overall_z, 10/nrow(fits)))]

# Get CA & SF data
load(here::here("data", "get", "got", "CA_SF_data2021-02-04.Rdata"))

# Utils to grab simulation/fit files
sims_root <- here::here("data","outputs","Calibration_Outputs")

get_sim <- function(sim){
  # Get sim file  
  sim_folder <- here::here(sims_root, sim)
  sim <- readRDS(paste0(sim_folder,"/",list.files(sim_folder)[1]))
  
  return(sim)
}

fits_root <- here::here("data","outputs","Calibration_Fits")

get_fit <- function(sim){
  # Get sim file  
  fit_folder <- here::here(fits_root, sim)
  fit <- readRDS(paste0(fit_folder,"/",list.files(fit_folder)[1]))
  
  return(fit$comp_dfs)
}

# Get input pars for reference dates
best_overall_sim <- get_sim(best_overall)
input_pars  <- best_overall_sim$input_pars
LEMMAABMv4::unpack_list(input_pars)

# Plot vars
par(mar = c(3,2,2,0.1),
    mgp = c(1.5,0.5,0))

# Compare daily hospitalizations ----------------
best_hosp_sim <- get_fit(best_hosp)$hosp

jpeg(here::here("Plots", "LHS_Calibration", "Fits1", "hosp_sims_best.jpg"),
     height = 4, width = 7, units = "in", res = 100)

plot(x    = sf_hosp$Date[which(sf_hosp$Date <= t.end)], 
     y    = sf_hosp$HOSP_tot[which(sf_hosp$Date <= t.end)], 
     type = "h", 
     lwd  = 1, 
     col  = "grey50",
     ylab = "Hospitalizations")

  lines(best_hosp_sim$Date, best_hosp_sim$N, col = "red")

  for(s in best10){
    hosp_sim <- get_fit(s)$hosp
    lines(hosp_sim$Date, hosp_sim$N, col = "blue")
  }
  
  legend("topleft", bty = "n",
         lty = 1, col = c("red", "blue"),
         legend = c("Best fit", "Top10 Overall"),
         cex = 0.7)
  
dev.off()  


# Compare weekly deaths -------------------------
best_dths_sim <- as_tibble(get_sim(best_dths)$agents[state == "D", c("id", "sex", "age", "race", "t_death")]) %>% 
  mutate(
    tod = as.Date(as.character(zoo::as.Date.numeric(t_death)))
  ) %>% 
  group_by(tod) %>% 
  summarise(n_d_sim = n())

jpeg(here::here("Plots", "LHS_Calibration", "Fits1", "dths_sims_best.jpg"),
     height = 4, width = 7, units = "in", res = 100)

plot(x    = sf_case$Date[which(sf_case$Date <= t.end)], 
     y    = sf_case$cum_death[which(sf_case$Date <= t.end)], 
     type = "h", 
     lwd  = 1, 
     col  = "grey50",
     ylab = "Sumulative Deaths")

lines(best_dths_sim$tod, best_dths_sim$n_d_sim, col = "red")

for(s in best10){
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
best_dths_race_sim <- get_sim(best_dths_race)$dths_race %>% 
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

ggsave(here::here("Plots", "LHS_Calibration", "Fits1", "dths_race_sims_best.jpg"),
       units="in", width = 5, height = 3)

# Compare cumulative Dec 1 confirmed cases by race ----------------------
best_case_race_sim <- get_sim(best_case_race)$case_race %>% 
  rename("Sim" = n_sim,
         "Obs" = n_obs)
  pivot_longer(Sim:Obs)
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

ggsave(here::here("Plots", "LHS_Calibration", "Fits1", "case_race_sims_best.jpg"),
       units="in", width = 7, height = 5)

# Compare CT Cases by month ----------------------
best_ct_cases_sim <- get_sim(best_ct_cases)$ct_cases %>% 
  mutate(mse = (n_sim - n_obs)^2)

best_ct_cases_sim %>% 
  ggplot(aes(x = MO_YR, y = mse)) +
    geom_violin() +
    geom_jitter(aes(col = as.factor(ct)),
                shape = 16, position = position_jitter(0.2)) +
    theme_classic() +
    theme(legend.position = "none")
    
ggsave(here::here("Plots", "LHS_Calibration", "Fits1", "ct_cases_sims_best.jpg"),
       units="in", width = 7, height = 5)

