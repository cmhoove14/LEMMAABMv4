# ---------------------------------------
# Assess fits
# Chris Hoover Feb 2021
# ---------------------------------------

library(tidyverse)
library(data.table)

# Fits run on cluster, transferred to local file
lhs_fits <- readRDS(here::here("data", "outputs", "LHS_Fits.rds"))
fits <- bind_rows(lapply(1:length(lhs_fits), function(x){
  out <- lhs_fits[[x]]
  
  if(!is.numeric(out[[2]])){
    out[2] <- 0
  } 
  return(out)
}))

fits <- fits %>% 
  mutate(tot_mse = log(hosp_mse)+ 
           log(dths_mse)+ 
           log(ct_cases_mse)+ 
           log(dths_race_mse)+
           log(case_race_mse))

# Number successful runs assessed
sum(fits$status)

best_hosp      <- fits[which.min(fits$hosp_mse),]
best_dths      <- fits[which.min(fits$dths_mse),]
best_dths_race <- fits[which.min(fits$dths_race_mse),]
best_ct_cases  <- fits[which.min(fits$ct_cases_mse),]
best_case_race <- fits[which.min(fits$case_race_mse),]
best_overall   <- fits[which.min(fits$tot_mse),]

# Sim 505 (best overall and best hospitalized fit) transferred from biostat cluster
fit_best <- readRDS(here::here("data", "outputs", "505","ABMv4_testingS_vaxFALSE_2020-02-17-2020-12-02sim1.rds"))

# Get CA & SF data
load(here::here("data", "get", "got", "CA_SF_data2021-02-04.Rdata"))

# Get input pars
input_pars  <- fit_best$input_pars
LEMMAABMv4::unpack_list(input_pars)

# Compare daily hospitalizations ----------------
sim_hosp <- as_tibble(fit_best$epi_curve[state == "Ih",])
sim_hosp$Date <- as.Date(as.character(sim_hosp$date)) # Date formate from sim messed up because of sub-daily time step

sim_hosp %>% 
  ggplot() +
  geom_col(data = sf_hosp %>% filter(Date <= t.end),
           aes(x = Date, y = HOSP_tot),
           col = "darkblue", fill = "blue", alpha = 0.4) +
  geom_line(aes(x = Date, y = N),
            size = 1.2, col = "red") +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  labs(y = "Active hospitalizations",
       title = "Daily hospitalizations fit")

ggsave(here::here("Plots", "LHS_Calibration", paste0("Best_Fit", Sys.Date(), "Hospitalizations.jpg")),
       units="in", width = 9, height = 5)

# Compare weekly deaths -------------------------
sim_dths <- as_tibble(fit_best$agents[state == "D", c("id", "sex", "age", "race", "t_death")]) %>% 
  mutate(
    tod = as.Date(as.character(zoo::as.Date.numeric(t_death)))
  ) %>% 
  arrange(tod) %>% 
  mutate(deaths = 1) %>% 
  group_by(tod) %>% 
  summarise(n_dth = sum(deaths)) %>% 
  ungroup() %>% 
  mutate(cum_deaths = cumsum(n_dth))

sim_dths %>% 
  ggplot() +
  geom_col(data = sf_case %>% filter(Date <= t.end),
           aes(x = Date, y = cum_death),
           col = "black", fill = "grey80", alpha = 0.4) +
  geom_line(aes(x = tod, y = cum_deaths),
            size = 1.2, col = "red") +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)) +
  labs(y = "Cumulative deaths",
       title = "Daily deaths fit")

ggsave(here::here("Plots", "LHS_Calibration", paste0("Best_Fit", Sys.Date(), "Deaths.jpg")),
       units="in", width = 4, height = 3)

# Compare cumulative Dec 1 deaths by race ---------------------
# State database only has non-hispanic white, non-hispanic black, hispanic, and other, so condense to match
sim_dths_race <- as_tibble(fit_best$agents[state == "D", c("id", "race", "t_death")]) %>% 
  mutate(race2 = if_else(race %in% c(1,2,8), race, 9)) %>% 
  group_by(race2) %>% 
  summarise("Sim_deaths" = n())

obs_dths_race <- data.frame(race2 = c(1, 2, 8, 9),
                            "Obs_deaths" = c(55,9,44,78))

comp_dths_race <- merge(sim_dths_race, obs_dths_race, by = "race2") %>% 
  pivot_longer(Sim_deaths:Obs_deaths) %>% 
  mutate(Race_Eth = case_when(race2 == 1 ~ "White only",
                              race2 == 2 ~ "Black only",
                              race2 == 8 ~ "Hispanic_Latinx",
                              race2 == 9 ~ "Other"))

comp_dths_race %>% 
  ggplot() +
    theme_classic() +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16)) +
    geom_bar(aes(x = Race_Eth, y = value, fill = name),
               stat = "identity", position = "dodge") +
    labs(x = "Race/Ethnicity",
         y = "Dec1 Deaths",
         fill = "",
         title = "Cumulative Deaths by Race fit")

ggsave(here::here("Plots", "LHS_Calibration", paste0("Best_Fit", Sys.Date(), "Deaths_race.jpg")),
       units="in", width = 5, height = 3)

# Compare cumulative Dec 1 confirmed cases by race ----------------------
sim_case_race <- fit_best$linelist_tests %>% 
  filter(test_pos == 1 & Date <= as.Date("2020-12-01")) %>% 
  dplyr::select(-Date) %>% # Don't care about date since comparing cumulative counts
  distinct() %>%  #Remove duplicate rows due to error mentioned above
  group_by(race) %>% 
  summarise(n_sim = n())

obs_case_race <- sf_case_race %>% 
  filter(Date == as.Date("2020-12-01")) %>% 
  dplyr::select(Race,Cum_Cases) %>% 
  rename("n_obs" = Cum_Cases)

# Quite a few NAs, so allocate them in proportion to cases with known race
# This assumes there aren't systematic biases in reporting of race among known cases, which, probably not true, but best we can do
case_race_non_na <- obs_case_race$n_obs[which(!is.na(obs_case_race$Race))]
case_race_na     <- obs_case_race$n_obs[which(is.na(obs_case_race$Race))]
obs_total        <- sum(case_race_non_na)
obs_ratios       <- case_race_non_na / obs_total
obs_add          <- round(case_race_na*obs_ratios)

obs_case_race2 <- obs_case_race[!is.na(obs_case_race$Race),]
obs_case_race2$n_obs <- obs_case_race2$n_obs + obs_add

comp_case_race <- merge(sim_case_race, obs_case_race2, by.x = "race", by.y = "Race") %>% 
  pivot_longer(n_sim:n_obs)%>% 
  mutate(Race_Eth = case_when(race == 1 ~ "White only",
                              race == 2 ~ "Black only",
                              race == 3 ~ "Am. Indian/AK Native",
                              race == 4 ~ "Asian only",
                              race == 5 ~ "HI/Pac Islander",
                              race == 6 ~ "Other",
                              race == 7 ~ "Two or more",
                              race == 8 ~ "Hispanic/Latinx"))

comp_case_race %>% 
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

ggsave(here::here("Plots", "LHS_Calibration", paste0("Best_Fit", Sys.Date(), "Cases_race.jpg")),
       units="in", width = 7, height = 5)
