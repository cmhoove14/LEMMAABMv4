# ---------------------------------------
# Compare simulation to observed data metrics
# Chris Hoover Feb 2021
# ---------------------------------------
library(tidyverse)
library(data.table)

# Get simulation
sim_file <- "data/outputs/Oops/ABMv4_testingS_vaxFALSE_2020-02-17-2020-12-02sim1.rds"
  
sim <- readRDS(sim_file)  

# Get observed
load("data/get/got/CA_SF_data2021-02-04.Rdata")

# Compare daily hospitalizations ----------------
sim_hosp <- as_tibble(sim$epi_curve[state == "Ih",])
  sim_hosp$Date <- as.Date(as.character(sim_hosp$date)) # Date formate from sim messed up because of sub-daily time step

comp_hosp <- merge(sim_hosp, sf_hosp, by = "Date")

hosp_mse <- sum(log(comp_hosp$N/comp_hosp$HOSP_CONF)^2)

# Compare weekly deaths -------------------------
sim_dths <- as_tibble(sim$agents[state == "D", c("id", "sex", "age", "race", "t_death")]) %>% 
  mutate(
    tod = zoo::as.Date.numeric(t_death),
    wod =paste0(lubridate::epiweek(tod), "_",
                lubridate::year(tod))
  )

sim_dths_wk <- sim_dths %>% 
  group_by(wod) %>% 
  summarise(n_d_sim = n())

obs_dths_wk <- sf_case %>% 
  dplyr::select(Date, Deaths) %>% 
  mutate(wod = paste0(lubridate::epiweek(Date), "_",
                      lubridate::year(Date))) %>% 
  group_by(wod) %>% 
  summarise(n_d_obs = sum(Deaths))

comp_dths <- merge(sim_dths_wk, obs_dths_wk, by = "wod")

dths_mse <- sum((comp_dths$n_d_sim - comp_dths$n_d_obs)^2)

# Compare cumulative Dec 1 deaths by race ---------------------
sim_dths_race <- sim_dths %>% 
  group_by(race) %>% 
  summarise(n_dths = n())

obs_dths_race <- TBD

# Compare Monthly census tract confirmed cases -------------------
sim_cases <- sim$linelist_tests %>% 
  dplyr::select(id,race,state,ct, Date,test_pos)

# Compare cumulative Dec 1 confirmed cases by race ----------------------
  