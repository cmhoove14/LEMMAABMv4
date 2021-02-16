# ---------------------------------------
# Sensitivity of model fits to different parameters
# Chris Hoover Feb 2021
# ---------------------------------------
library(tidyverse)
library(data.table)
library(abc)

opts <- commandArgs(TRUE)

taskID <- as.numeric(opts[1])

root <- here::here("data","outputs","Calibration_Outputs")

# get observed for date matching ------------------
load(here::here("data","get","got","CA_SF_data2021-02-10.Rdata"))
# Observed weekly deaths
dths_dates <- sf_case %>% filter(Date <= as.Date("2020-12-01")) %>% dplyr::select(Date) # For merging with sims to get dates right

# Get observed summaries to ensure matching of simulated to observed -----------
load("data/processed/obs_summaries_for_ABC.Rdata")

# Below creates matrix to fill with simulation observations, only need to do once
#matrix(data = NA, nrow = 2000, ncol = length(obs_sums)) -> sim_sum_fill ; saveRDS(sim_sum_fill, here::here("data/processed/sim_summaries_for_ABC.rds"))

# Get sim file   --------------------
sim_folder <- here::here(root, taskID)
sim <- readRDS(paste0(sim_folder,"/",list.files(sim_folder)[1]))

# Process sim to get sim smmaries -------------
# Hospitalizations  
  sim_hosp <- as_tibble(sim$epi_curve[state == "Ih",])
  sim_hosp$Date <- as.Date(as.character(sim_hosp$date)) # Date formate from sim messed up because of sub-daily time step
  
  comp_hosp <- merge(sim_hosp, sf_hosp, by = "Date")

  hosp_sim <- comp_hosp$N
  
  if(length(hosp_sim) != length(hosp_obs)){
    warning("Differing number of hospitalization observations between sim and observed")
  }
  
# Deaths   
  sim_dths <- as_tibble(sim$agents[state == "D", c("id", "sex", "age", "race", "t_death")]) %>% 
    mutate(
      tod = zoo::as.Date.numeric(t_death)
    ) %>% 
    arrange(tod) %>%
    right_join(dths_dates, by = c("tod" = "Date")) %>% 
    mutate(wod =paste0(lubridate::epiweek(tod), "_",
                       lubridate::year(tod))) %>% 
    group_by(wod) %>% 
    summarise(n_d_sim = sum(!is.na(id))) %>% 
    pull(n_d_sim)

  if(length(sim_dths) != length(dths_obs)){
    warning("Differing number of weekly death observations between sim and observed")
  }
  
  
# Deaths by race
  sim_dths_race <- as_tibble(sim$agents[state == "D", c("id", "sex", "age", "race", "t_death")]) %>% 
    mutate(race2 = if_else(race %in% c(1,2,8), race, 9)) %>% 
    group_by(race2) %>% 
    summarise(n_dths = n()) %>% 
    pull(n_dths)
  
  if(length(sim_dths_race) != length(dths_race_obs)){
    warning("Differing number of deaths by race observations between sim and observed")
  }
  
  
# Confirmed cases by race
  sim_case_race <- sim$linelist_tests %>% 
    filter(test_pos == 1 & Date <= as.Date("2020-12-01")) %>% 
    dplyr::select(-Date) %>% # Don't care about date since comparing cumulative counts
    group_by(race) %>% 
    summarise(n_sim = n()) %>% 
    pull(n_sim)
  
  if(length(sim_case_race) != length(case_race_obs)){
    warning("Differing number of cases by race observations between sim and observed")
  }
  
# add these summaries to matrix holding all summary values (probably not most efficient way to do this but it works)
  sim_sum_fill <- readRDS(here::here("data/processed/sim_summaries_for_ABC.rds"))
  sim_sum_fill[taskID,] <- c(hosp_sim, sim_dths, sim_dths_race,sim_case_race)
  saveRDS(sim_sum_fill, here::here("data/processed/sim_summaries_for_ABC.rds"))
  