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

# Get sim file  
sim_folder <- here::here(root, taskID)
sim <- readRDS(paste0(sim_folder,"/",list.files(sim_folder)[1]))

# Process sim to get sim smmaries
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



sim_hosp <- as_tibble(sim$epi_curve[state == "Ih",])
sim_hosp$Date <- as.Date(as.character(sim_hosp$date)) # Date formate from sim messed up because of sub-daily time step


