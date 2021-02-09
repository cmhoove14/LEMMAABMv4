# ---------------------------------------
# Process fit scores
# Chris Hoover Feb 2021
# ---------------------------------------
library(tidyverse)
library(data.table)
library(zoo)

# Folders containing lists of outputs on wynton
root <- here::here("data","outputs","Calibration_Fits")

n_tasks <- 1440

fitsdf <- bind_rows(lapply(1:n_tasks, function(x){
  # Get sim file  
  fit_folder <- here::here(root, x)
  fit <- readRDS(paste0(sim_folder,"/",list.files(sim_folder)[1]))
  
  fits <- fit$fits
  
  return(fits)
}))

if(sum(fitsdf$status) != n_tasks){
  warning("Some sims returned 0 status")
}

get_z <- function(vec){
  mu   <- mean(vec)
  stdv <- sd(vec)
  z    <- (vec-mu)/stdv
  
  return(z)
}

fitsdf_out <- fits_df %>% 
  mutate(
    hosp_z      = get_z(hosp_mse),
    dths_z      = get_z(dths_mse),
    dths_race_z = get_z(dths_race_mse),
    ct_cases_z  = get_z(ct_cases_mse),
    case_race_z = get_z(case_race_mse),
    overall_z   = hosp_z + dths_z + dths_race_z + ct_cases_z + case_race_z
  ) %>% 
  arrange(-overall_z)

saveRDS(fitsdf_out, here::here("data", "processed", "LHS_Fits1_summary.rds"))