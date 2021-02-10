# ---------------------------------------
# Process fit scores
# Chris Hoover Feb 2021
# ---------------------------------------
library(tidyverse)
library(data.table)
library(zoo)

# Folders containing lists of outputs on wynton
root <- here::here("data","outputs","Calibration2_Fits")

n_tasks <- 1000

fitsdf <- bind_rows(lapply(1:n_tasks, function(x){
  # Get sim file  
  fit_folder <- here::here(root, x)
  fit <- readRDS(paste0(fit_folder,"/",list.files(fit_folder)[1]))
  
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

fitsdf_out <- fitsdf %>% 
  mutate(
    hosp_z                  = get_z(hosp_fit),
    dths_z                  = get_z(dths_fit),
    dths_race_z             = get_z(dths_race_fit),
    ct_cases_z              = get_z(ct_cases_fit),
    case_race_z             = get_z(case_race_fit),
    overall_z               = hosp_z + dths_z + dths_race_z + ct_cases_z + case_race_z,
    overall_fit             = hosp_fit+dths_fit+dths_race_fit+ct_cases_fit+case_race_fit,
    hosp_dths_race_fit      = hosp_fit+dths_fit+dths_race_fit+case_race_fit,
    overall_fit_norm        = hosp_fit_norm+dths_fit_norm+dths_race_fit_norm+ct_cases_fit_norm+case_race_fit_norm,
    hosp_dths_race_fit_norm = hosp_fit_norm+dths_fit_norm+dths_race_fit_norm+case_race_fit_norm
  ) %>% 
  arrange(overall_fit_norm)

saveRDS(fitsdf_out, here::here("data", "processed", "LHS_Fits2_summary.rds"))