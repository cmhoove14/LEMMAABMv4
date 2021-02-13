# ---------------------------------------
# Plot best fitting runs that were rerun in duplicate
# Chris Hoover Feb 2021
# ---------------------------------------
library(tidyverse)
library(ggplot2)
library(data.table)

# Get fits and lhs to refence fits and pars
fits <- readRDS(here::here("data", "processed", "LHS_Fits1_summary.rds"))
lhs  <- readRDS(here::here("data/processed/Calibration_LHS_Wynton.rds")) 

# Remove fits that resulted in 0 Hospitalizations
fits <- fits[fits$hosp_fit >0,]

# Sims with best fits to individual categories and best overall
best_hosp           <- fits$sim[which.min(fits$hosp_fit)]
best10_norm         <- fits$sim[which(fits$overall_fit_norm <= quantile(fits$overall_fit_norm, 10/nrow(fits), na.rm = T))]

lhs_reruns <- lhs[c(best_hosp, best10_norm),]
lhs_reruns_expand <- rbind(lhs_reruns[rep(1:nrow(lhs_reruns),each=10),])

rerun_sims <- rep(c(best_hosp, best10_norm),each=10)

# Get CA & SF data
load(here::here("data", "get", "got", "CA_SF_data2021-02-10.Rdata"))

# Util to grab simulation/fit files
sims_root <- here::here("data","outputs","Calibration_Reruns")

get_sim <- function(sim){
  # Get sim file  
  sim_folder <- here::here(sims_root, sim)
  sim <- readRDS(paste0(sim_folder,"/",list.files(sim_folder)[1]))
  
  return(sim)
}
