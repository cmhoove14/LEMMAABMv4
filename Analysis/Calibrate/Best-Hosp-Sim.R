
library(data.table)
library(tidyverse)

# Get observed datasets for comparison --------------
load(here::here("data/get/got/CA_SF_data2021-02-02.Rdata"))

# Census tracts and agents
SF_cts <- readRDS(here::here("data/processed/SF_cts_sf.rds"))
sf_pop <- readRDS(here::here("data/processed/SF_agents_processed.rds"))

sim_files <- list.files(here::here("data/outputs/Calibration_Sims"))


# Observed vs simulated hospitalizations  
# Fill in MSE between simulated and observed to determine best fit plot below
hosps_fit <- numeric(length(sim_files))
sf_hosp_dt <- as.data.table(sf_hosp)

sim_hosps <- rbindlist(lapply(sim_files, function(s){
  sim <- readRDS(paste0("data/outputs/Calibration_Sims/", s))
  
  ref_date <- sim[["input_pars"]]$time_pars$ref_date
  bta_base <- sim[["input_pars"]]$trans_pars$bta_base
  sip_red  <- sim[["input_pars"]]$trans_pars$bta_sip_rd
  
  dt <- sim[["epi_curve"]]
  dt_Ih <- dt[state == "Ih",]
  dt_Ih[,ndays   := 1:nrow(dt_Ih)]
  dt_Ih[,Date    := as.Date(ref_date+ndays)]
  dt_Ih[,sip_red := sip_red]
  dt_Ih[,bta     := bta_base]
  dt_Ih[,bta_sip := paste0(bta, sip_red)]
  
  sim_obs_mrg <- data.table::merge.data.table(dt_Ih, sf_hosp_dt, by = "Date") 
  
  head(sim_obs_mrg)
  
  # Determine "fit"    
  if(nrow(sim_obs_mrg) >= 100){  #Don't count sims where infection dies out
    sim_obs_mrg[, MSE := (N-HOSP_tot)^2]
    
    hosps_fit[which(s == sim_files)] <- sum(sim_obs_mrg[, MSE], na.rm = T)
  } else {
    hosps_fit[which(s == sim_files)] <- NA_real_
  }
  return(dt_Ih)
  
}))


## Best "fit" hosp  
cat(hosps_fit,"\n")

best_file <- sim_files[which.min(hosps_fit)]

best_sim <- readRDS(paste0("data/outputs/Calibration_Sims/", best_file))

ref_date <- best_sim[["input_pars"]]$time_pars$ref_date
bta_base <- best_sim[["input_pars"]]$trans_pars$bta_base
sip_red  <- best_sim[["input_pars"]]$trans_pars$bta_sip_rd

dt_best <- best_sim[["epi_curve"]]
dt_best_Ih <- dt_best[state == "Ih",]
  dt_best_Ih[,ndays   := 1:nrow(dt_best_Ih)]
  dt_best_Ih[,Date    := as.Date(ref_date+ndays)]
  dt_best_Ih[,sip_red := sip_red]
  dt_best_Ih[,bta     := bta_base]
  dt_best_Ih[,bta_sip := paste0(bta, sip_red)]

sf_hosp %>% 
  ggplot() +
  geom_col(aes(x = Date, y = HOSP_tot), 
           col = "darkblue", fill = "blue",
           alpha = 0.4) +
  geom_line(data = dt_best_Ih,
            aes(x = Date, y = N), 
            col = "red",
            size = 1.2) +
  theme_classic() +
  labs(x = "Date", y = "Hospitalizations",
       title = "Hospitalizations best sim compared to observed")

ggsave(filename = "Plots/Init_Calibrate_Best_Hosps.jpg",
       units = "in", height = 5, width = 8)

saveRDS(best_sim, "data/outputs/Calibrate_Best_Hosp.rds")