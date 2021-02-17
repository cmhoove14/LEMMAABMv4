# ---------------------------------------
# Sensitivity of model fits to different parameters
# Chris Hoover Feb 2021
# ---------------------------------------
library(tidyverse)
library(data.table)
library(abc)

# Get fits and lhs to reference fits and pars
fits <- readRDS(here::here("data", "processed", "LHS_Fits1_summary.rds"))
lhs  <- readRDS(here::here("data/processed/Calibration_LHS_Wynton.rds")) 

# Remove fits that resulted in 0 Hospitalizations
bad_fits <- which(fits$hosp_fit <= 0)
fits     <- fits[-bad_fits,]
lhs_filt <- lhs[-bad_fits,]

# Get obs summaries
load(here::here("data/processed/obs_summaries_for_ABC.Rdata"))

# Get sim summaries
# Only have to do this once to process individual summaries into matrix, then can load from below
# sim_sums <- do.call(rbind,lapply(1:2000, function(i){
#   readRDS(here::here("data/outputs/Calibration_ABC_Sums", paste0("ABC_Sums_", i)))
# }))
# 
# saveRDS(sim_sums, here::here("data/processed/sim_summaries_for_ABC.rds"))

# Load if after runnig above
sim_sums <- readRDS(here::here("data/processed/sim_summaries_for_ABC.rds"))

# Make sure in right order
sim_sums <- sim_sums[order(sim_sums[,1]),]

# Get rid of column with sim number and columns with "bad sims" id'ed above
sim_sums <- sim_sums[,-1]
sim_sums <- sim_sums[-bad_fits,]

# ABC do your thang -----------------
abc_est <- abc(target = obs_sums,
               param = lhs_filt,  
               sumstat = sim_sums,  
               tol = round(nrow(fits)*0.01),
               transf = "log",
               method = "neuralnet")
