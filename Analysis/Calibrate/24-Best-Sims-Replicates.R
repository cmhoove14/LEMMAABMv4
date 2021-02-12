# ---------------------------------------
# Run best fitting parameter sets in duplicate
# Chris Hoover Feb 2021
# ---------------------------------------
data.table::setDTthreads(1)

# Get options passed from BASH -----------------
opts <- commandArgs(TRUE)

taskID <- as.numeric(opts[1])
output_path <- as.character(opts[2])

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


# Load data and files from paths ---------------------
  input_pars  <- readRDS(here::here("data/processed/input_pars_calibrate.rds"))
  data_inputs <- readRDS(here::here("data/processed/data_inputs_calibrate.rds"))
  vax_phases  <- readRDS(here::here("data/processed/vax65p_scenario.rds"))
  
  visitors    <- TRUE 
  testing     <- "S" 
  vaccination <- FALSE 
  verbose     <- FALSE 
  store_extra <- TRUE 
  
# Replace pars in list with pars from lhs -------------------
  this_parset <- lhs_reruns_expand[taskID,]
  
  input_pars$trans_pars$bta_base             <- this_parset[1]
  input_pars$trans_pars$bta_hh               <- this_parset[2]
  input_pars$trans_pars$bta_work             <- this_parset[3]
  input_pars$trans_pars$bta_sip_rd           <- this_parset[4]
  input_pars$trans_pars$hpi_bta_mult         <- this_parset[5]
  
  input_pars$quar_pars$q_prob_contact        <- this_parset[6]
  input_pars$quar_pars$q_prob_resinf         <- this_parset[7]
  input_pars$quar_pars$q_prob_symptoms       <- this_parset[8]
  input_pars$quar_pars$q_prob_testpos        <- this_parset[9]
  input_pars$quar_pars$q_prob_essential      <- this_parset[10]  
  input_pars$quar_pars$q_bta_red_exp         <- this_parset[11]  
  input_pars$quar_pars$known_contact_prob    <- this_parset[12]
  input_pars$quar_pars$q_dur_mean            <- this_parset[13]
  
  input_pars$test_pars$hpi_mult              <- this_parset[14]
  input_pars$test_pars$income_mult           <- this_parset[15]
  input_pars$test_pars$case_finding_mult     <- this_parset[16]
  input_pars$test_pars$cont_mult             <- this_parset[17]
  input_pars$test_pars$symp_mult             <- this_parset[18]
  input_pars$test_pars$res_mult              <- this_parset[19]
  input_pars$test_pars$symp_state_mult       <- this_parset[20]
  input_pars$test_pars$hosp_mult             <- this_parset[21]
  input_pars$test_pars$essential_prob        <- this_parset[22]
  
  input_pars$other_pars$mask_red             <- this_parset[23]
  input_pars$other_pars$visitor_mult_testing <- this_parset[24]
  input_pars$other_pars$visitor_mult_sfgrph  <- this_parset[25]
  input_pars$other_pars$mort_mult            <- this_parset[26]
  
  input_pars$init_states$E0                  <- this_parset[27]
  
library(LEMMAABMv4)
  
  LEMMAABMv4::covid_abm_v4(data_inputs = data_inputs, 
                           input_pars  = input_pars, 
                           vax_phases  = vax_phases,
                           visitors    = visitors, 
                           testing     = testing, 
                           vaccination = vaccination,
                           verbose     = verbose, 
                           store_extra = store_extra,
                           output_path = paste0(output_path,taskID,"/"))
  