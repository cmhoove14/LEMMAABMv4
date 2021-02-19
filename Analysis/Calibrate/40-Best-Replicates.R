# ---------------------------------------
# Run best fitting parameter set 100 times for variability in agents and transmission stochasticity
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
best_pars <- lhs[fits$sim[which.min(fits$hosp_fit)],]

# Load data and files from paths ---------------------
input_pars  <- readRDS(here::here("data/processed/input_pars_calibrate.rds"))
data_inputs <- readRDS(here::here("data/processed/data_inputs_calibrate.rds"))
vax_phases  <- readRDS(here::here("data/processed/vax65p_scenario.rds"))

visitors    <- TRUE 
testing     <- "S" 
vaccination <- FALSE 
verbose     <- FALSE 
store_extra <- TRUE 
initial     <- TRUE

# Replace pars in list with best pars from lhs -------------------
# Some of these manual entries as placeholders until calibration finalized

input_pars$trans_pars$bta_base             <- 0.22 # best_pars[1]
input_pars$trans_pars$bta_hh               <- 1.25 #best_pars[2]
input_pars$trans_pars$bta_work             <- 1.10 #best_pars[3]
input_pars$trans_pars$bta_sip_rd           <- 1.0 #best_pars[4]
input_pars$trans_pars$hpi_bta_mult         <- .33  #best_pars[5]

input_pars$quar_pars$q_prob_contact        <- best_pars[6]
input_pars$quar_pars$q_prob_resinf         <- best_pars[7]
input_pars$quar_pars$q_prob_symptoms       <- best_pars[8]
input_pars$quar_pars$q_prob_testpos        <- best_pars[9]
input_pars$quar_pars$q_prob_essential      <- best_pars[10]  
input_pars$quar_pars$q_bta_red_exp         <- best_pars[11]  
input_pars$quar_pars$known_contact_prob    <- best_pars[12]
input_pars$quar_pars$q_dur_mean            <- best_pars[13]

input_pars$test_pars$hpi_mult              <- 1 #best_pars[14]
input_pars$test_pars$income_mult           <- 1 #best_pars[15]
input_pars$test_pars$case_finding_mult     <- 0.01 #best_pars[16]
input_pars$test_pars$cont_mult             <- 10 #best_pars[17]
input_pars$test_pars$symp_mult             <- 10 #best_pars[18]
input_pars$test_pars$res_mult              <- 100 #best_pars[19]
input_pars$test_pars$symp_state_mult       <- 1000# best_pars[20]
input_pars$test_pars$hosp_mult             <- 10000 #best_pars[21]
input_pars$test_pars$essential_prob        <- 1 #best_pars[22]

input_pars$other_pars$mask_red             <- 0.6 #best_pars[23]
input_pars$other_pars$visitor_mult_testing <- 10 #best_pars[24]
input_pars$other_pars$visitor_mult_sfgrph  <- 50 #best_pars[25]
input_pars$other_pars$mort_mult            <- 0.25 #best_pars[26]

input_pars$init_states$E0                  <- 3 #best_pars[27]

library(LEMMAABMv4)

LEMMAABMv4::covid_abm_v4(data_inputs = data_inputs, 
                         input_pars  = input_pars, 
                         vax_phases  = vax_phases,
                         visitors    = visitors, 
                         testing     = testing, 
                         vaccination = vaccination,
                         verbose     = verbose, 
                         store_extra = store_extra,
                         initial     = initial,
                         output_path = paste0(output_path,taskID,"/"))
