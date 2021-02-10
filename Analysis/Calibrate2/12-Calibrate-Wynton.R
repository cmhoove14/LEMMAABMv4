
data.table::setDTthreads(1)

# Get options passed from BASH -----------------
opts <- commandArgs(TRUE)

taskID <- as.numeric(opts[1])
data_inputs_path <- as.character(opts[2])
input_pars_path  <- as.character(opts[3])
vax_phases_path  <- as.character(opts[4])
output_path      <- as.character(opts[5]) 

if(length(opts) > 5){
  visitors    <- as.logical(opts[6]) 
  testing     <- opts[7]  
  vaccination <- as.logical(opts[8])
  verbose     <- as.logical(opts[9])  
  store_extra <- as.logical(opts[10])  
} else {
  visitors    <- TRUE 
  testing     <- "S" 
  vaccination <- FALSE 
  verbose     <- FALSE 
  store_extra <- TRUE 
}

cat("\n", opts, "\n")


# Load data files from bash paths ---------------------
input_pars  <- readRDS(here::here(input_pars_path))
data_inputs <- readRDS(here::here(data_inputs_path))
vax_phases  <- readRDS(here::here(vax_phases_path))

# Replace pars in list with pars from lhs -------------------
lhs <- readRDS("data/processed/Calibration_LHS2_Wynton.rds")

input_pars$trans_pars$bta_base             <- lhs[taskID,1]
input_pars$trans_pars$bta_hh               <- lhs[taskID,2]
input_pars$trans_pars$bta_work             <- lhs[taskID,3]
input_pars$trans_pars$bta_sip_rd           <- lhs[taskID,4]
input_pars$trans_pars$hpi_bta_mult         <- lhs[taskID,5]

input_pars$quar_pars$q_prob_contact        <- lhs[taskID,6]
input_pars$quar_pars$q_prob_resinf         <- lhs[taskID,7]
input_pars$quar_pars$q_prob_symptoms       <- lhs[taskID,8]
input_pars$quar_pars$q_prob_testpos        <- lhs[taskID,9]
input_pars$quar_pars$q_prob_essential      <- lhs[taskID,10]  
input_pars$quar_pars$q_bta_red_exp         <- lhs[taskID,11]  
input_pars$quar_pars$known_contact_prob    <- lhs[taskID,12]
input_pars$quar_pars$q_dur_mean            <- lhs[taskID,13]

input_pars$other_pars$mort_mult            <- lhs[taskID,14]
input_pars$init_states$E0                  <- lhs[taskID,15]

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
