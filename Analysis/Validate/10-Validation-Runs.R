# ---------------------
# Validation Runs for LEMMABMv4
# Chris Hoover Feb 2021
# ---------------------

data.table::setDTthreads(1)

# Get options passed from BASH -----------------
opts <- commandArgs(TRUE)

taskID <- as.numeric(opts[1])
data_inputs_path <- as.character(opts[2])
input_pars_path  <- as.character(opts[3])
vax_phases_path  <- as.character(opts[4])
output_path      <- as.character(opts[5]) 

# Get inputs ------------------
input_pars  <- readRDS(here::here(input_pars_path))
data_inputs <- readRDS(here::here(data_inputs_path))
vax_phases  <- readRDS(here::here(vax_phases_path))

if(length(opts) > 5){
  visitors    <- as.logical(opts[6]) 
  testing     <- opts[7]  
  vaccination <- as.logical(opts[8])
  verbose     <- as.logical(opts[9])  
  store_extra <- as.logical(opts[10])  
  initial     <- as.logical(opts[11])  
} else {
  visitors    <- TRUE 
  testing     <- "S" 
  vaccination <- FALSE 
  verbose     <- FALSE 
  store_extra <- TRUE 
  initial     <- FALSE
}

# Randomly sample a simulation from replicates to use in this simulation ---------------
n_rep_runs <- length(list.files(here::here("data/outputs/Best_Replicates3")))

use_rep    <- sample(1:n_rep_runs, 1)
use_file   <- here::here("data/outputs/Best_Replicates3", use_rep)

got_rep    <- readRDS(paste0(use_file, "/", list.files(use_file)[1]))

# Get agents and pars used in simulation run
data_inputs$agents     <- got_rep$agents
input_pars$trans_pars  <- got_rep$input_pars$trans_pars
input_pars$test_pars   <- got_rep$input_pars$test_pars
input_pars$quar_pars   <- got_rep$input_pars$quar_pars
input_pars$other_pars  <- got_rep$input_pars$other_pars

# Run simulation --------------
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
