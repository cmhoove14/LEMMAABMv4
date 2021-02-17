# ---------------------
# Validation Runs for LEMMABMv4
# Chris Hoover Feb 2021
# ---------------------

library(data.table)
data.table::setDTthreads(1)

# Get options passed from BASH -----------------
opts <- commandArgs(TRUE)

taskID           <- as.numeric(opts[1])
input_pars_path  <- as.character(opts[2])
scenarios_path   <- as.character(opts[3])
output_path      <- as.character(opts[4]) 

# Get inputs ------------------
input_pars  <- readRDS(here::here(input_pars_path))

if(length(opts) > 4){
  visitors    <- as.logical(opts[5]) 
  testing     <- opts[6]  
  vaccination <- as.logical(opts[7])
  verbose     <- as.logical(opts[8])  
  store_extra <- as.logical(opts[9])  
  initial     <- as.logical(opts[10])  
} else {
  visitors    <- TRUE 
  testing     <- "S" 
  vaccination <- TRUE 
  verbose     <- FALSE 
  store_extra <- TRUE 
  initial     <- FALSE
}

# Randomly sample a simulation from replicates to use in this simulation ---------------
n_rep_runs <- length(list.files(here::here("data/outputs/Best_Replicates2")))

use_rep    <- sample(1:n_rep_runs, 1)
use_file   <- here::here("data/outputs/Best_Replicates2", use_rep)

got_rep    <- readRDS(paste0(use_file, "/", list.files(use_file)[1]))

# Get pars used in simulation run ---------------------
input_pars$trans_pars  <- got_rep$input_pars$trans_pars
input_pars$test_pars   <- got_rep$input_pars$test_pars
input_pars$quar_pars   <- got_rep$input_pars$quar_pars
input_pars$other_pars  <- got_rep$input_pars$other_pars

# Get data inputs based on vax scenario forecast -----------
load(here::here(scenarios_path))

rep_scen <- vax_scens_reps[taskID,]

# movement data
if(rep_scen$MVMT == "OPT"){
  data_inputs <- readRDS(here::here("data/processed/data_inputs_forecast_opt.rds"))
} else if(rep_scen$MVMT == "PES"){
  data_inputs <- readRDS(here::here("data/processed/data_inputs_forecast_pes.rds"))
} else {
  stop("Movement setting not recognized")
}

# Vax availability data
if(rep_scen$VAX == "OPT"){
  data_inputs$vax_per_day <- readRDS(here::here("data/processed/vax_forecast_opt.rds"))
} else if(rep_scen$VAX == "PES"){
  data_inputs$vax_per_day <- readRDS(here::here("data/processed/vax_forecast_pes.rds"))
} else {
  stop("Vax availability setting not recognized")
}

# agents from simulation
data_inputs$agents     <- got_rep$agents
  agent_cts <- unique(data_inputs$agents$ct)

# Create vaccination phases based on vax scenario forecast -----------
vax_phases <- list()
  vax_phases$dates <- vax_phase_dates

vax_phases$ages    <- list()
vax_phases$occps   <- list()
vax_phases$cts     <- list()
vax_phases$type    <- list()  
vax_phases$metric  <- list()  

# Phase 1 always healthcare workers
  vax_phases$ages[[1]]   <- c(15:85)
  vax_phases$occps[[1]]  <- 10
  vax_phases$cts[[1]]    <- agent_cts
  vax_phases$type[[1]]   <- "S"
  vax_phases$metric[[1]] <- NA

# Phase 2 always 65p
  vax_phases$ages[[2]]  <- c(65:85)
  vax_phases$occps[[2]] <- 0:23
  vax_phases$cts[[2]]   <- agent_cts
  vax_phases$type[[2]]  <- "S"
  vax_phases$metric[[2]] <- NA
  
# Phase 3 determined by scenario inputs
if(rep_scen$TGT == "65p"){
  
  vax_phases$ages[[3]]  <- c(65:85)
  vax_phases$occps[[3]] <- 0:23
  vax_phases$cts[[3]]   <- agent_cts
  vax_phases$type[[3]]  <- "S"
  vax_phases$metric[[3]] <- NA
  
} else if(rep_scen$TGT == "PLACE" & rep_scen$MET == "HPI"){
  
  vax_phases$ages[[3]]  <- c(15:85)
  vax_phases$occps[[3]] <- 0:23
  vax_phases$cts[[3]]   <- unique(data_inputs$agents[hpi_quartile == 4, ct])
  vax_phases$type[[3]]  <- "S"
  vax_phases$metric[[3]] <- NA
} else if(rep_scen$TGT == "PLACE" & rep_scen$MET == "CASE"){
  
  vax_phases$ages[[3]]  <- c(15:85)
  vax_phases$occps[[3]] <- 0:23
  vax_phases$cts[[3]]   <- agent_cts
  vax_phases$type[[3]]  <- "A"
  vax_phases$metric[[3]] <- "CASE"
} else if(rep_scen$TGT == "PLACE" & rep_scen$MET == "HOSP"){
  
  vax_phases$ages[[3]]  <- c(15:85)
  vax_phases$occps[[3]] <- 0:23
  vax_phases$cts[[3]]   <- agent_cts
  vax_phases$type[[3]]  <- "A"
  vax_phases$metric[[3]] <- "HOSP"
}


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
