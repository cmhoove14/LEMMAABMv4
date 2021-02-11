data.table::setDTthreads(1)

# Get options passed from BASH -----------------
opts <- commandArgs(TRUE)

taskID <- as.numeric(opts[1])

# Load data files from bash paths ---------------------
input_pars  <- readRDS(here::here("data","processed","input_pars_calibrate.rds"))
data_inputs <- readRDS(here::here("data", "processed", "data_inputs_calibrate.rds"))
vax_phases  <- readRDS(here::here("data", "processed", "vax65p_scenario.rds"))
output_path <- here::here("Scratch", taskID)

visitors    <- TRUE 
testing     <- "S" 
vaccination <- FALSE 
verbose     <- FALSE 
store_extra <- TRUE 

# Replace pars in list with manual pars -------------------
# Reduce run time for quicker runs
input_pars$time_pars$t.end <- as.Date("2020-10-01")
input_pars$time_pars$t.tot <- input_pars$time_pars$t.end - input_pars$time_pars$t0

input_pars$trans_pars$bta_base             <- 0.28
input_pars$trans_pars$bta_hh               <- 1
input_pars$trans_pars$bta_work             <- 1
input_pars$trans_pars$bta_sip_rd           <- 0.5
input_pars$trans_pars$hpi_bta_mult         <- 0.25

input_pars$other_pars$visitor_mult_testing <- 10
input_pars$other_pars$visitor_mult_sfgrph  <- 100

input_pars$other_pars$mort_mult            <- 0.5
input_pars$init_states$E0                  <- 3

library(LEMMAABMv4)

covid_abm_v4(data_inputs = data_inputs, 
             input_pars  = input_pars, 
             vax_phases  = vax_phases,
             visitors    = visitors, 
             testing     = testing, 
             vaccination = vaccination,
             verbose     = verbose, 
             store_extra = store_extra,
             output_path = paste0(output_path,"/"))
