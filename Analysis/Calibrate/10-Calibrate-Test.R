# Test run

devtools::load_all() # library(LEMMAABMv4)
library(tictoc)
library(progress)

input_pars  <- readRDS(here::here("data/processed/input_pars_calibrate.rds"))
data_inputs <- readRDS(here::here("data/processed/data_inputs_calibrate.rds"))
vax_phases  <- readRDS(here::here("data/processed/vax65p_scenario.rds"))

visitors <- TRUE 
testing <- "S" 
vaccination <- FALSE 
verbose <- TRUE 
store_extra <- FALSE 

set.seed(430)

tic()

time_run <- LEMMAABMv4::covid_abm_v4(data_inputs = data_inputs, 
                                     input_pars  = input_pars, 
                                     vax_phases  = vax_phases,
                                     visitors    = visitors, 
                                     testing     = testing, 
                                     vaccination = vaccination,
                                     verbose     = verbose, 
                                     store_extra = store_extra,
                                     output_path = paste0(here::here("data", "outputs"), "/"))

toc()