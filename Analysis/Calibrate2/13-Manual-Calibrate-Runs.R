# ---------------------------------------
# Manual calibration runs
# Chris Hoover Feb 2021
# ---------------------------------------

# Load data files from bash paths ---------------------
input_pars  <- readRDS(here::here("data","processed","input_pars_calibrate.rds"))
data_inputs <- readRDS(here::here("data", "processed", "data_inputs_calibrate.rds"))
vax_phases  <- readRDS(here::here("data", "processed", "vax65p_scenario.rds"))
output_path <- here::here("Scratch")

  visitors    <- TRUE 
  testing     <- "S" 
  vaccination <- FALSE 
  verbose     <- FALSE 
  store_extra <- TRUE 
  
# Replace pars in list with manual pars -------------------

input_pars$trans_pars$bta_base             <- 0.26
input_pars$trans_pars$bta_hh               <- 1.4
input_pars$trans_pars$bta_work             <- 1.1
input_pars$trans_pars$bta_sip_rd           <- 0.75
input_pars$trans_pars$hpi_bta_mult         <- 0.25

input_pars$other_pars$mort_mult            <- 0.5
input_pars$init_states$E0                  <- 3

devtools::load_all()

covid_abm_v4(data_inputs = data_inputs, 
             input_pars  = input_pars, 
             vax_phases  = vax_phases,
             visitors    = visitors, 
             testing     = testing, 
             vaccination = vaccination,
             verbose     = verbose, 
             store_extra = store_extra,
             output_path = paste0(output_path,"/"))
