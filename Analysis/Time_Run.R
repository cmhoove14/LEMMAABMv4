# Test run

devtools::load_all() # library(LEMMAABMv4)
library(tictoc)

input_pars  <- readRDS(here::here("data/processed/input_pars.rds"))
data_inputs <- readRDS(here::here("data/processed/data_inputs.rds"))
vax_phases  <- readRDS(here::here("data/processed/vax65p_scenario.rds"))

bta_base    <- 0.25
bta_hh      <- 1.2
bta_work    <- 1.2
bta_sip_red <- 1/3

visitors <- TRUE 
testing <- TRUE 
adaptive <- FALSE 
vaccination <- FALSE 
verbose <- FALSE 
store_extra <- TRUE 

set.seed(430)

tic()

LEMMAABMv4::covid_abm_v4(bta_base    = bta_base, 
                         bta_hh      = bta_hh, 
                         bta_work    = bta_work, 
                         bta_sip_red = bta_sip_red, 
                         data_inputs = data_inputs, 
                         input_pars  = input_pars, 
                         vax_phases  = vax_phases,
                         visitors    = visitors, 
                         testing     = testing, 
                         adaptive    = adaptive, 
                         vaccination = vaccination,
                         verbose     = verbose, 
                         store_extra = store_extra)

toc()