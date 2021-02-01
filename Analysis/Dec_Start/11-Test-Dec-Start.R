devtools::load_all()
library(tictoc)

bta_base    = 0.20 
bta_hh      = 1 
bta_wrk    = 1 
bta_sip_red = 0.33 

data_inputs <- readRDS("data/processed/data_inputs_Dec_Start.rds" )
input_pars  <- readRDS("data/processed/input_pars_Dec_start.rds" )
vax_phases  <- readRDS("data/processed/vax65p_scenario.rds")
output_path <- "data/processed/Test_Dec_Start/"

visitors    <- TRUE 
testing     <- "S" 
vaccination <- "S" 
verbose     <- FALSE 
store_extra <- TRUE 

tic()
LEMMAABMv4::covid_abm_v4(bta_base    = bta_base, 
                         bta_hh      = bta_hh, 
                         bta_work    = bta_wrk, 
                         bta_sip_red = bta_sip_red, 
                         data_inputs = data_inputs, 
                         input_pars  = input_pars, 
                         vax_phases  = vax_phases,
                         visitors    = visitors, 
                         testing     = testing, 
                         vaccination = vaccination,
                         verbose     = verbose, 
                         store_extra = store_extra,
                         output_path = output_path)
toc()

