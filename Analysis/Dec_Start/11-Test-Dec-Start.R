devtools::load_all()
library(tictoc)

x = 0.10 
bta_hh = 1 
bta_work = 1 
bta_sip_red = 0.33 

input_pars <- readRDS("data/processed/data_inputs_Dec_Start.rds" )
data_inputs <- readRDS("data/processed/input_pars_Dec_start.rds" )
vax_phases <- readRDS("data/processed/vax65p_scenario.rds")

tic()
test_Dec_Start <- LEMMAABMv4::covid_abm_v4(bta_base    = x, 
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

saveRDS(test_Dec_Start, "data", "outputs", "test_Dec_start.rds")