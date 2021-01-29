devtools::load_all() # library(LEMMAABMv4)
library(tictoc)

input_pars  <- readRDS(here::here("data/processed/input_pars.rds"))
data_inputs <- readRDS(here::here("data/processed/data_inputs.rds"))
vax_phases  <- readRDS(here::here("data/processed/vax65p_scenario.rds"))

#bta_base    <- 0.25
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

n_cores <- parallel::detectCores()-2

n_sims_per_par <- 4

bta_sweeps <- rep(c(0.15, 0.175, 0.2, 0.225, 0.25, 0.275), each = n_sims_per_par)

clooster <- parallel::makeCluster(n_cores)

clusterEvalQ(cl = clooster,
             expr = lapply(c("data.table", "wrswoR", "dqrng", "matrixStats", "fastmatch", "lubridate", "LEMMAABMv4"), 
                           library,
                           character.only = TRUE))

parallel::clusterExport(cl = clooster, 
                        c("bta_sweeps", "bta_hh", "bta_work", "bta_sip_red",
                          "data_inputs", "input_pars", "vax_phases",
                          "visitors", "testing", "adaptive", "vaccination", "verbose", "store_extra"))

sim_results <- parLapplyLB(cl = clooster,
                           X=bta_sweeps, 
                           fun = function(x){
                             LEMMAABMv4::covid_abm_v4(bta_base    = x, 
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
                           })

parallel::stopCluster(clooster)

toc()

saveRDS(sim_results, paste0(here::here("data", "outputs", 
                                       paste0("ABMv4_bta_calibrate", min(bta_sweeps), "-", max(bta_sweeps), "_", Sys.Date(),".rds"))))