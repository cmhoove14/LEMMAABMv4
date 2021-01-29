library(LEMMAABMv4)

# Get options passed from BASH -----------------
opts <- commandArgs(TRUE)

# Assign variables from BASH
bta_base    <- as.numeric(opts[1])
bta_hh      <- as.numeric(opts[2])
bta_work    <- as.numeric(opts[3])
bta_sip_red <- as.numeric(opts[4])

data_inputs_path <- as.character(opts[5])
input_pars_path  <- as.character(opts[6])
vax_phases_path  <- as.character(opts[7])

if(length(opts) > 7){
  visitors    <- as.logical(opts[8]) 
  testing     <- as.logical(opts[9])  
  adaptive    <- as.logical(opts[10])  
  vaccination <- as.logical(opts[11])  
  verbose     <- as.logical(opts[12])  
  store_extra <- as.logical(opts[13])  
} else {
  visitors    <- TRUE 
  testing     <- TRUE 
  adaptive    <- FALSE 
  vaccination <- FALSE 
  verbose     <- FALSE 
  store_extra <- TRUE 
}

# Load data files from bash paths ---------------------
input_pars  <- readRDS(here::here(input_pars_path))
data_inputs <- readRDS(here::here(data_inputs_path))
vax_phases  <- readRDS(here::here(vax_phases_path))

# Setup, export everything to cluster, and run in parallel ------------------
#Setup for running jobs across parallel nodes in cluster
n_cores <- parallel::detectCores()

n_sims_per_par <- 3

bta_sweeps <- rep(c(0.15, 0.175, 0.2, 0.225, 0.25, 0.275), each = n_sims_per_par)

clooster <- parallel::makeCluster(n_cores)

clusterEvalQ(cl = clooster,
             expr = lapply(c("data.table", "wrswoR", "dqrng", "matrixStats", "fastmatch", "lubridate", "tidyverse", "LEMMAABMv4"), 
                           library,
                           character.only = TRUE))

parallel::clusterExport(cl = clooster, 
                        c("bta_sweeps", "bta_hh", "bta_work", "bta_sip_red",
                          "data_inputs", "input_pars", "vax_phases",
                          "visitors", "testing", "adaptive", "vaccination", "verbose", "store_extra"))

sim_results <- parallel::parLapply(cl = clooster,
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

parallel::stopCluster(cl)

saveRDS(sim_results, paste0(here::here("data", "outputs", 
                                       paste0("ABMv4_bta_calibrate_n", n_sims_per_par,
                                              "_bta", min(bta_sweeps), "-", max(bta_sweeps), 
                                              "_", Sys.Date(),".rds"))))