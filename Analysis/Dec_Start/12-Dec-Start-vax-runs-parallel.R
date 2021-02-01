library(LEMMAABMv4)

data.table::setDTthreads(1)

# Get options passed from BASH -----------------
opts <- commandArgs(TRUE)

# Assign variables from BASH
bta_base    <- as.numeric(opts[1])
bta_hh      <- as.numeric(opts[2])
bta_work    <- as.numeric(opts[3])
bta_sip_red <- as.numeric(opts[4])

#Max number of times to run simulation in sequence (e.g. one core will run this many jobs)
max_iters <- as.numeric(opts[5])

data_inputs_path <- as.character(opts[6])
input_pars_path  <- as.character(opts[7])
vax_phases_path  <- as.character(opts[8])

if(length(opts) > 8){
  visitors    <- as.logical(opts[9]) 
  testing     <- opts[10]  
  vaccination <- opts[11]
  verbose     <- as.logical(opts[12])  
  store_extra <- as.logical(opts[13])  
} else {
  visitors    <- TRUE 
  testing     <- "S" 
  vaccination <- "N" 
  verbose     <- FALSE 
  store_extra <- TRUE 
}

cat("\n",opts, visitors, testing, vaccination , verbose , store_extra ,"\n")


# Load data files from bash paths ---------------------
input_pars  <- readRDS(here::here(input_pars_path))
data_inputs <- readRDS(here::here(data_inputs_path))
vax_phases  <- readRDS(here::here(vax_phases_path))
  vax_filename <- strsplit((strsplit(vax_phases_path,  "_scenario.rds"))[[1]], 
                           "data/processed/")[[1]][2]

# Setup, export everything to cluster, and run in parallel ------------------
#Setup for running jobs across parallel nodes in cluster
nworkers <- 12

n_cores <- nworkers

n_jobs <- nworkers*max_iters

clooster <- parallel::makeCluster(n_cores)

clusterEvalQ(cl = clooster,
             expr = lapply(c("data.table", "wrswoR", "dqrng", "matrixStats", "fastmatch", "lubridate", "tidyverse", "LEMMAABMv4"), 
                           library,
                           character.only = TRUE))

parallel::clusterExport(cl = clooster, 
                        c("bta_base", "bta_hh", "bta_work", "bta_sip_red",
                          "data_inputs", "input_pars", "vax_phases", "n_jobs",
                          "visitors", "testing", "vaccination", "verbose", "store_extra"))

sim_results <- parallel::parLapply(cl = clooster,
                                   X=rep(bta_base,n_jobs), 
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
                                                              vaccination = vaccination,
                                                              verbose     = verbose, 
                                                              store_extra = store_extra)
                                   })

parallel::stopCluster(clooster)

saveRDS(sim_results, paste0(here::here("data", "outputs", 
                                       paste0("ABMv4_Dec_Start_", 
                                              "_bta", bta_base, 
                                              "_vax", vax_filename, vaccination, 
                                              "_", Sys.Date(),".rds"))))