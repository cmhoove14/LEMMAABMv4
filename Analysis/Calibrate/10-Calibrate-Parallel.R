library(LEMMAABMv4)

data.table::setDTthreads(1)

# Get options passed from BASH -----------------
opts <- commandArgs(TRUE)

# Assign variables from BASH
bta_base_lo    <- as.numeric(opts[1])
bta_base_hi    <- as.numeric(opts[2])
bta_hh      <- as.numeric(opts[3])
bta_work    <- as.numeric(opts[4])
bta_sip_red <- as.numeric(opts[5])

#Max number of times to run simulation in sequence (e.g. one core will run this many jobs)
max_iters <- as.numeric(opts[6])

data_inputs_path <- as.character(opts[7])
input_pars_path  <- as.character(opts[8])
vax_phases_path  <- as.character(opts[9])

if(length(opts) > 9){
  visitors    <- as.logical(opts[10]) 
  testing     <- as.logical(opts[11])  
  adaptive    <- as.logical(opts[12])  
  vaccination <- as.logical(opts[13])  
  verbose     <- as.logical(opts[14])  
  store_extra <- as.logical(opts[15])  
} else {
  visitors    <- TRUE 
  testing     <- TRUE 
  adaptive    <- FALSE 
  vaccination <- FALSE 
  verbose     <- FALSE 
  store_extra <- TRUE 
}

cat("\n",opts, visitors, testing, adaptive, vaccination , verbose , store_extra ,"\n")


# Load data files from bash paths ---------------------
input_pars  <- readRDS(here::here(input_pars_path))
data_inputs <- readRDS(here::here(data_inputs_path))
vax_phases  <- readRDS(here::here(vax_phases_path))

# Setup, export everything to cluster, and run in parallel ------------------
#Setup for running jobs across parallel nodes in cluster
RAM <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern = T))/1e6
nworkers <- floor(RAM/6)

n_cores <- nworkers

n_jobs <- nworkers*max_iters

n_sims_per_par <- 5

sweep_length <- floor(n_jobs/n_sims_per_par)

bta_sweeps <- rep(seq(bta_base_lo, bta_base_hi, length.out = sweep_length), each = n_sims_per_par)

cat(RAM, "gb RAM available\n", 
    nworkers,"workers deployed to simulate",n_sims_per_par,"simulations per parameter across", sweep_length,"parameter values\n")

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

parallel::stopCluster(clooster)

saveRDS(sim_results, paste0(here::here("data", "outputs", 
                                       paste0("ABMv4_bta_calibrate_n", n_sims_per_par,
                                              "_bta", bta_base_lo, "-", bta_base_hi, "_",
                                              "SiP_red", bta_sip_red,
                                              "_", Sys.Date(),".rds"))))