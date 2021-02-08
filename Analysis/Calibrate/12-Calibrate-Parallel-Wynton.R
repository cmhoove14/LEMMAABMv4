
data.table::setDTthreads(1)

# Get options passed from BASH -----------------
opts <- commandArgs(TRUE)

lhs_start        <- as.numeric(opts[1])
lhs_end          <- as.numeric(opts[2])
data_inputs_path <- as.character(opts[3])
input_pars_path  <- as.character(opts[4])
vax_phases_path  <- as.character(opts[5])
output_path      <- as.character(opts[6]) 

if(length(opts) > 6){
  visitors    <- as.logical(opts[7]) 
  testing     <- opts[8]  
  vaccination <- as.logical(opts[9])
  verbose     <- as.logical(opts[10])  
  store_extra <- as.logical(opts[11])  
} else {
  visitors    <- TRUE 
  testing     <- "S" 
  vaccination <- FALSE 
  verbose     <- FALSE 
  store_extra <- TRUE 
}

cat("\n", opts, "\n")


# Load data files from bash paths ---------------------
input_pars  <- readRDS(here::here(input_pars_path))
data_inputs <- readRDS(here::here(data_inputs_path))
vax_phases  <- readRDS(here::here(vax_phases_path))

# Replace pars in list with pars from lhs -------------------
lhs <- readRDS("data/processed/Calibration_LHS.rds")

# Setup, export everything to cluster, and run in parallel ------------------
#Setup for running jobs across parallel nodes in cluster
n_cores <- future::availableCores()

clooster <- parallel::makeCluster(n_cores)

parallel::clusterEvalQ(cl = clooster,
                       expr = lapply(c("data.table", "wrswoR", "dqrng", "matrixStats", "fastmatch", "lubridate", "tidyverse", "LEMMAABMv4"), 
                                     library,
                                     character.only = TRUE))

parallel::clusterExport(cl = clooster, 
                        c("lhs", "lhs_start", "lhs_end", "data_inputs", "input_pars", "vax_phases", "output_path",
                          "visitors", "testing", "vaccination", "verbose", "store_extra"))

parallel::parLapply(cl = clooster,
                    X=c(lhs_start:lhs_end), 
                    fun = function(x){
                      
                      input_pars$trans_pars$bta_base             <- lhs[x,1]
                      input_pars$trans_pars$bta_hh               <- lhs[x,2]
                      input_pars$trans_pars$bta_work             <- lhs[x,3]
                      input_pars$trans_pars$bta_sip_rd           <- lhs[x,4]
                      input_pars$trans_pars$hpi_bta_mult         <- lhs[x,5]
                        
                      input_pars$quar_pars$q_prob_contact        <- lhs[x,6]
                      input_pars$quar_pars$q_prob_resinf         <- lhs[x,7]
                      input_pars$quar_pars$q_prob_symptoms       <- lhs[x,8]
                      input_pars$quar_pars$q_prob_testpos        <- lhs[x,9]
                      input_pars$quar_pars$q_prob_essential      <- lhs[x,10]  
                      input_pars$quar_pars$q_bta_red_exp         <- lhs[x,11]  
                      input_pars$quar_pars$known_contact_prob    <- lhs[x,12]
                      input_pars$quar_pars$q_dur_mean            <- lhs[x,13]
                      
                      input_pars$test_pars$hpi_mult              <- lhs[x,14]
                      input_pars$test_pars$income_mult           <- lhs[x,15]
                      input_pars$test_pars$case_finding_mult     <- lhs[x,16]
                      input_pars$test_pars$cont_mult             <- lhs[x,17]
                      input_pars$test_pars$symp_mult             <- lhs[x,18]
                      input_pars$test_pars$res_mult              <- lhs[x,19]
                      input_pars$test_pars$symp_state_mult       <- lhs[x,20]
                      input_pars$test_pars$hosp_mult             <- lhs[x,21]
                      input_pars$test_pars$essential_prob        <- lhs[x,22]
                      
                      input_pars$other_pars$mask_red             <- lhs[x,23]
                      input_pars$other_pars$visitor_mult_testing <- lhs[x,24]
                      input_pars$other_pars$visitor_mult_sfgrph  <- lhs[x,25]
                      input_pars$other_pars$mort_mult            <- lhs[x,26]
                      
                      input_pars$init_states$E0                  <- lhs[x,27]

                      LEMMAABMv4::covid_abm_v4(data_inputs = data_inputs, 
                                               input_pars  = input_pars, 
                                               vax_phases  = vax_phases,
                                               visitors    = visitors, 
                                               testing     = testing, 
                                               vaccination = vaccination,
                                               verbose     = verbose, 
                                               store_extra = store_extra,
                                               output_path = paste0(output_path,x,"/"))
                    })

parallel::stopCluster(clooster)
