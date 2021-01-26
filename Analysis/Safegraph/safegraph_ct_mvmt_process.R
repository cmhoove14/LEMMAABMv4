library(parallel)
library(matrixStats)

# Read in data
ct_mv <- readRDS(here::here("data","processed","Safegraph","SFCensTractsMvmt2020-01-01to2020-12-31.rds"))

n_days <- dim(ct_mv)[3]

ct_cdf <- ct_mv
ct_indices <- ct_mv

# Convert to list of matrices instead https://stackoverflow.com/questions/64039381/fast-random-sampling-from-matrix-of-cumulative-probability-mass-functions-in-r
ct_cdf_ls <- list()
  
for(d in 1:n_days){
  ct_mat <- ct_mv[,,d]
  
  n_comm <- nrow(ct_mat)
  
  ct_devices <- matrixStats::rowSums2(ct_mat)
  no_devices <- which(ct_devices == 0)
  ct_norm <- do.call(rbind, lapply(1:n_comm, function(r){
    ct_mat[r,]/ct_devices[r]
  }))
  
  diag(ct_norm)[which(ct_devices==0)] <- 1
  ct_norm[is.nan(ct_norm)] <- 0
  
  nbhd_mat_cdf <- matrix(0, nrow = n_comm, ncol = n_comm)
    
  for (i in 1:n_comm) {
    # Create cdf for row i
    nbhd_mat_cdf[i,] <- sapply(1:n_comm, function(j) sum(ct_norm[i,1:j]))
  }
  
  ct_cdf_ls[[d]] <- nbhd_mat_cdf
  
  print(d)
  
}    
  
  saveRDS(ct_cdf_ls, here::here("data", "processed", "Safegraph", "safegraph_ct_mvmt_cdf_list_2020rocessed.rds"))

