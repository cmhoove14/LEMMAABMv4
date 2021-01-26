library(parallel)
library(matrixStats)

# Read in data
agents <- readRDS(here::here("data", "sf_synthetic_agents_cbg_dt.rds"))
cbg_mv <- readRDS(here::here("data","SFCensBlockGroupsMvmt2020-01-01to2020-11-09.rds"))

n_days <- dim(cbg_mv)[3]

cbg_cdf <- cbg_mv
cbg_indices <- cbg_mv

# Function to convert device matrix to probability matrix
for(d in 1:n_days){
  cbg_mat <- cbg_mv[,,d]
  
  n_comm <- nrow(cbg_mat)
  
  cbg_devices <- matrixStats::rowSums2(cbg_mat)
  no_devices <- which(cbg_devices == 0)
  cbg_norm <- do.call(rbind, lapply(1:n_comm, function(r){
    cbg_mat[r,]/cbg_devices[r]
  }))
  
  diag(cbg_norm)[which(cbg_devices==0)] <- 1
  cbg_norm[is.nan(cbg_norm)] <- 0

  nbhd_mat_cdf <- matrix(0, nrow = n_comm, ncol = n_comm)
  nbhd_mat_index <- matrix(NA_integer_, nrow = n_comm, ncol = n_comm)
  
  for (i in 1:n_comm) {
    # Create cdf for row i
    nbhd_mat_cdf[i,] <- sapply(1:n_comm, function(j) sum(cbg_norm[i,1:j]))
    
    # Get indices of row
    indices <- which(cbg_mat[i,] > 0)
    nbhd_mat_index[i,] <- sapply(1:n_comm, function(j) max(indices[j>=indices]))
    
  }
  
  cbg_cdf[,,d] <- nbhd_mat_cdf
  cbg_indices[,,d] <- nbhd_mat_index
  # Replace columns with no devices recorded with same cbg indicating residents stay in their cbg
  cbg_indices[no_devices,,d] <- do.call(rbind, lapply(no_devices, function(i) rep(i, n_comm)))
  print(d)
  
}  

  saveRDS(cbg_cdf, here::here("data", "safegraph_cbg_mvmt_cdf_processed_2020-09-11.rds"))
  saveRDS(cbg_indices, here::here("data", "safegraph_cbg_mvmt_ind_processed_2020-09-11.rds"))
    
# Convert to list of matrices instead https://stackoverflow.com/questions/64039381/fast-random-sampling-from-matrix-of-cumulative-probability-mass-functions-in-r
cbg_cdf_ls <- list()
  
for(d in 1:n_days){
  cbg_mat <- cbg_mv[,,d]
  
  n_comm <- nrow(cbg_mat)
  
  cbg_devices <- matrixStats::rowSums2(cbg_mat)
  no_devices <- which(cbg_devices == 0)
  cbg_norm <- do.call(rbind, lapply(1:n_comm, function(r){
    cbg_mat[r,]/cbg_devices[r]
  }))
  
  diag(cbg_norm)[which(cbg_devices==0)] <- 1
  cbg_norm[is.nan(cbg_norm)] <- 0
  
  nbhd_mat_cdf <- matrix(0, nrow = n_comm, ncol = n_comm)
    
  for (i in 1:n_comm) {
    # Create cdf for row i
    nbhd_mat_cdf[i,] <- sapply(1:n_comm, function(j) sum(cbg_norm[i,1:j]))
  }
  
  cbg_cdf_ls[[d]] <- nbhd_mat_cdf
  
  print(d)
  
}    
  
  saveRDS(cbg_cdf_ls, here::here("data", "safegraph_cbg_mvmt_cdf_list_processed_2020-11-13.rds"))

# Get monthly and day of the week averages for use in projections since don't have observations beyond observation day
  t0 <- as.Date("2020-01-01")
  
  for(d in 1:n_days){
  cbg_mat <- cbg_mv[,,d]
  
  n_comm <- nrow(cbg_mat)
  
  cbg_devices <- matrixStats::rowSums2(cbg_mat)
  no_devices <- which(cbg_devices == 0)
  cbg_norm <- do.call(rbind, lapply(1:n_comm, function(r){
    cbg_mat[r,]/cbg_devices[r]
  }))
  
  diag(cbg_norm)[which(cbg_devices==0)] <- 1
  cbg_norm[is.nan(cbg_norm)] <- 0

  nbhd_mat_cdf <- matrix(0, nrow = n_comm, ncol = n_comm)
  nbhd_mat_index <- matrix(NA_integer_, nrow = n_comm, ncol = n_comm)
  
  for (i in 1:n_comm) {
    # Create cdf for row i
    nbhd_mat_cdf[i,] <- sapply(1:n_comm, function(j) sum(cbg_norm[i,1:j]))
    
    # Get indices of row
    indices <- which(cbg_mat[i,] > 0)
    nbhd_mat_index[i,] <- sapply(1:n_comm, function(j) max(indices[j>=indices]))
    
  }
  
  cbg_cdf[,,d] <- nbhd_mat_cdf
  cbg_indices[,,d] <- nbhd_mat_index
  # Replace columns with no devices recorded with same cbg indicating residents stay in their cbg
  cbg_indices[no_devices,,d] <- do.call(rbind, lapply(no_devices, function(i) rep(i, n_comm)))
  print(d)
  
}  
  date <- t0 + d

    
  