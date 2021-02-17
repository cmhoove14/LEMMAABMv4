library(data.table)
library(readr)
library(parallel)
library(dplyr)

# Get options passed from BASH -----------------
opts <- commandArgs(TRUE)

# Assign variables from BASH
year    <- as.character(opts[1])
dat_dir <- as.character(opts[2])

keep_cols <- c("origin_census_block_group", "device_count",
               "completely_home_device_count", "part_time_work_behavior_devices", "full_time_work_behavior_devices")

sumDevices <- function(csv, YEAR = year){
  sfgrph <- data.table(readr::read_csv(paste0(dat_dir, "/", YEAR, "/", csv)))
  sfgrph[, state_cnty_fips:=substr(origin_census_block_group, start = 1, stop = 5)]

  sfgrph_sf <- sfgrph[state_cnty_fips == "06075"]

  date <- as.Date(substr(csv, 1, 10))
  
  out <- sfgrph_sf[, ..keep_cols]
  
  out[, Date:=date]
  
  setcolorder(out, c(keep_cols[1],"Date", keep_cols[2:5]))
  
  return(as.data.frame(out))
}

files <- list.files(paste0(dat_dir,"/",year))
csvs <- files[grepl(".csv", files)]

n_cores <- detectCores()

cl <- makeCluster(n_cores)
clusterExport(cl, c("csvs", "sumDevices", "keep_cols", "year"))
invisible(clusterEvalQ(cl, lapply(c("data.table", "readr", "dplyr"), 
                        library, character.only = T)))


sfgrph_device_sums <- bind_rows(parLapply(cl, csvs, sumDevices, YEAR = year))

stopCluster(cl)

dates <- as.Date(substr(csvs, 1, 10))

start <- min(dates)
end <- max(dates)

saveRDS(sfgrph_device_sums, here::here("data/processed/Safegraph",
                                       paste0("SF_devices_sum", start, "to", end, ".rds")))
