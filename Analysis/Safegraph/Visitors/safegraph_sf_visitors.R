library(data.table)
library(readr)
library(parallel)
library(jsonlite)

# Get options passed from BASH -----------------
opts <- commandArgs(TRUE)

# Assign variables from BASH
year    <- as.character(opts[1])

# Get cbg ids and define a couple util functions (thanks, Josh)
sf_cbgs <- read_csv(here::here("Census_2010_CBGs_SF.csv"))
  sf_cbgs_dt <- as.data.table(sf_cbgs$GEOID10)
  colnames(sf_cbgs_dt) <- "visits"

ToCounty1 <- function(origin, visits) {
  visits_vec <- unlist(jsonlite::fromJSON(visits))
  data.table(origin, visits = names(visits_vec), num_visits = visits_vec)
}

ToCounty <- function(row, dt) {
  ToCounty1(dt[row, origin_census_block_group], dt[row, destination_cbgs])
}

# Functions returns number of devices in i with home in j on day t
SF_CBG_Visitors <- function(csv){
  sfgrph <- data.table(readr::read_csv(csv))
    sfgrph[, state_fips:=substr(origin_census_block_group, start = 1, stop = 2)]
    sfgrph[, cnty_fips:=substr(origin_census_block_group, start = 3, stop = 5)]
  
  # Restrict to mvmt within CA
    sfgrph_ca <- sfgrph[state_fips == "06"]

  dt <- rbindlist(lapply(1:nrow(sfgrph_ca), ToCounty, dt = sfgrph_ca))
    dt[, origin_county := substr(origin, 1, 5)]
    dt[, visits_county := substr(visits, 1, 5)]
  dt_sf <- dt[visits_county == "06075",]
  dt_sf_outsiders <- dt_sf[origin_county !="06075",]
  
  dt2 <- dt_sf_outsiders[, .(num_visits = sum(num_visits)), by = c("origin_county", "visits")]
  dt2 <- data.table::dcast(dt2, formula = "visits ~ origin_county", value.var = "num_visits")
  
  dt_fin <- merge.data.table(sf_cbgs_dt, dt2, by = "visits", all.x = T)
  
  return(dt_fin)

}

files <- list.files(year)
csvs <- files[grepl(".csv", files)]

dates <- as.Date(substr(csvs, 1, 10))

start <- min(dates)
end <- max(dates)

ndays <- as.integer(end-start)

cl <- makeCluster(detectCores())

  clusterExport(cl, c("start", "end", "dates", "csvs", "files", "ndays", "year",
                      "SF_CBG_Visitors", "ToCounty", "ToCounty1", "sf_cbgs_dt"))

  invisible(clusterEvalQ(cl, lapply(c("data.table", "readr", "jsonlite"), 
                                    library, character.only = T)))
  
  fill_list <- parLapplyLB(cl, 1:ndays,
                           function(t){
                             idate <- dates[t]
                             
                             ifile <- paste0(year, "/", files[grepl(idate, files)])
                             
                             if(length(ifile) == 1){
                               out <- SF_CBG_Visitors(ifile)
                             } else if(length(ifile) == 0){
                               out <- paste0("No file found for ", idate)
                             } else {
                               out <- paste0("More than one file found for ", idate)
                             }
                             
                           })
  
  parallel::stopCluster(cl)

saveRDS(fill_list, paste0("SF_Visitors", start, "to", end, ".rds"))
