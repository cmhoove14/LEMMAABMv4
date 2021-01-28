library(tidyverse)
library(data.table)
library(matrixStats)

sfgrph_files <- list.files(here::here("data", "processed", "Safegraph"))
sf_visitors_file <- sfgrph_files[grepl("SF_Visitors2021", sfgrph_files)]

sf_visitors <- readRDS(here::here("data", "processed", "Safegraph", sf_visitors_file))

# Get county fips codes ~~~~~~~~~~~~~~~~~~~~~~~~~---------------------------
temp.file <- paste(tempfile(),".xlsx",sep = "")
download.file("https://www2.census.gov/programs-surveys/popest/geographies/2018/all-geocodes-v2018.xlsx", 
              temp.file, mode = "wb")

fips <- readxl::read_excel(temp.file, skip = 4)
  ca_cnty_fips <- fips %>% 
    filter(`State Code (FIPS)` == "06" &
             `County Code (FIPS)` != "000") %>% 
    mutate(County = `Area Name (including legal/statistical area description)`,
           fips = paste0(`State Code (FIPS)`, `County Code (FIPS)`)) %>% 
    dplyr::select(County, fips)
  ca_cnty_fips_dt <- as.data.table(ca_cnty_fips)

# Get county populations ~~~~~~~~~~~~~~~~~~~~~~~~~~~----------------------  
temp.file2 <- paste(tempfile(),".xlsx", sep = "")
download.file("https://www2.census.gov/programs-surveys/popest/tables/2010-2019/counties/totals/co-est2019-annres-06.xlsx", 
              temp.file2, mode = "wb")

pops <- readxl::read_excel(temp.file2, skip = 3)
  pops <- pops[2:59,c(1, ncol(pops))]
  colnames(pops) <- c("County", "pops")
  
  pops$County <- substr(pops$County, 2, nchar(pops$County)-(nchar(",California")+1))
  
  pops$County %in% ca_cnty_fips_dt$County
  
  ca_cnty_fips_dt <- merge(ca_cnty_fips_dt, pops, by = "County")
  
# Get case data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~---------------------------
  source(here::here("data", "get", "COVID_CA_get_latest.R"))
  
  CA_cases_dt <- as.data.table(CA_cases)
  CA_cases_dt[, County := paste0(county, " County")]

  CA_cases_dt2 <- merge(CA_cases_dt, ca_cnty_fips_dt, by = "County")
    CA_cases_dt2$date <- as.Date(CA_cases_dt2$date)
  
# Process visitors file  
t0 <- as.Date("2019-12-31")  
  
sf_tot_visitors <- lapply(1:length(sf_visitors), function(t){
  dt <- melt(sf_visitors[[t]], "visits")
  dt[, ct:=substr(visits,1,11)]
  dt[, visits:=NULL]
    colnames(dt)[1:2] <- c("fips", "Visits")
  dt_ct <- dt[, Visits:=sum(Visits), by = list(fips, ct)]  
    
  dt_ct[, date:=t0+t]  
  dt_ct[, fips:=as.character(fips)]  
  
  dt2 <- merge(dt_ct, CA_cases_dt2, by = c("fips", "date"),
               all.x = TRUE)
  
  dt2[, inf_prob:=(newcountconfirmed/pops)]

  return(dt2[, c("fips", "County", "totalcountconfirmed", "totalcountdeaths", "newcountdeaths"):=NULL])  
})

saveRDS(sf_tot_visitors, here::here("data", "processed", "Safegraph", "SF_visitors_CTs_2021Processed.rds"))
