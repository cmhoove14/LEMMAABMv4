# -------------------
# Process synthetic agents database for use in model 
# Chris Hoover Jan 2021
# -------------------
library(tidyverse)
library(lubridate)
library(fastmatch)
library(LEMMAABMv4)

agents <- read_csv(here("data", "raw", "total_sf_pop.csv"))

### Household size (house_id)
# variable house_id is not globally unique, only within census tracts, so need to create new identifier
agents$ct_hh_id <- paste0(agents$geoid, "_", agents$house_id)  
agents$hhid     <- as.numeric(factor(agents$ct_hh_id))
agents$house_id <- agents$ct_hh_id <- NULL # remove superfluous columns

# Check household sizes by race
#check_hh_size <- agents %>% group_by(hhid) %>% summarise(n = n(), race = first(race)) ; table(check_hh_size$n, check_hh_size$race)

### Individual id (indiv_id)
# variable indiv_id is not globally unique, only within census tracts, so need to create new identifier
agents$ct_indiv_id <- paste0(agents$geoid, "_", agents$indiv_id)  
agents$id          <- as.numeric(factor(agents$ct_indiv_id))
agents$indiv_id    <- agents$ct_indiv_id <- NULL # remove superfluous columns

length(unique(agents$id)) == nrow(agents) # Check everyone has unique id


# Agent characteristics
### Sex
# 1. Male
# 2. Female

### Age
# age group     recode
# 1 <5          1
# 2 5-9         1      
# 3 10-14       2
# 4 15-17       2
# 5 18-19       2
# 6 20          3
# 7 21          3
# 8 22-24       3
# 9 25-29       3
# 10 30-34      4
# 11 35-39      4
# 12 40-44      5
# 13 45-49      5
# 14 50-54      6
# 15 55-59      6
# 16 60-61      7
# 17 62-64      7
# 18 65-66      7
# 19 67-69      7
# 20 70-74      8
# 21 75-79      8
# 22 80-84      9
# 23 85+        9

# Convert age code to age number to coincide with functions that provide epi transition probabilities by age
agents$age_code <- agents$age
agents$age[agents$age_code == 1] <- 5
agents$age[agents$age_code == 2] <- 15
agents$age[agents$age_code == 3] <- 25
agents$age[agents$age_code == 4] <- 35
agents$age[agents$age_code == 5] <- 45
agents$age[agents$age_code == 6] <- 55
agents$age[agents$age_code == 7] <- 65
agents$age[agents$age_code == 8] <- 75
agents$age[agents$age_code == 9] <- 85

  agents$age_code <- NULL
  
### Race
# 1. White alone
# 2. .Black or African American alone
# 3. American Indian alone
# 3. Alaska Native alone
# 3. American Indian and Alaska Native tribes specified; or American Indian or Alaska Native, not specified and no other races
# 4. Asian alone
# 5. Native Hawaiian and Other Pacific Islander alone
# 6. Some Other Race alone
# 7. Two or More Races
# 8. Hispanic/Latinx

### Household income groups
  # 1 <50k
  # 2 50-100k
  # 3 >100k
  
### Geoid is the census tract of residents
  # Convert to numeric (removes leading 0) for faster matching and processing
  agents$ct <- as.numeric(agents$geoid)
  agents$geoid <- NULL
  
### Occupation
# 0 None
# 1	Management occupations
# 2	Business and financial operations occupations
# 3	Computer and mathematical occupations
# 4	Architecture and engineering occupations
# 5	Life, physical, and social science occupations
# 6	Community and social service occupations
# 7	Legal occupations
# 8	Educational instruction, and library occupations
# 9	Arts, design, entertainment, sports, and media occupations
# 10	Health diagnosing and treating practitioners and other technical occupations
# 10	Health technologists and technicians
# 11	Healthcare support occupations
# 12	Firefighting and prevention, and other protective service workers including supervisors
# 12	Law enforcement workers including supervisors
# 13	Food preparation and serving related occupations
# 14	Building and grounds cleaning and maintenance occupations
# 15	Personal care and service occupations
# 16	Sales and related occupations
# 17	Office and administrative support occupations
# 18	Farming, fishing, and forestry occupations
# 19	Construction and extraction occupations
# 20	Installation, maintenance, and repair occupations
# 21	Production occupations
# 22	Transportation occupations
# 23	Material moving occupations

# Numeric codes for essential occupations
essentials <- c(6,12:14,18:23) # Not including healthcare workers (10/11) assuming access to PPE etc. Not ideal, but different kind of risk than what we intend to capture with essential worker label
agents$essential <- 0
agents$essential[agents$occp %in% essentials] <- 1
sum(agents$essential)/nrow(agents) # Results in ~17% essential workers

# Check essential worker status by race. 
# agents %>% group_by(race) %>% summarise(n=n(), essentials = sum(essential), prop_essential=essentials/n)
# Latinx has highest rate, checks out  

# Add unique workplaces
# Data for between ct movement  
  # List of matrices representing CDFs of between CT movement
  sf_ct_cdf_ls <- readRDS(here::here("data", "processed", "Safegraph", "safegraph_ct_mvmt_cdf_list_2020processed.rds"))
  # CT Ids corresponding to column/row indices in above matrices
  sf_ct_ids <- read_csv(here::here("data", "raw", "Census_2010_Tracts.csv")) %>% pull(GEOID10) %>% as.numeric()

# First get average weekday probability of being in CT j from CT i
  dates <- seq.Date(as.Date("2020-01-07"), as.Date("2020-02-17"), b = "day")
  dates_days <- wday(dates)
  dates_use <- dates[which(dates_days %in% c(2:6))]
  indices_use <- as.numeric(dates_use - as.Date("2019-12-31"))
  
  sf_cdf_use <- sf_ct_cdf_ls[indices_use]
  
  sf_cdf_array <- do.call(cbind, sf_cdf_use)
  sf_cdf_array <- array(sf_cdf_array, dim=c(dim(sf_cdf_use[[1]]), length(sf_cdf_use)))
  
  sf_cdf_mean <- apply(sf_cdf_array, c(1, 2), mean, na.rm = TRUE)
  
# Next assign work CT to every agent with occp != 0
  set.seed(430)
  agent_cts <- agents$ct
  work_rn <- dqrunif(nrow(agents))
  ct_indices <- fmatch(agent_cts, sf_ct_ids)   # Match ct codes to matrix index
  mat_cdf_expand <- sf_cdf_mean[ct_indices,]   # Expand CDF matrix corresponding to residence matrix
  work_cts <- sf_ct_ids[max.col(work_rn < mat_cdf_expand, "first")] # Probabilistically assign work cts from random numbers and cdf
  
  workers <- which(agents$occp != 0) # Agents who are workers

  # Assign work CTs
  agents$work_ct <- NA 
  agents$work_ct[workers] <- work_cts[workers]
    
# Next assign workplaces (maybe better to think of them as "offices", closer contacts presumed to be e.g. coworkers) by occupation and ct
  agents$work_ct_occp <- paste0(agents$work_ct, "_", agents$occp)
  
  # hist(agents %>% filter(occp!=0) %>% group_by(work_ct_occp) %>% summarise(n_workers = n()) %>% pull(n_workers), breaks = 30)
  # View(agents %>% filter(occp!=0) %>% group_by(work_ct_occp) %>% summarise(n_workers = n()))

# Workplace size taken as poisson distributed with rate 10  
  worksize_rate <- 10
  agents$work <- NA
  ct_occps <- unique(agents$work_ct_occp[which(agents$occp != 0)])

  
# First assign workplaces where there are 5 or fewer workers of the same occupation in the census tract
  for(i in 1:length(ct_occps)){
    n_workers <- sum(agents$work_ct_occp == ct_occps[i]) # Workers in this sector
    
  # Get offices to assign workers to  
    office_sizes <- rpois(1, worksize_rate)
    while(sum(office_sizes) < n_workers){
      office_sizes <- c(office_sizes, rpois(1, worksize_rate))
    }
    
    # Generates unique office identifiers 
    offices <- paste0(ct_occps[i], "_", unlist(sapply(1:length(office_sizes), 
                                                      function(x) rep(x, office_sizes[x]))))
    # Trims office identifiers to be same length as n_workers
    offices <- sample(offices, n_workers)
    
  # Assign workers to offices
    agents$work[which(agents$work_ct_occp == ct_occps[i])] <- offices
    
  cat(i, " ")  
  }
  
  sum(is.na(agents$work[which(agents$occp != 0)])) # Should be 0
  
  #Convert to numeric work id for faster computations/matching
    agents$work_fac <- as.numeric(factor(agents$work))
      length(unique(agents$work)) == length(unique(agents$work_fac)) # Check nothing got lost
      
  # Convert to numeric      
    work_id <- 5000000  # Max hhid is 387386, ct ids start with 6 and are 10 digits long, so start work ids at 5000000 (starts with 5, 7 digits long)
  
    agents$work <- agents$work_fac + work_id
    
  agents$work_ct_occp <- agents$work_fac <- NULL
  
#Convert to data.table  
agents <- data.table(agents)
  setkey(agents, id, hhid)
  
saveRDS(agents, here("data", "processed", "SF_agents_processed.rds"))
