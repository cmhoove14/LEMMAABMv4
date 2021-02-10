# ---------------------------------------
# Compare simulation to observed data metrics
# Chris Hoover Feb 2021
# ---------------------------------------
library(tidyverse)
library(data.table)
library(zoo)

opts <- commandArgs(TRUE)

taskID <- as.numeric(opts[1])

root <- here::here("data","outputs","Calibration_Outputs")

# Get sim file  
sim_folder <- here::here(root, taskID)
sim <- readRDS(paste0(sim_folder,"/",list.files(sim_folder)[1]))

# Get observed
load(here::here("data","get","got","CA_SF_data2021-02-04.Rdata"))

# Compare daily hospitalizations ----------------
sim_hosp <- as_tibble(sim$epi_curve[state == "Ih",])
sim_hosp$Date <- as.Date(as.character(sim_hosp$date)) # Date formate from sim messed up because of sub-daily time step

comp_hosp <- merge(sim_hosp, sf_hosp, by = "Date")

hosp_mse <- sum(dpois(comp_hosp$N, comp_hosp$HOSP_CONF, log = T))

# Compare weekly deaths -------------------------
sim_dths <- as_tibble(sim$agents[state == "D", c("id", "sex", "age", "race", "t_death")]) %>% 
  mutate(
    tod = zoo::as.Date.numeric(t_death),
    wod =paste0(lubridate::epiweek(tod), "_",
                lubridate::year(tod))
  )

sim_dths_wk <- sim_dths %>% 
  group_by(wod) %>% 
  summarise(n_d_sim = n())

obs_dths_wk <- sf_case %>% 
  dplyr::select(Date, Deaths) %>% 
  mutate(wod = paste0(lubridate::epiweek(Date), "_",
                      lubridate::year(Date))) %>% 
  group_by(wod) %>% 
  summarise(n_d_obs = sum(Deaths))

comp_dths <- merge(sim_dths_wk, obs_dths_wk, by = "wod")

dths_nll <- sum(dpois(comp_dths$n_d_sim, comp_dths$n_d_obs, log = T))

# Compare cumulative Dec 1 deaths by race ---------------------
# State database only has non-hispanic white, non-hispanic black, hispanic, and other, so condense to match
sim_dths_race <- sim_dths %>% 
  mutate(race2 = if_else(race %in% c(1,2,8), race, 9)) %>% 
  group_by(race2) %>% 
  summarise(n_dths = n())

obs_dths_race <- data.frame(race2 = c(1, 2, 8, 9),
                            deaths = c(55,9,44,78))

comp_dths_race <- merge(sim_dths_race, obs_dths_race, by = "race2")

dths_race_nll <- sum(dpois(comp_dths_race$n_dths, comp_dths_race$deaths, log = T))

# Compare Monthly census tract confirmed cases -------------------
sim_cases <- sim$linelist_tests %>% 
  dplyr::select(id,race,state,ct, Date,test_pos, t_til_test_note) %>% 
  filter(test_pos == 1) %>% 
  mutate(
    Report_Date = as.Date(Date + t_til_test_note),
    MO_YR = paste0(lubridate::month(Report_Date), "_", lubridate::year(Report_Date))
  ) %>% 
  group_by(ct, MO_YR) %>% 
  summarise(n_sim = sum(test_pos))

obs_cases <- sf_geo_null %>% 
  mutate(MO_YR = paste0(lubridate::month(Date), "_", lubridate::year(Date)),
         ct = as.numeric(id)) %>% 
  group_by(ct, MO_YR) %>% 
  summarise(n_obs = sum(as.numeric(new_confirmed_cases)))

# Get reference data frame for all potential ct-months of comparison
month_dates <- seq.Date(sim$input_pars$test_pars$test_start, sim$input_pars$time_pars$t.end, by = "month")
MO_YRs      <- paste0(lubridate::month(month_dates), "_", lubridate::year(month_dates))
all_cts     <- unique(sim$agents$ct)

ct_months <- as_tibble(expand.grid("ct" = all_cts, 
                                   "MO_YR" = MO_YRs))

comp_ct_cases <- ct_months %>% 
  left_join(sim_cases, by = c("ct", "MO_YR")) %>% 
  left_join(obs_cases, by = c("ct", "MO_YR")) %>% 
  replace_na(list(n_sim = 0, n_obs = 0)) # Assume no data means no cases

# Since this has lots of 0s, need to modify to prevent -Inf
ct_cases_nll_vec <- dpois(comp_ct_cases$n_sim, comp_ct_cases$n_obs, log = T)
  ct_cases_nll_vec[is.infinite(ct_cases_nll_vec)] <- comp_ct_cases$n_sim[is.infinite(ct_cases_nll_vec)]*-1

ct_cases_nll <- sum(ct_cases_nll_vec)

# Compare cumulative Dec 1 confirmed cases by race ----------------------
sim_case_race <- sim$linelist_tests %>% 
  filter(test_pos == 1 & Date <= as.Date("2020-12-01")) %>% 
  dplyr::select(-Date) %>% # Don't care about date since comparing cumulative counts
  group_by(race) %>% 
  summarise(n_sim = n())

obs_case_race <- sf_case_race %>% 
  filter(Date == as.Date("2020-12-01")) %>% 
  dplyr::select(Race,Cum_Cases) %>% 
  rename("n_obs" = Cum_Cases)

# Quite a few NAs, so allocate them in proportion to cases with known race
# This assumes there aren't systematic biases in reporting of race among known cases, which, probably not true, but best we can do
case_race_non_na <- obs_case_race$n_obs[which(!is.na(obs_case_race$Race))]
case_race_na     <- obs_case_race$n_obs[which(is.na(obs_case_race$Race))]
obs_total        <- sum(case_race_non_na)
obs_ratios       <- case_race_non_na / obs_total
obs_add          <- round(case_race_na*obs_ratios)

obs_case_race2 <- obs_case_race[!is.na(obs_case_race$Race),]
obs_case_race2$n_obs <- obs_case_race2$n_obs + obs_add

comp_case_race <- merge(sim_case_race, obs_case_race2, by.x = "race", by.y = "Race")

case_race_nll<- sum(dpois(comp_case_race$n_sim, comp_case_race$n_obs, log = T))

rm(sim)
gc()               

out_list <- list()

out_list$fits <- list("sim"           = taskID, 
                      "status"        = 1,
                      "hosp_mse"      = hosp_nll,
                      "dths_mse"      = dths_nll,
                      "dths_race_mse" = dths_race_nll,
                      "ct_cases_mse"  = ct_cases_nll,
                      "case_race_mse" = case_race_nll)

out_list$comp_dfs <- list("hosp"      = comp_hosp,
                          "dths"      = comp_dths,
                          "ct_cases"  = comp_ct_cases,
                          "dths_race" = comp_dths_race,
                          "case_race" = comp_case_race)

out_path <- here::here("data","outputs","Calibration_Fits",taskID)

if(dir.exists(out_path)){
  saveRDS(out_list, paste0(out_path, "/LHS_NLLs.rds"))
} else {
  dir.create(out_path)
  saveRDS(out_list, paste0(out_path, "/LHS_NLLs.rds"))
}

