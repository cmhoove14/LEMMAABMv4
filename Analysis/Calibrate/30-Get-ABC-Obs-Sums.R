# ---------------------------------------
# Process observed data into vector for use in ABC
# Chris Hoover Feb 2021
# ---------------------------------------
library(tidyverse)
library(data.table)


load(here::here("data","get","got","CA_SF_data2021-02-10.Rdata"))

# Process observed into vector to put into abc --------------------------
#Observed daily hospitalizations
hosp_obs <- sf_hosp %>% 
  filter(Date >= as.Date("2020-03-23") & Date <= as.Date("2020-12-01")) %>% 
  mutate(Hosp_point = round(HOSP_CONF + 0.3*(HOSP_PUI+ICU_PUI))) %>% 
  pull(Hosp_point)

# Observed weekly deaths
dths_dates <- sf_case %>% filter(Date <= as.Date("2020-12-01")) %>% dplyr::select(Date) # For merging with sims to get dates right

dths_obs <- sf_case %>% 
  filter(Date <= as.Date("2020-12-01")) %>% 
  dplyr::select(Date, Deaths) %>% 
  mutate(wod = paste0(lubridate::epiweek(Date), "_",
                      lubridate::year(Date))) %>% 
  group_by(wod) %>% 
  summarise(n_d_obs = sum(Deaths)) %>% 
  pull(n_d_obs)

#Observed deaths by race by Dec 1
dths_race_obs <- data.frame(race2 = c(1, 2, 8, 9),
                            deaths = c(55,9,44,78)) %>% pull(deaths)

# Observed cumulative Dec 1 cases by race
obs_case_race <- sf_case_race %>% 
  filter(Date == as.Date("2020-12-01")) %>% 
  dplyr::select(Race,Cum_Cases) %>% 
  rename("n_obs" = Cum_Cases) %>% 
  arrange(Race)

# Quite a few NAs, so allocate them in proportion to cases with known race
# This assumes there aren't systematic biases in reporting of race among known cases, which, probably not true, but best we can do
case_race_non_na <- obs_case_race$n_obs[which(!is.na(obs_case_race$Race))]
case_race_na     <- obs_case_race$n_obs[which(is.na(obs_case_race$Race))]
obs_total        <- sum(case_race_non_na)
obs_ratios       <- case_race_non_na / obs_total
obs_add          <- round(case_race_na*obs_ratios)

obs_case_race2 <- obs_case_race[!is.na(obs_case_race$Race),] 

case_race_obs <- obs_case_race2$n_obs + obs_add

# Combine in same vector
obs_sums <- c(hosp_obs, dths_obs, dths_race_obs, case_race_obs)

save(list = c("hosp_obs", "dths_obs", "dths_race_obs", "case_race_obs", "obs_sums"),
     file = here::here("data/processed/obs_summaries_for_ABC.Rdata"))