# ---------------------
# Prep data inputs for LEMMAABM
# Chris Hoover Jan 2021
# ---------------------

library(tidyverse)

# Load par inputs to get timeframe ------------------
input_pars <- readRDS(here::here("data", "processed", "input_pars_debug.rds"))

t0 <- input_pars$time_pars$t0
t.end <- input_pars$time_pars$t.end
end.num <- as.numeric(t.end - as.Date("2019-12-31"))

# synthetic agents from Census/IPUMS data ------------
agents <- readRDS(here::here("data", "processed", "SF_agents_processed.rds"))
  data.table::setkey(agents, id, hhid)

# Add column for use in model
agents[, state := "S"]
agents[, nextstate := NA_character_]
agents[, tnext := 0]
agents[, t_symptoms := 0]


# UNCOMMENT BELOW/change size of sample TO Subset for development for faster runs/lower memory
# agents <- agents[agents$residence %in% sample(agents$residence, 2e4, replace = F)]  

N <- nrow(agents)

#Plot tests through time
#ggplot(data = sf_test) + geom_line(aes(x = Date, y = (tests/9e5)*1e5)) + theme_bw() +scale_x_date(date_breaks = "14 day") +theme(axis.text.x = element_text(angle = 45,hjust = 1))+labs(x="",y="SF Tests per 100k")

# Testing data for sims ----------------------
source(here::here("data", "get","COVID_CA_get_latest.R"))
  first_ca_case_reports <- as.numeric(as.Date(min(CA_cases$date)) - as.Date("2019-12-31"))

# sf_test is observed testing completed in SF county
# Must contain columns date_num and tests_pp to convert to testing function in model
sf_test_smooth <- sf_test %>% 
  mutate(Date = as.Date(substr(specimen_collection_date, 1,10)),
         tests_pp = tests/N) %>% 
  padr::pad() %>% 
  mutate(date_num = as.numeric(Date-as.Date("2019-12-31"))) %>% 
  padr::fill_by_value(tests_pp, value = 0) %>% 
  mutate(tests_pp_7day_avg = zoo::rollmean(tests_pp, 7, na.pad = T, align = "center"),
         tests_pp = case_when(is.na(tests_pp_7day_avg) & Date < as.Date("2020-03-10") ~ 2/N,
                              is.na(tests_pp_7day_avg) & Date > as.Date("2020-03-10") ~ 5000/N,
                              !is.na(tests_pp_7day_avg) ~ tests_pp_7day_avg)) 

last_sf_test <- max(sf_test_smooth$Date)

tests_avail <- sf_test_smooth %>% 
  dplyr::select(date_num, tests_pp)

# sf_test_smooth %>%  ggplot() + geom_line(aes(x = Date, y = tests_pp)) + geom_line(aes(x = Date, y = tests_pp_7day_avg), col = "red") + theme_classic()


# Safegraph data -------------------------
#San Francisco ct mvmt list derived from sfgrph data
sf_ct_cdf_ls20 <- readRDS(here::here("data", "processed", "Safegraph", "safegraph_ct_mvmt_cdf_list_2020processed.rds"))
sf_ct_cdf_ls21 <- readRDS(here::here("data", "processed", "Safegraph", "safegraph_ct_mvmt_cdf_list_2021processed.rds"))
sf_ct_cdf_ls <- c(sf_ct_cdf_ls20, sf_ct_cdf_ls21)
  
  sf_ct_cdf_ls <- sf_ct_cdf_ls[1:end.num] 

sf_ct_ids <- read_csv(here::here("data", "raw", "Census_2010_Tracts.csv")) %>% pull(GEOID10) %>% as.numeric()

#San francisco stay at home by percent by ct derived from safegraph
sf_sfgrph_pcts20 <- readRDS(here::here("data", "processed","Safegraph", "sfgrph_devices_pct_home_cts_2020.rds"))
sf_sfgrph_pcts21 <- readRDS(here::here("data", "processed","Safegraph", "sfgrph_devices_pct_home_cts_2021.rds"))

sf_sfgrph_pcts <- rbind(sf_sfgrph_pcts20, sf_sfgrph_pcts21)

  sf_sfgrph_pcts <- sf_sfgrph_pcts %>% filter(Date <= t.end)

last_sfgrph <- max(sf_sfgrph_pcts$Date)

# sf_sfgrph_pcts %>% 
#   group_by(Date) %>% 
#   summarise(n_devices     = sum(device_count), 
#             n_home        = sum(completely_home_device_count), 
#             n_part_work   = sum(part_time_work_behavior_devices), 
#             n_full_work   = sum(full_time_work_behavior_devices), 
#             per_home      = n_home/n_devices, 
#             per_part_work = n_part_work/n_devices, 
#             per_full_work = n_full_work/n_devices) %>% 
#   ggplot() +
#   geom_line(aes(x = Date, y = per_home), col = "green") +
#   geom_line(aes(x = Date, y = per_part_work), col = "orange") +
#   geom_line(aes(x = Date, y = per_full_work), col = "red") +
#   theme_bw() +
#   labs(y = "Pct device behavior",
#        title = "Safegraph home/work metrics SF County")

sf_sfgrph_pct_home <- sf_sfgrph_pcts %>% 
  dplyr::select(CT, Date, pct_home) %>% 
  data.table::as.data.table()

# Visitors to SF county from other CA counties ------------
sf_visitors20 <- readRDS(here::here("data", "processed", "Safegraph", "SF_visitors_CTs_2020Processed.rds"))
sf_visitors21 <- readRDS(here::here("data", "processed", "Safegraph", "SF_visitors_CTs_2021Processed.rds"))

sf_visitors <- c(sf_visitors20, sf_visitors21)

  sf_visitors <- sf_visitors[1:end.num]

# Vaccination data ------------------
# Data here but not downloadable yet https://data.sfgov.org/stories/s/a49y-jeyc
# Hack to get approximation of vaccines per day so far
vax_start <- as.Date("2020-12-10")
vax_last  <- t.end
  vax_days <- as.numeric(vax_start-as.Date("2019-12-31")):as.numeric(vax_last-as.Date("2019-12-31"))
  
nvax_last <- 3000
vax_max   <- 10000 # max vaccines per day to plateau at
  nvax <- rep(NA_real_, length(vax_days))
  nvax[1] <- 1
  nvax[length(vax_days)] <- nvax_last
  nvax <- round(zoo::na.approx(nvax))
  
vax_per_day <- data.frame(days = vax_days ,
                          vax  = nvax)

# Predict/impute into the future if simulation beyond present is desired
if(t.end > last_sf_test){
# Determine number of days necessary to "impute" then assign average of same number of days in the past into the future  
  n_add <- as.numeric(t.end - last_sf_test)
  lookback <- last_sf_test-n_add-7
  avg_past <- sf_test_smooth %>% filter(Date > lookback) %>% pull(tests_pp) %>% mean()
  tests_pp_pad <- c(sf_test_smooth$tests_pp_7day_avg, rep(avg_past, n_add))
  dates_pad <- c(sf_test_smooth$date_num, 
                 max(sf_test_smooth$date_num, na.rm = T)+c(1:n_add))
  
  tests_avail <- data.frame(date_num = dates_pad,
                            tests_pp = tests_pp_pad)
}

if(t.end > last_sfgrph){
  #Need the simulation to run beyond when we have safegraph movement data to inform movement, so repeat older safegraph data to project in the future 
  last_sf_day <- wday(last_sfgrph)
  n_add <- as.numeric(t.end - last_sfgrph)
  lookback <- last_sfgrph-n_add
  lookback_day <- ifelse(last_sf_day == 7, 1, last_sf_day+1)
  lookback_week <- lookback-c(1:7)
  lookback_start <- lookback_week[which(wday(lookback_week) == lookback_day)]
  
  sf_sfgrph_pct_home <- rbindlist(list(sf_sfgrph_pct_home,
                                       sf_sfgrph_pct_home %>% 
                                         filter(Date >= lookback_start) %>% 
                                         mutate(Date = Date + 1 + (last_sfgrph-lookback_start))))
  
  sf_cbg_cdf_ls <- c(sf_cbg_cdf_ls, 
                     sf_cbg_cdf_ls[(length(sf_cbg_cdf_ls)-as.numeric(last_sfgrph-lookback_start)):length(sf_cbg_cdf_ls)]) 
  
  sf_visitors <- c(sf_visitors,
                   sf_visitors[(length(sf_visitors)-as.numeric(last_sfgrph-lookback_start)):length(sf_visitors)])
  
  
}


# Final object to export
data_inputs                 <- list()
data_inputs$agents          <- agents
data_inputs$ct_cdf_list     <- sf_ct_cdf_ls
data_inputs$ct_ids          <- sf_ct_ids
data_inputs$stay_home_dt    <- sf_sfgrph_pct_home
data_inputs$visitors_list   <- sf_visitors
data_inputs$tests_avail     <- tests_avail
data_inputs$vax_per_day     <- vax_per_day

saveRDS(data_inputs, here::here("data", "processed", "data_inputs_debug.rds"))
