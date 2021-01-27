# ---------------------
# Prep data inputs for LEMMAABM
# Chris Hoover Jan 2021
# ---------------------

library(tidyverse)

# synthetic agents from Census/IPUMS data ------------
agents <- readRDS(here::here("data", "processed", "SF_agents_processed.rds"))
  data.table::setkey(agents, id, hhid)

# Add column for use in model
agents[, state := "S"]
agents[, nextstate := NA]
agents[, tnext := 0]
agents[, t_symptoms := 0]


# UNCOMMENT BELOW/change size of sample TO Subset for development for faster runs/lower memory
# agents <- agents[agents$residence %in% sample(agents$residence, 2e4, replace = F)]  

N <- nrow(agents)

#Plot tests through time
#ggplot(data = sf_test) + geom_line(aes(x = Date, y = (tests/9e5)*1e5)) + theme_bw() +scale_x_date(date_breaks = "14 day") +theme(axis.text.x = element_text(angle = 45,hjust = 1))+labs(x="",y="SF Tests per 100k")

# Testing data for sims ----------------------
source(here::here("data", "get","COVID_CA_get_latest.R"))


# sf_test is observed testing completed in SF county
# Must contain columns date_num and tests_pp to convert to testing function in model
sf_test_smooth <- sf_test %>% 
  mutate(Date = as.Date(substr(specimen_collection_date, 1,10)),
         date_num = as.numeric(Date-as.Date("2019-12-31")),
         tests_pp = tests/N) %>% 
  padr::pad() %>% 
  padr::fill_by_value(tests_pp, value = 0) %>% 
  mutate(tests_pp_7day_avg = zoo::rollmean(tests_pp, 7, na.pad = T, align = "center"),
         tests_pp = case_when(is.na(tests_pp_7day_avg) & Date < as.Date("2020-03-10") ~ 2/N,
                              is.na(tests_pp_7day_avg) & Date > as.Date("2020-03-10") ~ 5000/N,
                              !is.na(tests_pp_7day_avg) ~ tests_pp_7day_avg)) 

last_sf_test <- max(sf_test_smooth$Date)

# sf_test_smooth %>%  ggplot() + geom_line(aes(x = Date, y = tests_pp)) + geom_line(aes(x = Date, y = tests_pp_7day_avg), col = "red") + theme_classic()


# Safegraph data -------------------------
#San Francisco ct mvmt list derived from sfgrph data
sf_ct_cdf_ls <- readRDS(here::here("data", "processed", "Safegraph", "safegraph_ct_mvmt_cdf_list_2020processed.rds"))
sf_ct_ids <- read_csv(here::here("data", "raw", "Census_2010_Tracts.csv")) %>% pull(GEOID10) %>% as.numeric()

#San francisco stay at home by percent by cbg derived from safegraph
sf_sfgrph_pcts <- readRDS(here::here("data", "processed","Safegraph", "sfgrph_devices_pct_home_cts_2020.rds"))

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
sf_visitors <- readRDS(here::here("data", "processed", "Safegraph", "SF_visitors_CTs_2020Processed.rds"))

# Vaccination data ------------------
# Data here but not doanloadable yet https://data.sfgov.org/stories/s/a49y-jeyc
# Hack to get approximation of vaccines per day so far
vax_start <- as.Date("2020-12-15")
vax_per_day <- data.frame(days <- as.numeric(vax_start-as.Date("2019-12-31")+c(0,10,20,30,40)),
                          vax <- c(1, 800, 800, 2200, 2200))

# Final object to export
data_inputs                 <- list()
data_inputs$agents          <- agents
data_inputs$ct_cdf_list     <- sf_ct_cdf_ls
data_inputs$ct_ids          <- sf_ct_ids
data_inputs$stay_home_dt    <- sf_sfgrph_pct_home
data_inputs$visitors_list   <- sf_visitors
data_inputs$tests_avail     <- sf_test_smooth
data_inputs$vax_per_day     <- vax_per_day

saveRDS(data_inputs, here::here("data", "processed", "data_inputs.rds"))
