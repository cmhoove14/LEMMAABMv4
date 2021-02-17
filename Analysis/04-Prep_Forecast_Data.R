# ---------------------
# Prep data inputs for LEMMAABM
# Chris Hoover Feb 2021
# ---------------------

library(tidyverse)
library(lubridate)
library(data.table)

t0 <- as.Date("2020-12-02")
ref_date <- t0-1
t.end <- as.Date("2021-04-10")
N <- 9e5 # Approx pop of SF 900,000 

# Testing data for sims ----------------------
# source(here::here("data", "get","COVID_CA_get_latest.R"))
load(here::here("data", "get", "got", "CA_SF_data2021-02-16.Rdata"))
# sf_test is observed testing completed in SF county
# Must contain columns date_num and tests_pp to convert to testing function in model
sf_test_smooth <- sf_test %>% 
  mutate(Date = as.Date(substr(specimen_collection_date, 1,10)),
         tests_pp = tests/N) %>% 
  padr::pad() %>% 
  mutate(date_num = as.numeric(Date-ref_date)) %>% 
  padr::fill_by_value(tests_pp, value = 0) %>% 
  mutate(tests_pp_7day_avg = zoo::rollmean(tests_pp, 7, na.pad = T, align = "center"),
         tests_pp = case_when(is.na(tests_pp_7day_avg) & Date < as.Date("2020-03-10") ~ 2/N,
                              is.na(tests_pp_7day_avg) & Date > as.Date("2020-03-10") ~ 5000/N,
                              !is.na(tests_pp_7day_avg) ~ tests_pp_7day_avg))

# Add testing data based on recent average
avg_past <- sf_test_smooth %>% filter(date_num > 0) %>% pull(tests_pp) %>% mean()

# Repeat the past then add obs onto the end for the future
n_add <- 70
tests_pp_pad <- c(sf_test_smooth$tests_pp_7day_avg[which(sf_test_smooth$date_num >0 & !is.na(sf_test_smooth$tests_pp_7day_avg))], 
                  rep(avg_past, n_add))
dates_pad <- c(sf_test_smooth$date_num[which(sf_test_smooth$date_num >0)], 
               max(sf_test_smooth$date_num, na.rm = T)+c(1:(n_add-3)))

tests_avail <- data.frame(date_num = dates_pad,
                          tests_pp = tests_pp_pad)

# sf_test_smooth %>%  ggplot() + geom_line(aes(x = Date, y = tests_pp)) + geom_line(aes(x = Date, y = tests_pp_7day_avg), col = "red") + theme_classic()


# Safegraph data -------------------------
#San francisco stay at home by percent by ct derived from safegraph
sf_sfgrph_pcts20 <- readRDS(here::here("data", "processed","Safegraph", "sfgrph_devices_pct_home_cts_2020.rds"))
sf_sfgrph_pcts21 <- readRDS(here::here("data", "processed","Safegraph", "sfgrph_devices_pct_home_cts_2021.rds"))

sf_sfgrph_pcts <- rbind(sf_sfgrph_pcts20, sf_sfgrph_pcts21)

sf_sfgrph_pct_home <- sf_sfgrph_pcts %>% 
  dplyr::select(CT, Date, pct_home) %>% 
  data.table::as.data.table()

# Add data for optimistic scenario
sf_sfgrph_pct_home_add_opt <- sf_sfgrph_pct_home %>% 
  filter(Date >= as.Date("2020-10-01") & Date <= as.Date("2020-12-01")) %>%
  group_by(CT) %>% 
  mutate(Date = max(sf_sfgrph_pct_home$Date) + row_number()) %>% 
  ungroup()

sf_sfgrph_pct_home_opt <- rbindlist(list(sf_sfgrph_pct_home,
                                         sf_sfgrph_pct_home_add_opt))


# Add data for pessimistic scenario
sf_sfgrph_pct_home_add_pes <- sf_sfgrph_pct_home %>% 
  filter(Date >= as.Date("2020-12-01") & Date <= as.Date("2021-02-01")) %>%
  group_by(CT) %>% 
  mutate(Date = max(sf_sfgrph_pct_home$Date) + row_number()) %>% 
  ungroup()

sf_sfgrph_pct_home_pes <- rbindlist(list(sf_sfgrph_pct_home,
                                         sf_sfgrph_pct_home_add_pes))




#San Francisco ct mvmt list derived from sfgrph data
sf_ct_cdf_ls20 <- readRDS(here::here("data", "processed", "Safegraph", "safegraph_ct_mvmt_cdf_list_2020processed.rds"))
sf_ct_cdf_ls21 <- readRDS(here::here("data", "processed", "Safegraph", "safegraph_ct_mvmt_cdf_list_2021processed.rds"))
sf_ct_cdf_ls <- c(sf_ct_cdf_ls20, sf_ct_cdf_ls21)

sf_ct_ids <- read_csv(here::here("data", "raw", "Census_2010_Tracts.csv")) %>% pull(GEOID10) %>% as.numeric()

# Add data for optimistic scenario
which_add_opt <- seq.Date(as.Date("2020-10-01"), as.Date("2020-12-01"), by = "day") - as.Date("2019-12-31")

sf_ct_cdf_ls_opt <- c(sf_ct_cdf_ls, sf_ct_cdf_ls[which_add_opt])

# Add data for pessimistic scenario
which_add_pes <- seq.Date(as.Date("2020-12-01"), as.Date("2021-02-01"), by = "day") - as.Date("2019-12-31")

sf_ct_cdf_ls_pes <- c(sf_ct_cdf_ls, sf_ct_cdf_ls[which_add_pes])

# Visitors to SF county from other CA counties ------------
sf_visitors20 <- readRDS(here::here("data", "processed", "Safegraph", "SF_visitors_CTs_2020Processed.rds"))
sf_visitors21 <- readRDS(here::here("data", "processed", "Safegraph", "SF_visitors_CTs_2021Processed.rds"))

sf_visitors <- c(sf_visitors20, sf_visitors21)
sf_visitors[(length(sf_visitors)-2):length(sf_visitors)] <- NULL


# Add for optimistic scenario (indices from above)
sf_visitors_opt <- c(sf_visitors, sf_visitors[which_add_opt])


# Add for pessimistic scenario (indices from above)
sf_visitors_pes <- c(sf_visitors, sf_visitors[which_add_pes])



# Final optimistic and pessimistic objects to export
data_inputs_opt                 <- list()
data_inputs_opt$ct_cdf_list     <- sf_ct_cdf_ls_opt
data_inputs_opt$ct_ids          <- sf_ct_ids
data_inputs_opt$stay_home_dt    <- sf_sfgrph_pct_home_opt
data_inputs_opt$visitors_list   <- sf_visitors_opt
data_inputs_opt$tests_avail     <- tests_avail

saveRDS(data_inputs_opt, here::here("data", "processed", "data_inputs_forecast_opt.rds"))

# Final pessimistic and pessimistic objects to export
data_inputs_pes                 <- list()
data_inputs_pes$ct_cdf_list     <- sf_ct_cdf_ls_pes
data_inputs_pes$ct_ids          <- sf_ct_ids
data_inputs_pes$stay_home_dt    <- sf_sfgrph_pct_home_pes
data_inputs_pes$visitors_list   <- sf_visitors_pes
data_inputs_pes$tests_avail     <- tests_avail

saveRDS(data_inputs_pes, here::here("data", "processed", "data_inputs_forecast_pes.rds"))











# Vaccination data ------------------
# Data here but not downloadable yet https://data.sfgov.org/stories/s/a49y-jeyc
# Hack to get approximation of vaccines per day so far
vax_start <- as.Date("2020-12-15")
vax_last_obs <- as.Date("2021-02-15")
vax_last  <- t.end
vax_days <- as.numeric(vax_start-ref_date):as.numeric(vax_last-ref_date)

nvax_last_obs <- 6000

# max vaccines per day assuming linear trend continues (optimistic)
nvax_trend <- nvax_last_obs/as.numeric(vax_last_obs-vax_start)
nvax_last   <- round(nvax_last_obs+nvax_trend*as.numeric(vax_last-vax_last_obs)) 
nvax <- rep(NA_real_, length(vax_days))
nvax[1] <- 1
nvax[length(vax_days)] <- nvax_last
nvax <- round(zoo::na.approx(nvax))

vax_per_day_opt <- data.frame(days = vax_days ,
                              vax  = nvax)

saveRDS(vax_per_day_opt, here::here("data", "processed", "vax_forecast_opt.rds"))



# vaccines per day assuming availability plateaus (pessimistic)
nvax_pes <- rep(NA_real_, length(vax_days))
nvax_pes[1] <- 1
nvax_pes[vax_last_obs-vax_start] <- nvax_last_obs
nvax_pes[length(vax_days)] <- nvax_last_obs
nvax_pes <- round(zoo::na.approx(nvax_pes))

vax_per_day_pes <- data.frame(days = vax_days ,
                              vax  = nvax_pes)



saveRDS(vax_per_day_pes, here::here("data", "processed", "vax_forecast_pes.rds"))
