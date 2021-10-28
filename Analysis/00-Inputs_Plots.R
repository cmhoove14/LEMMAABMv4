# ---------------------------------------
# Safegraph Inputs Viz
# Chris Hoover Feb 2021
# ---------------------------------------

library(tidyverse)
library(data.table)

sf_geos <- readRDS(here::here("data/processed/SF_all_geos_sf.rds")) %>% 
  mutate(CensusTract = as.numeric(geoid10))
sf_HPIs <- readRDS(here::here("data/processed/SF_HPI.rds"))

sf_geo_hpi <- sf_geos %>% 
  left_join(sf_HPIs, by = "CensusTract")

# Plot estimate of safegraph infected visitor rate through time ---------------------------
visitors20 <- readRDS(here::here("data/processed/Safegraph/SF_visitors_CTs_2020Processed.rds"))
visitors21 <- readRDS(here::here("data/processed/Safegraph/SF_visitors_CTs_2021Processed.rds"))

visitors <- rbindlist(c(visitors20, visitors21))

visitor_mult_sfgrph   <- 8
visitor_mult_testing <- 4

#Adjust for underreporting 
visitors[,inf_visits_rate_unadj := Visits*inf_prob]
visitors[,visits_adjust := Visits*visitor_mult_sfgrph]
visitors[,inf_adjust := newcount7day*visitor_mult_testing]
visitors[,inf_prob_adjust := inf_adjust/pops]
visitors[,inf_visits_rate := visits_adjust*inf_prob_adjust]
visitors[,CensusTract := as.numeric(ct)]

vis_geo_hpi <- visitors %>% left_join(sf_geo_hpi, by = "CensusTract")

vis_geo_hpi %>% 
  filter(!is.na(hpi2score)) %>%  #Just removes census tracts with no population
  group_by(quartiles, date) %>% 
  summarise(med_inf_visit_rate = median(inf_visits_rate_unadj),
            loq_inf_visit_rate = quantile(inf_visits_rate_unadj,0.25),
            hiq_inf_visit_rate = quantile(inf_visits_rate_unadj,0.75)) %>% 
  ggplot(aes(x = date, y = med_inf_visit_rate, col = as.factor(quartiles), fill = as.factor(quartiles))) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = loq_inf_visit_rate, ymax = hiq_inf_visit_rate), alpha = 0.3) +
    theme_classic() +
    labs(y = "Median expected rate of infected visitors",
         title = "Infected visitors rate by HPI quartile",
         fill = "HPI Quartile",
         col = "HPI Quartile")

ggsave(here::here("Plots/Inputs/expected_infected_visitors_by_hpi_quartile.jpg"),
       width = 5, height = 4, units = "in")

# Plot estimate of safegraph stay at home metrics through time ---------------------------
home20 <- readRDS(here::here("data/processed/Safegraph/sfgrph_devices_pct_home_cts_2020.rds"))
home21 <- readRDS(here::here("data/processed/Safegraph/sfgrph_devices_pct_home_cts_2021.rds"))

home_all <- as_tibble(rbind(home20, home21)) %>% 
  mutate(CensusTract = as.numeric(CT))

home_all_hpi <- home_all %>% 
  left_join(sf_geo_hpi, by = "CensusTract")

home_all_hpi_sum <- home_all_hpi %>% 
  group_by(Date, quartiles) %>% 
  summarise(n_devices = sum(device_count),
            n_pop = sum(pop2010),
            dev2pop = n_devices/n_pop,
            n_home = sum(completely_home_device_count),
            n_work = sum(part_time_work_behavior_devices)+sum(full_time_work_behavior_devices),
            `Percent Home` = n_home/n_devices,
            `Percent Work` = n_work/n_devices)

home_all_hpi_sum %>% 
  filter(!is.na(quartiles)) %>% 
  pivot_longer(`Percent Home`:`Percent Work`,
               names_to = "home_work",
               values_to = "percent") %>% 
  ggplot(aes(x = Date, y = percent, col = as.factor(quartiles), fill = as.factor(quartiles))) +
  facet_wrap("home_work", nrow = 2, scales = "free_y") +
  geom_line() +
  theme_classic() +
  labs(y = "Proportion device behavior",
       title = "Safegraph device behavior by HPI quartile",
       fill = "HPI Quartile",
       col = "HPI Quartile")

ggsave(here::here("Plots/Inputs/safegraph_percent_home_work_behavior.jpg"),
       width = 6, height = 6, units = "in")

# Correlation between cumulative cases and HPI ------------------------
load(here::here("data/get/got/CA_SF_data2021-02-10.Rdata"))

sf_ct_cases_jan21 <- sf_geo_null %>% 
  filter(Date == as.Date("2021-01-01")) %>% 
  mutate(CensusTract = as.numeric(id),
         prev_rate = cumulative_confirmed_cases/acs_population)

ct_case_hpi <- sf_ct_cases_jan21 %>% 
  left_join(sf_HPIs, by = "CensusTract")

hpi_prev_r2 <- cor(ct_case_hpi$hpi2score, ct_case_hpi$prev_rate)^2

ct_case_hpi %>% 
  ggplot(aes(x = hpi2score, y = prev_rate)) +
    geom_point(aes(col = as.factor(quartiles))) +
    theme_classic() +
    geom_smooth(method = "lm") +
    annotate(geom = "text",
             x = 1,
             y = 0.1,
             label = paste0(expression(r^2)," = ", round(hpi_prev_r2,2))) +
    labs(x = "Healthy Places Index",
         y = "Jan 2021 Cumulative Prevalence",
         col = "HPI Quartile")
    
ggsave(here::here("Plots/Inputs/hpi_Jan2021_cum_prev_rate_corr.jpg"),
       width = 5, height = 4, units = "in")
