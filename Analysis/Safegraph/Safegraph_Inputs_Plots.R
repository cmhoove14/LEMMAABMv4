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
