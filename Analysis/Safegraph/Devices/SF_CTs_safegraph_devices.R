# Convert SF stay at home metrics from CBG level to CT level

library(tidyverse)

sf_devices <- readRDS(here::here("data","processed","Safegraph","SF_devices_sum2020-01-01to2020-12-31.rds"))

sf_devices_ct <- sf_devices %>% 
  mutate(
    CT = substr(origin_census_block_group, 1, 11)
  ) %>% 
  group_by(CT, Date) %>% 
  summarise(
    across(device_count:full_time_work_behavior_devices, sum)
  ) %>% 
  mutate(
    pct_home      = completely_home_device_count/device_count,
    pct_part_work = part_time_work_behavior_devices/device_count,
    pct_full_work = full_time_work_behavior_devices/device_count
  )
    

saveRDS(sf_devices_ct, here::here("data","processed","Safegraph","sfgrph_devices_pct_home_cts_2020.rds"))
