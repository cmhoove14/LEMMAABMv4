# ---------------------------------------
# Sensitivity of model fits to different parameters
# Chris Hoover Feb 2021
# ---------------------------------------
library(tidyverse)
library(sensitivity)

fits <- readRDS(here::here("data", "processed", "LHS_Fits1_summary.rds"))
LHS  <- as_tibble(readRDS(here::here("data", "processed", "Calibration_LHS_Wynton.rds"))) %>% 
  mutate(sim = row_number())

lhs_fits <- fits %>% left_join(LHS, by = "sim") %>% 
  filter(hosp_fit > 0)

