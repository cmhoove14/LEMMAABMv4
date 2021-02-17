# --------------------------
# Vax Scenarios
# Chris Hoover Feb 2021
# --------------------------

library(tidyverse)
library(data.table)


# Reduced scenarios for CDPH interests ---------------------
input_pars <- readRDS("data/processed/input_pars_vax.rds")

vax_start <- input_pars$vax_pars$vax_start
vax_phase_dates <- vax_start + c(0,35,63)

vax_scens <- as_tibble(expand.grid(LOC  = "ZIP",
                                   MVMT = c("OPT", "PES"),
                                   VAX  = c("OPT", "PES"),
                                   MET  = "HPI",
                                   TGT = c("65p", "PLACE")))

n_runs_per_scen <- 25

vax_scens_reps <- vax_scens %>% slice(rep(1:n(), each = n_runs_per_scen))

save(list = c("vax_scens_reps", "vax_phase_dates"),
     file = here::here("data/processed/vax_scenarios_short.Rdata"))


# Comprehensive accounting of vaccination scenarios --------------------
# vax_scens <- as_tibble(expand.grid(LOC  = c("CT", "ZIP"),
#                                    MVMT = c("OCT", "DEC"),
#                                    VAX  = c("RISE", "PLAT"),
#                                    MET  = c("HOSP", "CASE"),
#                                    SCEN = c("65p", "ESS", "PLACE", "ALL")))


# Since eligibility placement metrics (hospitalizations or cases) don't matter for non-place based scenarios (all 65p and all essential workers), can remove those scenarios
# vax_scens_distinct <- vax_scens %>% 
#   mutate(MET = if_else(SCEN %in% c("65p", "ESS"), "NotApp", as.character(MET))) %>% 
#   distinct()
# 
# # Determine number of essential workers and 65+ agents to approximate amount of time to spend in each phase
# agents <- readRDS(here::here("data", "processed", "SF_agents_processed.rds"))
#   agents_65p <- agents[age >= 65,]
#   agents_ess <- agents[essential == 1,]  
  