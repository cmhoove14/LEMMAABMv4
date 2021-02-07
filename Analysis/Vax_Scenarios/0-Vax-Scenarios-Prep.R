# --------------------------
# Vax Scenarios
# Chris Hoover Feb 2021
# --------------------------

library(tidyverse)

vax_scens <- as_tibble(expand.grid(LOC = c("CT", "ZIP"),
                         MVMT = c("OCT", "DEC"),
                         VAX = c("RISE", "PLAT"),
                         MET = c("HOSP", "CASE"),
                         SCEN = c("65p", "ESS", "PLACE", "ALL")))


# Since eligibility placement metrics (hospitalizations or cases) don't matter for non-place based scenarios (all 65p and all essential workers), can remove those scenarios
vax_scens_distinct <- vax_scens %>% 
  mutate(MET = if_else(SCEN %in% c("65p", "ESS"), "NotApp", as.character(MET))) %>% 
  distinct() %>% View()
