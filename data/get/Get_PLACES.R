library(tidyverse)

system(paste0("wget -O ", here::here("data", "raw", "PLACES_all.csv"),
              " https://chronicdata.cdc.gov/api/views/cwsq-ngmh/rows.csv?accessType=DOWNLOAD")) 

PLACES <- read_csv(here::here("data", "raw", "PLACES_all.csv"))

PLACES_SF <- PLACES %>% 
  filter(CountyFIPS == "06075")

saveRDS(PLACES_SF, here::here("data", "processed", "SF_PLACES.rds"))
