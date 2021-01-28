library(tidyverse)

temp <- tempfile()

system(paste0("wget -O ", temp,
              " https://healthyplacesindex.org/wp-content/uploads/2019/04/HPI-Master-Files-2019-04-24.zip")) 

hpi <- read_csv(unz(temp, "HPI2_MasterFile_2019-04-24.csv"))

hpi_sf <- hpi %>% 
  filter(substr(CensusTract, 2, 4) == "075") # 075 is SF county code

saveRDS(hpi_sf, here::here("data", "processed", "SF_HPI.rds"))
