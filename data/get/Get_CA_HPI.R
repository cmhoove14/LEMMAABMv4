library(tidyverse)

temp <- tempfile()

system(paste0("wget -O ", temp,
              " https://healthyplacesindex.org/wp-content/uploads/2019/04/HPI-Master-Files-2019-04-24.zip")) 

hpi <- read_csv(unz(temp, "HPI2_MasterFile_2019-04-24.csv"))

# Get quartile and quintile cutoffs fro use below
  hpi_4ile_3 <- max(hpi$hpi2score[which(hpi$quartiles == 4)])
  hpi_4ile_2 <- max(hpi$hpi2score[which(hpi$quartiles == 3)])
  hpi_4ile_1 <- max(hpi$hpi2score[which(hpi$quartiles == 2)])

  hpi_5ile_4 <- max(hpi$hpi2score[which(hpi$quintiles == 5)])
  hpi_5ile_3 <- max(hpi$hpi2score[which(hpi$quintiles == 4)])
  hpi_5ile_2 <- max(hpi$hpi2score[which(hpi$quintiles == 3)])
  hpi_5ile_1 <- max(hpi$hpi2score[which(hpi$quintiles == 2)])
  

hpi_sf <- hpi %>% 
  filter(substr(CensusTract, 2, 4) == "075" & pop2010 > 0) # 075 is SF county code ;  don't need cts with nobody in them

  #View(hpi_sf %>% filter(is.na(hpi2score)))

# There are a handful of SF Cts that do not have HPI data due to the HPI exclusion criteria see (https://map.healthyplacesindex.org/), so will fill in their data based on mean of their neighbors --------------------
library(rgeos)
library(sf)

sf_cts <- geojsonsf::geojson_sf(here::here("data/raw/Census 2010_ Tracts for San Francisco.geojson")) %>% 
  mutate(CensusTract = as.numeric(geoid10))

sf_hpi_cts <- sf_cts %>% left_join(hpi_sf, by = "CensusTract")

# Which cts are NA
hpi_sf_nas <- hpi_sf$CensusTract[ which(is.na(hpi_sf$hpi2score))]

# Neighbors matrix
ct_neighbors <- sf::st_intersects(sf_hpi_cts)
ct_nas <- which(sf_hpi_cts$CensusTract %in% hpi_sf_nas)

# Verify neighbors are actually neighbors
  # mapview::mapview(sf_hpi_cts %>% slice(ct_neighbors[[ct_nas[1]]]))
  # mapview::mapview(sf_hpi_cts %>% slice(ct_neighbors[[ct_nas[2]]]))
  # mapview::mapview(sf_hpi_cts %>% slice(ct_neighbors[[ct_nas[3]]]))
  # mapview::mapview(sf_hpi_cts %>% slice(ct_neighbors[[ct_nas[4]]]))
  # mapview::mapview(sf_hpi_cts %>% slice(ct_neighbors[[ct_nas[5]]]))
  # mapview::mapview(sf_hpi_cts %>% slice(ct_neighbors[[ct_nas[6]]]))
  
# Fill scores as population-weighted mean of neighbors
pop_weight_mean = function(pops, vals){
  pops    <- pops[!is.na(vals)]
  vals    <- vals[!is.na(vals)]
  weights <- pops/sum(pops)
  
  sum(weights*vals)
}

for(i in ct_nas){
  # Get data from neighbors
  neighbs <- sf_hpi_cts %>% 
    slice(ct_neighbors[[i]])

  CT <- which(hpi_sf$CensusTract == sf_hpi_cts$CensusTract[i])
  
  # Fill in mising scores with mean of neighbors
  hpi_sf$economic[CT]         = pop_weight_mean(neighbs$pop2010, neighbs$economic)
  hpi_sf$education[CT]        = pop_weight_mean(neighbs$pop2010, neighbs$education)
  hpi_sf$housing[CT]          = pop_weight_mean(neighbs$pop2010, neighbs$housing)
  hpi_sf$healthcareaccess[CT] = pop_weight_mean(neighbs$pop2010, neighbs$healthcareaccess)
  hpi_sf$neighborhood[CT]     = pop_weight_mean(neighbs$pop2010, neighbs$neighborhood)
  hpi_sf$pollution[CT]        = pop_weight_mean(neighbs$pop2010, neighbs$pollution)
  hpi_sf$social[CT]           = pop_weight_mean(neighbs$pop2010, neighbs$social)
  hpi_sf$transportation[CT]   = pop_weight_mean(neighbs$pop2010, neighbs$transportation)
  
  # Get overall score with weights from https://healthyplacesindex.org/about/
  hpi_sf$hpi2score[CT]        = sum(hpi_sf$economic[CT]*0.32,
                                    hpi_sf$social[CT]*0.10,
                                    hpi_sf$education[CT]*0.19,
                                    hpi_sf$transportation[CT]*0.16,
                                    hpi_sf$neighborhood[CT]*0.08,
                                    hpi_sf$housing[CT]*0.05,
                                    hpi_sf$pollution[CT]*0.05,
                                    hpi_sf$healthcareaccess[CT]*0.05)
  
  # Get quintile and quartile from comparing to statewide scores
  hpi_sf$quartiles[CT] <- ifelse(hpi_sf$hpi2score[CT] < hpi_4ile_3, 4,
                                 ifelse(hpi_sf$hpi2score[CT] < hpi_4ile_2, 3, 
                                        ifelse(hpi_sf$hpi2score[CT] < hpi_4ile_1, 2, 1)))
  
  hpi_sf$quintiles[CT] <- ifelse(hpi_sf$hpi2score[CT] < hpi_5ile_4, 5,
                                 ifelse(hpi_sf$hpi2score[CT] < hpi_5ile_3, 4, 
                                        ifelse(hpi_sf$hpi2score[CT] < hpi_5ile_2, 3, 
                                               ifelse(hpi_sf$hpi2score[CT] < hpi_5ile_1, 2, 1))))
  
}

# View(hpi_sf %>% filter(CensusTract %in% hpi_sf_nas))

hpi_sf_out <- hpi_sf %>% 
  dplyr::select(CensusTract, pop2010, City, ZIP, hpi2score, quintiles, quartiles, 
                economic, social, education, transportation, neighborhood, housing, pollution, healthcareaccess)

saveRDS(hpi_sf_out, here::here("data", "processed", "SF_HPI.rds"))
