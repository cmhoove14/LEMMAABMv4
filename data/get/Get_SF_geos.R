library(geojsonsf)
library(sf)
library(tidyverse)

# SF Census tracts ----------------
system(paste0("wget -O ", here::here("data", "raw", "Census 2010_ Tracts for San Francisco.geojson"),
              " https://data.sfgov.org/api/geospatial/rarb-5ahf?method=export&format=GeoJSON")) 

SF_cts <- geojson_sf(here::here("data", "raw", "Census 2010_ Tracts for San Francisco.geojson"))

# Bay area zip codes --------------
system(paste0("wget -O ", here::here("data", "raw", "Bay Area ZIP Codes.geojson"),
              " https://data.sfgov.org/api/geospatial/u5j3-svi6?method=export&format=GeoJSON")) 

bay_zips <- geojson_sf(here::here("data", "raw", "Bay Area ZIP Codes.geojson"))
SF_zips <- bay_zips %>% filter(po_name == "SAN FRANCISCO" & zip != 94128)

#SF neighborhoods ------------------
system(paste0("wget -O ", here::here("data", "raw", "Analysis Neighborhoods.geojson"),
              " https://data.sfgov.org/api/geospatial/p5b7-5n3h?method=export&format=GeoJSON")) 

SF_nbhds <- geojson_sf(here::here("data", "raw", "Analysis Neighborhoods.geojson"))


# Merge geographies together ----------------
  # Check coordinate systems
  st_crs(SF_cts) == st_crs(SF_nbhds)
  st_crs(SF_nbhds) == st_crs(SF_zips)
  
  # Visually check overlap
  mapview::mapview(list(SF_cts, SF_nbhds))
  mapview::mapview(list(SF_cts, SF_zips))
  
  # joins
  SF_geos <- st_join(SF_cts, SF_zips, left = TRUE, largest = TRUE)
  SF_geos2 <- st_join(SF_geos, SF_nbhds, left = TRUE, largest = TRUE) %>% 
    filter(!is.na(zip)) %>% #Gets rid of two CTs with 0 population
    dplyr::select(geoid10, zip, nhood)
  
  saveRDS(SF_geos2, here::here("data", "processed", "SF_all_geos_sf.rds"))
  