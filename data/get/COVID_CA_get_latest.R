library(tidyverse)
library(geojsonsf)

# Get CA & SF data
if(sum(grepl(Sys.Date(), list.files(here::here("data", "get", "got")))) == 0){
  
# Get CA data -----------------  
  system(paste0("wget -O ", here::here("data", "get", "got", "CA_cases"), Sys.Date(), ".csv " ,
                "https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv")) 
  
  CA_cases <- read.csv(paste0(here::here("data", "get", "got", "CA_cases"), Sys.Date(), ".csv"))
  
  system(paste0("wget -O ", here::here("data", "get", "got", "CA_tests"), Sys.Date(), ".csv " ,
                "https://data.ca.gov/dataset/efd6b822-7312-477c-922b-bccb82025fbe/resource/b6648a0d-ff0a-4111-b80b-febda2ac9e09/download/statewide_testing.csv"))
  
  CA_tests <- read.csv(paste0(here::here("data", "get", "got", "CA_tests"), Sys.Date(), ".csv"))
    
  system(paste0("wget -O ", here::here("data", "get", "got", "CA_hosp"), Sys.Date(), ".csv " ,
                "https://data.ca.gov/dataset/529ac907-6ba1-4cb7-9aae-8966fc96aeef/resource/42d33765-20fd-44b8-a978-b083b7542225/download/hospitals_by_county.csv"))

  CA_hosp <- read.csv(paste0(here::here("data", "get", "got", "CA_hosp"), Sys.Date(), ".csv" ))
    
# Get SF data ----------------------
  system(paste0("wget -O ", here::here("data", "get", "got", "SF_hosp"), Sys.Date(), ".csv " ,
                "https://data.sfgov.org/resource/nxjg-bhem.csv"))
  
  system(paste0("wget -O ", here::here("data", "get", "got", "SF_case"), Sys.Date(), ".csv " ,
                "https://data.sfgov.org/resource/tvq9-ec9w.csv"))
  
  system(paste0("wget -O ", here::here("data", "get", "got", "SF_test"), Sys.Date(), ".csv " ,
                "https://data.sfgov.org/resource/nfpa-mg4g.csv"))
  
  system(paste0("wget -O ", here::here("data", "get", "got", "SF_geo"), Sys.Date(), ".geojson " ,
                "https://data.sfgov.org/api/views/d2ef-idww/rows.geojson?accessType=DOWNLOAD"))

# Clean SF data ---------------------    
### Hospitalizations  
  sf_hosp <- read.csv(paste0(here::here("data", "get", "got", "SF_hosp"), Sys.Date(), ".csv" )) %>% 
    mutate(Date = as.Date(reportdate),
           type = ifelse(dphcategory == "ICU", "ICU", "HOSP"),
           conf = ifelse(covidstatus == "PUI", "PUI", "CONF"),
           hosp_stat = paste(type, conf, sep = "_")) %>% 
    pivot_wider(names_from = hosp_stat,
                values_from = patientcount) %>%
    group_by(Date) %>% 
    summarise(ICU_PUI = sum(ICU_PUI, na.rm = T),
              ICU_CONF = sum(ICU_CONF, na.rm = T),
              HOSP_PUI = sum(HOSP_PUI, na.rm = T),
              HOSP_CONF = sum(HOSP_CONF, na.rm = T)) %>% 
    arrange(Date) %>% 
    mutate(HOSP_tot = ICU_CONF + HOSP_CONF,
           HOSP_max = ICU_CONF + HOSP_CONF + ICU_PUI + HOSP_PUI,
           cumICUconf = cumsum(ICU_CONF),
           cumICUpui = cumsum(ICU_PUI),
           cumHOSPconf = cumsum(HOSP_CONF),
           cumHOSPpui = cumsum(HOSP_PUI))
  
### Cases  
  sf_case_raw <- read.csv(paste0(here::here("data", "get", "got", "SF_case"), Sys.Date(), ".csv")) %>% 
    mutate(Date = as.Date(specimen_collection_date))
  
  #sf_case_raw %>% filter(case_disposition == "Confirmed") %>% ggplot() + geom_line(aes(x = Date, y = case_count, col = transmission_category)) + theme_bw()
  #sf_case_raw %>% filter(case_disposition == "Confirmed") %>% group_by(Date) %>% summarise(tot_cases = sum(case_count), cont_cases = case_count[which(transmission_category == "From Contact")], prop_contact = cont_cases/tot_cases) %>% ggplot() + geom_line(aes(x = Date, y = prop_contact)) + theme_bw() + labs(title = "Proportion of cases identified from contact")
  
  sf_case <- sf_case_raw %>% 
    padr::pad(., start_val = as.Date("2020-03-01")) %>%  
    pivot_wider(names_from = case_disposition,
                values_from = case_count) %>%
    group_by(Date) %>% 
    summarise(Cases = sum(Confirmed, na.rm = T),
              Deaths = sum(Death, na.rm = T),
              Cases_Community = sum(Confirmed[which(transmission_category == "Community")], na.rm = T),
              Cases_Contact = sum(Confirmed[which(transmission_category == "From Contact")], na.rm = T),
              Cases_Unknown = sum(Confirmed[which(transmission_category == "Unknown")], na.rm = T),
              Deaths_Community = sum(Death[which(transmission_category == "Community")], na.rm = T),
              Deaths_Contact = sum(Death[which(transmission_category == "From Contact")], na.rm = T),
              Deaths_Unknown = sum(Death[which(transmission_category == "Unknown")], na.rm = T)) %>% 
    arrange(Date) %>% 
    mutate(cum_case = cumsum(Cases),
           cum_death = cumsum(Deaths))
  
### Tests  
  sf_test <- read.csv(paste0(here::here("data", "get", "got", "SF_test"), Sys.Date(), ".csv")) %>% 
    mutate(Date = as.Date(specimen_collection_date)) %>% 
    arrange(Date) %>% 
    mutate(cum_tests = cumsum(tests),
           cum_pos = cumsum(pos))
  
  sf_all <- sf_test %>% 
    dplyr::select(Date, tests, pos, neg, pct, indeterminate, cum_tests, cum_pos) %>%
    full_join(sf_case, by = "Date") %>% 
    full_join(sf_hosp, by = "Date") %>% 
    mutate(time = as.integer(Date - as.Date("2020-02-29"))) %>% 
    filter(time >0)
  
### SF cases by geography
  sf_geo <- geojson_sf(paste0(here::here("data", "get", "got", "SF_geo"), Sys.Date(), ".geojson")) %>% 
    filter(area_type == "Census Tract") %>% 
    mutate(
      Date = as.Date(specimen_collection_date)
    ) %>% 
    dplyr::select(Date, id, acs_population, 
                  new_confirmed_cases, cumulative_confirmed_cases, rate_of_cumulative_confirmed_case, 
                  geometry)
  
# Save final object  
  save(list=c("sf_all", "sf_case", "sf_hosp", "sf_test", "sf_geo", "CA_cases", 
              "CA_hosp", "CA_tests"), 
       file=here::here("data", "get", "got", paste0("CA_SF_data", Sys.Date(), ".Rdata")))
  
# Delete csvs, geojson, and any older data files
  got_files <- list.files(here::here("data", "get", "got"))
  
  lapply(got_files[grepl(".csv", got_files)],
         function(file){
           unlink(paste0(here::here("data", "get", "got"), "/", file))
         })
  
  unlink(got_files[grepl(".geojson", got_files)])
  
  del_date <- Sys.Date() - 3
  
  unlink(got_files[grepl(del_date, got_files)])
  
} else {
  load(paste0(here::here("data", "get", "got"), "/CA_SF_data", Sys.Date(), ".Rdata"))
}
