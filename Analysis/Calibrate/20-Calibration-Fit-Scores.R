# ---------------------------------------
# Compare simulation to observed data metrics
# Chris Hoover Feb 2021
# ---------------------------------------

sim_folders <- as.numeric(list.files(here::here("data/outputs/LHS_Calibration/")))
sim_folders <- sim_folders[!is.na(sim_folders)]

clooster <- parallel::makeCluster(parallel::detectCores()-4)

parallel::clusterEvalQ(cl = clooster,
                       expr = lapply(c("data.table", "tidyverse", "zoo", "lubridate"), 
                                     library,
                                     character.only = TRUE))

parallel::clusterExport(cl = clooster, 
                        c("sim_folders"))

lhs_fits <- parallel::parLapply(cl = clooster,
                                X=sim_folders, 
                                fun = function(x){
                                  
                                  out <- tryCatch( # Some files failing to read, so added trycatch to skip them
                                    {
                                      # Get sim file  
                                      sim_folder <- paste0(here::here("data/outputs/LHS_Calibration", x))
                                      sim <- readRDS(paste0(sim_folder,"/",list.files(sim_folder)[1]))
                                      
                                      # Get observed
                                      load(here::here("data/get/got/CA_SF_data2021-02-04.Rdata"))
                                      
                                      # Compare daily hospitalizations ----------------
                                      sim_hosp <- as_tibble(sim$epi_curve[state == "Ih",])
                                      sim_hosp$Date <- as.Date(as.character(sim_hosp$date)) # Date formate from sim messed up because of sub-daily time step
                                      
                                      comp_hosp <- merge(sim_hosp, sf_hosp, by = "Date")
                                      
                                      hosp_mse <- sum((comp_hosp$N - comp_hosp$HOSP_CONF)^2)
                                      
                                      # Compare weekly deaths -------------------------
                                      sim_dths <- as_tibble(sim$agents[state == "D", c("id", "sex", "age", "race", "t_death")]) %>% 
                                        mutate(
                                          tod = zoo::as.Date.numeric(t_death),
                                          wod =paste0(lubridate::epiweek(tod), "_",
                                                      lubridate::year(tod))
                                        )
                                      
                                      sim_dths_wk <- sim_dths %>% 
                                        group_by(wod) %>% 
                                        summarise(n_d_sim = n())
                                      
                                      obs_dths_wk <- sf_case %>% 
                                        dplyr::select(Date, Deaths) %>% 
                                        mutate(wod = paste0(lubridate::epiweek(Date), "_",
                                                            lubridate::year(Date))) %>% 
                                        group_by(wod) %>% 
                                        summarise(n_d_obs = sum(Deaths))
                                      
                                      comp_dths <- merge(sim_dths_wk, obs_dths_wk, by = "wod")
                                      
                                      dths_mse <- sum((comp_dths$n_d_sim - comp_dths$n_d_obs)^2)
                                      
                                      # Compare cumulative Dec 1 deaths by race ---------------------
                                      # State database only has non-hispanic white, non-hispanic black, hispanic, and other, so condense to match
                                      sim_dths_race <- sim_dths %>% 
                                        mutate(race2 = if_else(race %in% c(1,2,8), race, 9)) %>% 
                                        group_by(race2) %>% 
                                        summarise(n_dths = n())
                                      
                                      obs_dths_race <- data.frame(race2 = c(1, 2, 8, 9),
                                                                  deaths = c(55,9,44,78))
                                      
                                      comp_dths_race <- merge(sim_dths_race, obs_dths_race, by = "race2")
                                      
                                      dths_race_mse <- sum((comp_dths_race$n_dths - comp_dths_race$deaths)^2)
                                      
                                      # Compare Monthly census tract confirmed cases -------------------
                                      sim_cases <- sim$linelist_tests %>% 
                                        dplyr::select(id,race,state,ct, Date,test_pos) %>% 
                                        filter(test_pos == 1) %>% 
                                        #bug in code that added all agents waiting on test results rather than all newly tested agents, below corrects to only keep first record
                                        # arrange(id, Date) %>% View()
                                        mutate(MO_YR = paste0(lubridate::month(Date), "_", lubridate::year(Date))) %>% 
                                        dplyr::select(-Date) %>% 
                                        distinct() %>% 
                                        group_by(ct, MO_YR) %>% 
                                        summarise(n_sim = sum(test_pos))
                                      
                                      obs_cases <- sf_geo_null %>% 
                                        mutate(MO_YR = paste0(lubridate::month(Date), "_", lubridate::year(Date)),
                                               ct = as.numeric(id)) %>% 
                                        group_by(ct, MO_YR) %>% 
                                        summarise(n_obs = sum(as.numeric(new_confirmed_cases)))
                                      
                                      # Get reference data frame for all potential ct-months of comparison
                                      month_dates <- seq.Date(sim$input_pars$test_pars$test_start, sim$input_pars$time_pars$t.end, by = "month")
                                      MO_YRs      <- paste0(lubridate::month(month_dates), "_", lubridate::year(month_dates))
                                      all_cts     <- unique(sim$agents$ct)
                                      
                                      ct_months <- as_tibble(expand.grid("ct" = all_cts, 
                                                                         "MO_YR" = MO_YRs))
                                      
                                      comp_ct_cases <- ct_months %>% 
                                        left_join(sim_cases, by = c("ct", "MO_YR")) %>% 
                                        left_join(obs_cases, by = c("ct", "MO_YR")) %>% 
                                        replace_na(list(n_sim = 0, n_obs = 0)) # Assume no data means no cases
                                      
                                      ct_cases_mse <- sum((comp_ct_cases$n_sim - comp_ct_cases$n_obs)^2)
                                      
                                      # Compare cumulative Dec 1 confirmed cases by race ----------------------
                                      sim_case_race <- sim$linelist_tests %>% 
                                        filter(test_pos == 1 & Date <= as.Date("2020-12-01")) %>% 
                                        dplyr::select(-Date) %>% # Don't care about date since comparing cumulative counts
                                        distinct() %>%  #Remove duplicate rows due to error mentioned above
                                        group_by(race) %>% 
                                        summarise(n_sim = n())
                                      
                                      obs_case_race <- sf_case_race %>% 
                                        filter(Date == as.Date("2020-12-01")) %>% 
                                        dplyr::select(Race,Cum_Cases) %>% 
                                        rename("n_obs" = Cum_Cases)
                                      
                                      # Quite a few NAs, so allocate them in proportion to cases with known race
                                      # This assumes there aren't systematic biases in reporting of race among known cases, which, probably not true, but best we can do
                                      case_race_non_na <- obs_case_race$n_obs[which(!is.na(obs_case_race$Race))]
                                      case_race_na     <- obs_case_race$n_obs[which(is.na(obs_case_race$Race))]
                                      obs_total        <- sum(case_race_non_na)
                                      obs_ratios       <- case_race_non_na / obs_total
                                      obs_add          <- round(case_race_na*obs_ratios)
                                      
                                      obs_case_race2 <- obs_case_race[!is.na(obs_case_race$Race),]
                                      obs_case_race2$n_obs <- obs_case_race2$n_obs + obs_add
                                      
                                      comp_case_race <- merge(sim_case_race, obs_case_race2, by.x = "race", by.y = "Race")
                                      
                                      case_race_mse <- sum((comp_case_race$n_sim - comp_case_race$n_obs)^2)
                                      
                                      rm(sim)
                                      gc()               
                                      
                                      return(list("sim"           = x, 
                                                  "status"        = 1,
                                                  "hosp_mse"      = hosp_mse,
                                                  "dths_mse"      = dths_mse,
                                                  "dths_race_mse" = dths_race_mse,
                                                  "ct_cases_mse"  = ct_cases_mse,
                                                  "case_race_mse" = case_race_mse))
                                      
                                    },
                                    error = function(cond){
                                      return(list("sim"           = x, 
                                                  "status"        = cond,
                                                  "hosp_mse"      = NA,
                                                  "dths_mse"      = NA,
                                                  "dths_race_mse" = NA,
                                                  "ct_cases_mse"  = NA,
                                                  "case_race_mse" = NA))
                                    }
                                  )  
                                  
                                  return(out)
                                  
                                })
                      
parallel::stopCluster(clooster)

saveRDS(lhs_fits, here::here("data/outputs/LHS_Calibration/LHS_Fits.rds"))                      

