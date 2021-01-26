library(tidyverse)
library(data.table)
library(wrswoR)
library(dqrng)
library(fitdistrplus)
library(matrixStats)
library(lubridate)
library(fastmatch)
library(parallel)
library(here)
#library(LEMMAABMv3)

source(here("data", "get","COVID_CA_get_latest.R"))

# ABM functions
devtools::load_all()

# synthetic agents from Census/IPUMS data ------------
agents <- readRDS(here::here("data", "processed", "SF_agents_processed.rds"))
  setkey(agents, id, hhid)

# Add column(s) for use in model
  agents[, state := "S"]
  agents[, nextstate := NA]
  agents[, tnext := 0]
  agents[, t_symptoms := 0]
  
  
# UNCOMMENT BELOW/change size of sample TO Subset for development for faster runs/lower memory
# agents <- agents[agents$residence %in% sample(agents$residence, 2e4, replace = F)]  
  
N <- nrow(agents)  

#Plot tests through time
#ggplot(data = sf_test) + geom_line(aes(x = Date, y = (tests/9e5)*1e5)) + theme_bw() +scale_x_date(date_breaks = "14 day") +theme(axis.text.x = element_text(angle = 45,hjust = 1))+labs(x="",y="SF Tests per 100k")

# Testing data for sims, blank functions ----------------------
  tests0 <- function(date_num){
    return(0)
  }

sf_test_smooth <- sf_test %>% 
  mutate(Date = as.Date(substr(specimen_collection_date, 1,10)),
         date_num = as.numeric(Date-as.Date("2019-12-31")),
         tests_pp = tests/N) %>% 
  padr::pad() %>% 
  padr::fill_by_value(tests_pp, value = 0) %>% 
  mutate(tests_pp_7day_avg = zoo::rollmean(tests_pp, 7, na.pad = T, align = "center"),
         tests_pp_7day_avg = case_when(is.na(tests_pp_7day_avg) & Date < as.Date("2020-03-10") ~ 2/N,
                                       is.na(tests_pp_7day_avg) & Date > as.Date("2020-03-10") ~ 5000/N,
                                       !is.na(tests_pp_7day_avg) ~ tests_pp_7day_avg)) 

last_sf_test <- max(sf_test_smooth$Date)
# sf_test_smooth %>%  ggplot() + geom_line(aes(x = Date, y = tests_pp)) + geom_line(aes(x = Date, y = tests_pp_7day_avg), col = "red") + theme_classic()

tests_fx <- approxfun(sf_test_smooth$date_num,
                      sf_test_smooth$tests_pp_7day_avg)

# Safegraph data UPDATE -------------------------
#San Francisco cbg mvmt list derived from sfgrph data
  sf_ct_cdf_ls <- readRDS(here::here("data", "processed", "Safegraph", "safegraph_ct_mvmt_cdf_list_2020processed.rds"))
  sf_ct_ids <- read_csv(here::here("data", "raw", "Census_2010_Tracts.csv")) %>% pull(GEOID10) %>% as.numeric()

#San francisco stay at home by percent by cbg derived from safegraph
  sf_sfgrph_pcts <- readRDS(here::here("data", "processed","Safegraph", "sfgrph_devices_pct_home_cts_2020.rds"))
  
  sf_sfgrph_pcts %>% 
    group_by(Date) %>% 
    summarise(n_devices     = sum(device_count), 
              n_home        = sum(completely_home_device_count), 
              n_part_work   = sum(part_time_work_behavior_devices), 
              n_full_work   = sum(full_time_work_behavior_devices), 
              per_home      = n_home/n_devices, 
              per_part_work = n_part_work/n_devices, 
              per_full_work = n_full_work/n_devices) %>% 
    ggplot() +
      geom_line(aes(x = Date, y = per_home), col = "green") +
      geom_line(aes(x = Date, y = per_part_work), col = "orange") +
      geom_line(aes(x = Date, y = per_full_work), col = "red") +
      theme_bw() +
      labs(y = "Pct device behavior",
           title = "Safegraph home/work metrics SF County")
    
  sf_sfgrph_pct_home <- sf_sfgrph_pcts %>% 
    dplyr::select(CT, Date, pct_home) %>% 
    as.data.table()

# Visitors to SF county from other CA counties
  sf_visitors <- readRDS(here::here("data", "processed", "Safegraph", "SF_visitors_CTs_2020Processed.rds"))
  
# ---------------------------------------------------------
# Pre-process data and sim setup
# ---------------------------------------------------------
# Time frame and step
t0    <- as.Date("2020-02-17")
today <- Sys.Date()

# Key change dates
SiP.start    <- as.Date("2020-03-15")  # Shelter in Place started
mask.start   <- as.Date("2020-04-17")  # Mask mandate initiated
reopen.start <- as.Date("2020-05-17")  # Start of reopening initiatives in SF
SiP2.start <- as.Date("2020-12-06")  # Start of reopening initiatives in SF
holidays     <- c(seq.Date(as.Date("2020-05-23"), as.Date("2020-05-25"), by = "day"), # Memorial Day
                  seq.Date(as.Date("2020-07-03"), as.Date("2020-07-05"), by = "day"), # 4th of July
                  seq.Date(as.Date("2020-09-05"), as.Date("2020-09-07"), by = "day"), # Labor day
                  seq.Date(as.Date("2020-11-26"), as.Date("2020-11-29"), by = "day"), # Thanksgiving
                  seq.Date(as.Date("2020-12-24"), as.Date("2020-12-26"), by = "day"), # Christmas 
                  seq.Date(as.Date("2020-12-31"), as.Date("2021-01-01"), by = "day")) # New Years
t.end        <- as.Date("2020-12-31")       # Simulation end date

t.tot <- as.numeric(t.end - t0)
dt = 4/24

# Day of week
day_of_week                                 <- lubridate::wday(seq.Date(t0, t.end, by = "day"))
day_of_week_expand                          <- rep(day_of_week, each = 1/dt)
day_of_week_expand[day_of_week_expand == 1] <- "U"
day_of_week_expand[day_of_week_expand == 2] <- "M"
day_of_week_expand[day_of_week_expand == 3] <- "T"
day_of_week_expand[day_of_week_expand == 4] <- "W"
day_of_week_expand[day_of_week_expand == 5] <- "R"
day_of_week_expand[day_of_week_expand == 6] <- "F"
day_of_week_expand[day_of_week_expand == 7] <- "S"

# Break up week into 6 parts, Morning, Dayx2, Evening, Nightx2: SHOULD UPDATE IF timestep!=4/24
time_of_day <- rep(c("M", "D", "D", "E", "N", "N"), times = t.tot)  

# Transmission probabilities across different edges https://www.medrxiv.org/content/10.1101/2020.05.10.20097469v1.full.pdf
  #trans.hh <- 0.1
  #trans.work <- 0.01
  #trans.school <- 0.02
  #trans.other <- 0.002
  
# Actually don't think this is necessary as the way transmission works by estimating FOI in each location will naturally lead to more transmission in smaller areas (e.g. households, offices, classrooms) than in larger areas  
  trans.all <- 0.27
  
# Beta reduction to account for micro behaviors that are not accounted for in reductions in movement that cause reduction in transmission probability
  bta_change_df <- data.frame(dates = c(t0, SiP.start, reopen.start, holidays),
                              btas = c(trans.all, trans.all*(1/3), trans.all*(1/3), rep(trans.all, length(holidays)))) %>% 
    padr::pad() %>% 
    mutate(date_num = as.numeric(dates - as.Date("2019-12-31")))

  bta_change_df$btas[is.na(bta_change_df$btas) & bta_change_df$dates < SiP.start] <- trans.all
  bta_change_df$btas[is.na(bta_change_df$btas) & bta_change_df$dates > SiP.start & bta_change_df$dates < reopen.start] <- trans.all*(1/3)
  bta_change_df$btas[is.na(bta_change_df$btas) & bta_change_df$dates > SiP.start & bta_change_df$dates > reopen.start] <- trans.all*(1/3)
  
  bta_change_fx <- approxfun(bta_change_df$date_num,
                             bta_change_df$btas)
  
  #plot(sapply(1:1000, bta_change_fx), type = "l")
# ---------------------------------------------------------    
# Troubleshoot run model
# ---------------------------------------------------------  
agents2 <- as.data.frame(agents)
  
# Call ABM_V3 function
#  abm_v3_test_run <- covid_abm_v3(bta_fx = bta_change_fx, bta_hh = 1, bta_scl = 1, bta_work = 1, bta_other = 1, E0 = 5, Ip0 = 2, Ia0 = 1, Im0 = 0, Imh0 = 0, Ih0 = 0, R0 = 0, D0 = 0, agents_dt = agents, cbg_cdf = sf_cbg_cdf_expand, ct_ids = sf_ct_ids, stay_home_dt = sf_sfgrph_pct_home_expand, t0 = t0, t.tot = t.tot, dt = dt, day_of_week_fx = day_of_week_expand, time_of_day_fx = time_of_day, SiP.start = SiP.start, scl.close = scl.close, mask.start = mask.start, mask.red = 0.6, mask_fx = mask_fx, quar_fx = quar_fx, social_fx = social_fx, q_dur_fx = q_dur_fx, init_other = 100, other_bta = 0.75, other_symp_dur_fx  = other_symp_dur_fx, testing = FALSE, test_delay_fx = test_delay_fx, tests_pub_fx = tests0, tests_pvt_fx = tests0, test_probs_fx = test_probs_fx, adaptive = FALSE, adapt_start  = 0, adapt_freq = 7, adapt_site_fx = adapt_site_fx, adapt_site_mult = 3 , adapt_site_delay_fx = test_delay_fx) 

# For troubleshooting ------------------------
agents <- as.data.table(agents2)
  
bta_fx = bta_change_fx ;  bta_hh = 1 ; bta_work = 1 ; E0 = 3 ;  Ip0 = 2 ;  Ia0 = 1 ;  Im0 = 0 ;  Imh0 = 0 ;  Ih0 = 0 ;  R0 = 0 ;  D0 = 0 ; agents_dt = agents ;  ct_cdf_list = sf_ct_cdf_ls;  ct_ids = sf_ct_ids ; stay_home_dt = sf_sfgrph_pct_home ; visitors = TRUE ; visitors_list = sf_visitors; visitor_mult_testing = 10 ; visitor_mult_sfgrph = 100 ; t0 = t0 ;  t.tot = t.tot ;  dt = dt ;  day_of_week_fx = day_of_week_expand ;  time_of_day_fx = time_of_day ; SiP.start = SiP.start ; part.reopen = reopen.start; mask.start = mask.start ;  mask.red = 0.6 ; mask_fx = mask_fx ; social_fx = social_fx ; q_prob_contact = 0.2^4 ; q_prob_resinf = 0.5^4 ; q_prob_symptoms =0.5^4 ; q_prob_testpos = 0.9^4 ; q_dur_fx = q_dur_fx ; known_contact_prob = 9 ; testing = TRUE ; test_start = as.Date("2020-03-01") ; test_delay_fx = test_delay_fx ;  tests_pp_fx = tests_fx ; tests_pub = 0.6 ; tests_wknd = 0.5 ; test_probs_pub_fx = test_probs_pub_fx ; test_probs_pvt_fx = test_probs_pvt_fx ; symp_mult = 10 ; race_test_mults = rep(1,8) ; cont_mult = 10 ; res_mult = 100 ; nosymp_state_mult = 1 ; symp_state_mult = 1000 ; hosp_mult =10000 ; test.red = 0 ; adaptive = FALSE ; adapt_start = as.Date("2020-04-01") ; adapt_freq = 14 ; adapt_site_fx = adapt_site_fx ; adapt_site_geo = "nbhd"; n_adapt_sites = 1 ; adapt_site_test_criteria = "per_pos"; adapt_site_mult = 4 ; adapt_site_delay_fx = test_delay_fx ; verbose = TRUE ; store_extra = TRUE
  
set.seed(430)
  
# Initial conditions
agents <- agents_dt
N <- nrow(agents)

e.seed   <- E0     #Exposed
ip.seed  <- Ip0    #infected pre-symptomatic
ia.seed  <- Ia0    #infected asymptomatic
im.seed  <- Im0    #infected mildly symptomatic
imh.seed <- Imh0   #infected mildly symptomatic, will become severe
ih.seed  <- Ih0    #infected severely symptomatic
d.seed   <- D0     #dead
r.seed   <- R0     #removed
non.s    <- e.seed + ip.seed + ia.seed + im.seed + imh.seed + ih.seed + d.seed + r.seed
s.seed   <- N - non.s


# Initial infection allocated among non-children
init.Es   <- sample(agents[!age %in% c(5,15,65,75,85), id], e.seed)   
init.Ips  <- sample(agents[!age %in% c(5,15,65,75,85), id], ip.seed)   
init.Ias  <- sample(agents[!age %in% c(5,15,65,75,85), id], ia.seed)   
init.Ims  <- sample(agents[!age %in% c(5,15,65,75,85), id], im.seed)   
init.Imhs <- sample(agents[!age %in% c(5,15,65,75,85), id], imh.seed)   
init.Ihs  <- sample(agents[!age %in% c(5,15,65,75,85), id], ih.seed)   
init.Ds   <- sample(agents[!age %in% c(5,15,65,75,85), id], d.seed)   
init.Rs   <- sample(agents[!age %in% c(5,15,65,75,85), id], r.seed)   

agents[id %in% init.Es, state:="E"]
agents[id %in% init.Ips, state:="Ip"]
agents[id %in% init.Ias, state:="Ia"]
agents[id %in% init.Ims, state:="Im"]
agents[id %in% init.Imhs, state:="Imh"]
agents[id %in% init.Ihs, state:="Ih"]
agents[id %in% init.Ds, state:="D"]
agents[id %in% init.Rs, state:="R"]

# Keep track of everyone's infection status through time   
epi_curve <- matrix(NA, nrow = t.tot/dt, ncol = 9)
epi_curve[1,] <- sum.inf(agents[,state])

# Keep record of infection events
infection_reports <- list()

# Keep track of test data through time
test_reports <- list()

# Keep track of extra metrics if store_extra = TRUE  
if(store_extra){
  stay_home <- numeric(length = (t.tot/dt))
  quar_iso  <- numeric(length = (t.tot/dt))
  inf_quar  <- numeric(length = (t.tot/dt))
  mean_FOI  <- numeric(length = (t.tot/dt))
  
  stay_home[1] = quar_iso[1] = inf_quar[1] = mean_FOI[1] = 0
}  

# Determine adaptive testing days
if(adaptive & class(adapt_start) == "Date"){
  adapt_days <- seq(adapt_start, t0+t.tot, by = adapt_freq)
} else if (adaptive){
  adapt_days <- seq(t0+adapt_start, t0+t.tot, by = adapt_freq)
} else {
  NULL
}

# Get populations by geographies for use in adaptive testing site placement
if(adaptive){
  geo_pops <- agents[, .(pop = .N), by = adapt_site_geo]
}

# Update characteristics of initial infections  
# Transition time
agents[state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), tnext:=t_til_nxt(state)]
# State entering once transition time expires
agents[state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), nextstate:=next_state(state, age)]
#Time initial infections occurred
agents[state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), t_infection:=dt]

# Add compliance and sociality metrics, start people with no known contacts
agents[, mask := mask_fx(nrow(agents))] # Probability of wearing a mask
agents[, sociality := social_fx(nrow(agents))] # Sociality metric
agents[, quarantine := 0] # initial quarantine values
agents[, q_prob := 0]
agents[, q_duration := 0]
agents[, q_bta_red:=1]
agents[, contact := 0]
agents[, t_since_contact := 0]
agents[, t_symptoms := 0]
agents[, tested := 0] # Nobody tested to start
agents[, t_since_test := 14.1] # Start everyone off eligible for testing
agents[, test_pos := 0] # Start everyone off with no postive test status
agents[, init_test := 0] # Start everyone off eligible for testing
agents[, t_til_test_note:=0] #nobody tested to start, so nobody waiting on notification
agents[, adapt_site := 0]   # No adaptive testing sites to start

# Run simulation    
for(t in 2:(t.tot/dt)){
  
  # Time step characteristics
  date_now <- t0+t*dt
  agents[, Date:=date_now]
  date_num <- as.numeric(floor(date_now-as.Date("2019-12-31")))
  beta_today <- bta_fx(date_num)
  day_week <- day_of_week_fx[t]
  time_day <- time_of_day_fx[t]
  SiP.active <- ifelse(date_now > SiP.start, 1, 0)
  Reopen <- ifelse(date_now > reopen.start, 1, 0)
  mask.mandate <- ifelse(date_now > mask.start, 1, 0)
  
  if(verbose){cat(as.character(date_now), time_day, "------------------------------------\n\n")}
  
  # Advance transition times, time since infection,time since symptoms started, other illness symptoms, quarantine duration, and test notification times
  agents[state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), tnext:=tnext-dt]
  agents[state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), t_infection:=t_infection+dt]
  
  agents[state %in% c("Im", "Imh", "Ih"), t_symptoms:=t_symptoms+dt]
  
  agents[tested == 0, t_since_test:=t_since_test+dt]
  agents[q_duration > 0, q_duration:=q_duration-dt]
  
  agents[t_since_contact > 0, t_since_contact:=t_since_contact+dt]
  
  agents[init_test == 1, t_til_test_note:=t_til_test_note-dt]
  
  # Advance expired states to next state, determine new nextstate and time til next state, reset expired quarantines, test notifications
  agents[tnext < 0, state:=nextstate]
  agents[tnext < 0 & state %in% c("R", "D"), t_symptoms:=0]
  agents[tnext < 0 & state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), nextstate:=next_state(state, age)]
  agents[tnext < 0 & state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), tnext:=t_til_nxt(state)]
  agents[state %in% c("R", "D") & test_pos == 1, test_pos:=0]
  agents[t_til_test_note < 0 & test_pos == 1, tested:= 1]
  agents[t_til_test_note < 0, init_test:= 0]
  agents[t_til_test_note < 0 & test_pos == 0, q_duration:=0]
  agents[q_duration < 0, q_duration:=0]
  agents[q_duration < 0, q_bta_red:=1]
  agents[q_duration < 0, quarantine:=0]
  agents[t_since_contact > 14, t_since_contact:=0] #Agents stop considering contact relevant after 14 days
  
  if(verbose){ cat("Infections advanced\n") } 
  
  # Implement testing only in the morning for simplicity and speed ---------------
  if(testing & time_day == "M" & date_now >= test_start){
    # If adaptive design, use test reports to select site for new test site placement  
    if(adaptive & as.character(date_now) %in% as.character(adapt_days)){
      
      adapt_sites_add <- adapt_site_fx(test_reports, adapt_freq, n_adapt_sites, adapt_site_geo, geo_pops, 
                                       t0, date_now, adapt_site_test_criteria) # Determine CT receiving new site
      
      agents[, adapt_site:=0] # Reset from old so not just adding new sites perpetually
      
      agents[get(adapt_site_geo) %in% adapt_sites_add, adapt_site:=1] # assign new site
      
      
      if(verbose) {cat("New testing site(s) added in", adapt_sites_add, date_now, "\n")}
    }
    
    # Determine number of public and private tests available
    if(day_week %in% c("U", "S")){
      n_tests <- rpois(1, N*tests_pp_fx(date_num)*tests_wknd)
    } else {
      n_tests <- rpois(1, N*tests_pp_fx(date_num))
    }
    
    n_tests_pub <- rpois(1, n_tests*tests_pub)
    n_tests_pvt <- n_tests - n_tests_pub
    
    if(n_tests > 0){
      
      # Determine agents who are eligible for each type of testing and get their individual testing probability
      eligible_ids <- agents[((t_symptoms > 0 | t_since_contact > 0 | essential == 1 | res_inf > 0) & 
                                t_since_test > 7 & tested == 0 & init_test == 0 & state!= "D") | 
                               (adapt_site == 1 & tested == 0 & init_test == 0 & state!= "D") | 
                               (t_symptoms < t_since_test & tested == 0 & init_test == 0 & state!= "D"), 
                             id]
      
      pvt_eligible_ids <- agents[tested == 0 & init_test ==0 & state!= "D", 
                                 id]
      
      agents[id %in% eligible_ids,
             test_prob:=test_probs_pub_fx(hhincome, race, essential, t_symptoms,  
                                          state, t_since_contact, res_inf,  
                                          adapt_site, adapt_site_mult, n_tests_pub,
                                          symp_mult, race_test_mults, cont_mult, res_mult, nosymp_state_mult, symp_state_mult, hosp_mult)]
      
      agents[id %in% pvt_eligible_ids,
             test_prob:=test_probs_pvt_fx(hhincome, race, essential, t_symptoms,  
                                          state, t_since_contact, res_inf, n_tests_pvt, 
                                          symp_mult, race_test_mults, cont_mult, res_mult, nosymp_state_mult, symp_state_mult, hosp_mult)]
      
      eligible_probs <- agents[id %in% eligible_ids, 
                               test_prob]
      
      pvt_eligible_probs <- agents[id %in% pvt_eligible_ids, 
                                   test_prob]
      
      if((length(eligible_ids)+length(pvt_eligible_ids)) < n_tests){ # Remove t_since_test criteria to create more eleigible agents
        warning(cat("\nOnly",length(eligible_ids)+length(pvt_eligible_ids)), "eligible agents for testing and",
                (n_tests_pub + n_tests_pvt), "tests available\n")
        
        eligible_ids <- agents[((t_symptoms > 0 | t_since_contact > 0 | essential == 1 | res_inf > 0) & 
                                  tested == 0 & init_test == 0 & state!= "D") | 
                                 (adapt_site == 1 & tested == 0 & init_test == 0 & state!= "D"), 
                               id]
        
        agents[id %in% eligible_ids,
               test_prob:=test_probs_pub_fx(income_bracket, race, essential, t_symptoms,  
                                            state, t_since_contact, res_inf, 
                                            adapt_site, adapt_site_mult, n_tests_pub, 
                                            symp_mult, race_test_mults, cont_mult, res_mult, nosymp_state_mult, symp_state_mult, hosp_mult)]
        
        eligible_probs <- agents[id %in% eligible_ids, 
                                 test_prob]
        
        
      }
      
      if(!is.na(n_tests_pub) & n_tests_pub > 0 & n_tests_pub < length(eligible_ids)){
        # Conduct public testing via sampling
        pub_test_ids <- eligible_ids[wrswoR::sample_int_crank(length(eligible_ids),
                                                              n_tests_pub,
                                                              eligible_probs)]
        
        # tag agents who are tested and sample for delay from test to notification
        agents[id %in% pub_test_ids, init_test:=1]
        agents[id %in% pub_test_ids, t_til_test_note:=test_delay_fx(.N)]
        agents[id %in% pub_test_ids & adapt_site == 1, t_til_test_note:=adapt_site_delay_fx(.N)]
        agents[id %in% pub_test_ids & state %in% c("Ip", "Ia", "Im", "Imh", "Ih"), test_pos:=1]
      } else {
        pub_test_ids = NA
        warning(cat("\nPublic testing not conducted due to lack of eligible agents\n"))
      }
      
      if(!is.na(n_tests_pvt) & n_tests_pvt > 0){
        
        # No testing twice
        if(!is.na(pub_test_ids)){
          already_tested <- which(pvt_eligible_ids %in% pub_test_ids)
          pvt_eligible_ids <- pvt_eligible_ids[-already_tested]
          pvt_eligible_probs <- pvt_eligible_probs[-already_tested]
        }  
        
        # Conduct private testing via sampling
        pvt_test_ids <- pvt_eligible_ids[wrswoR::sample_int_crank(length(pvt_eligible_ids),
                                                                  n_tests_pvt,
                                                                  pvt_eligible_probs)]
        
        agents[id %in% pvt_test_ids, init_test:=1]
        agents[id %in% pvt_test_ids, t_til_test_note:=test_delay_fx(.N)]
        agents[id %in% pvt_test_ids & state %in% c("Ip", "Ia", "Im", "Imh", "Ih"), test_pos:=1]
      } else {
        pvt_test_ids = NA
      }
      # Tested agents
      # Test results and reset time since last test for those who were tested
      # agents[id %in% testeds, tested:=test_sens(state, t_infection)]
      test_reports[[as.numeric(date_now-t0)]] <- agents[id %in% c(pub_test_ids, pvt_test_ids),
                                                        c("id", "age", "race", "state", "nextstate",
                                                          "t_infection", "t_symptoms", "t_since_contact", "res_inf", "test_prob", 
                                                          "hhid", "hhincome", "ct",
                                                          "adapt_site", "essential", "Date", "test_pos", "t_til_test_note")]
      
      agents[id %in% c(pub_test_ids, pvt_test_ids), t_since_test:=0]
      agents[, test_prob:=NULL]
      
      if(verbose){ cat(n_tests, "tests conducted,",
                       nrow(agents[id %in% c(pub_test_ids, pvt_test_ids) & test_pos==1,]), "(",
                       nrow(agents[id %in% c(pub_test_ids, pvt_test_ids) & test_pos==1,])/n_tests*100, 
                       "%) positive\n",
                       nrow(agents[id %in% c(pub_test_ids, pvt_test_ids) & test_pos==1 & state %in% c("Ip", "Ia")]), "without symptoms,",
                       nrow(agents[id %in% c(pub_test_ids, pvt_test_ids) & test_pos==1 & state %in% c("Im", "Imh")]), "with mild symptoms, and",
                       nrow(agents[id %in% c(pub_test_ids, pvt_test_ids) & test_pos==1 & state == "Ih"]), "in hospital\n\n")}
      
      rm(list = c("n_tests", "n_tests_pub", "n_tests_pvt",
                  "eligible_ids", "eligible_probs", "pvt_eligible_ids", "pvt_eligible_probs", "pub_test_ids", "pvt_test_ids"))
    }
  }
  
  # Simulate infection --------------------
  # If noone transmitting, skip. Assume agents inactive at night
  if(nrow(agents[state %!in% c("S", "E", "D", "R")])>0 & time_day != "N"){
    
    # Determine locations ---------------
    # Reset residence infection and pct_home then get Get CBG-based stay at home compliance 
    agents[, c("res_inf", "pct_home"):=NULL]
    
    home.today <- stay_home_dt[Date == as.character(date_now),]
    home.today[,ct:=as.numeric(CT)]
    home.today[,pct_home:=pct_home^(1/4)] # Correct stay at home all day percentage for four possible time steps
    home.today[,c("Date", "CT"):=NULL]
    home.mean <- mean(home.today$pct_home)
    agents <- agents[home.today, on = "ct"] # merge to get stay home pct by Ct
    agents[is.na(pct_home), pct_home:=home.mean]
    
    # Find locations of those not deceased, in the hospital, in group quarters, or quarantining  
    #Agents that are quarantined stay at home, those in hospital considered in hospital, not contributing to infection. Those in prisons or nursing homes stay there. Everyone else is "mobile"
    agents[q_duration > 0, location:=hhid]
    agents[state %in% c("S", "E", "Ip", "Ia", "Im", "Imh", "R") & q_duration == 0, 
           mobile:=1]
    
    # Agents that are workers
    agents[occp != 0 & mobile == 1, 
           location:=worker_location(state, 
                                     SiP.active, Reopen, pct_home, time_day, day_week,
                                     age, essential, sociality,
                                     hhid, ct)]
    
    # Agents that are not working
    agents[occp == 0 & mobile == 1,
           location:=other_location(state, 
                                    SiP.active, pct_home, 
                                    age, sociality,   
                                    hhid, ct)]
    
    # Probabilistically move agents in community to other communities based on safegraph movement
    ct_movers <- fmatch(agents[mobile == 1 & location == ct, location],ct_ids) # Get ct indices of movers
    ct_randnum <- dqrng::dqrunif(length(ct_movers)) # Random numbers that interact with cdfs
    ct_moves <- ct_ids[GetCTVisit_cpp(ct_movers, ct_randnum, ct_cdf_list[[date_num]])]
    
    agents[mobile == 1 & location == ct, 
           location:=ct_moves]
    
    if(verbose){
      cat("Locations resolved\n")
      cat(nrow(agents[location==hhid,])/nrow(agents)*100,"% agents at home\n\n")
    }
    
    # Add in visiting agents
    if(visitors){
      visits_today <- visitors_list[[date_num]]
      agents_visit <- visitors_to_agents(visits_today, visitor_mult_testing, visitor_mult_sfgrph)
      
      agents <- rbindlist(list(agents, agents_visit), fill = TRUE)
      
      if(verbose){
        cat("\n", sum(agents_visit$infector, na.rm = T), "infected agents visiting\n\n")
      }
    }  
    
    # Determine who's transmitting and number of individuals by location  
    agents[state %in% c("Ip", "Ia", "Im", "Imh"), infector :=1]
    
    agents[, n_present:=.N, by = location]
    
    # Determine FOI in locations where someone is transmitting
    agents[,wear.mask:=0]
    
    if(mask.mandate == 1){ # Only really care about people transmitting wearing a mask
      agents[infector == 1, wear.mask:=rbinom(.N, 1, mask)]
      agents[tested == 1 & infector == 1, wear.mask:=1] # anyone known tested positive wears mask
    } 
    
    agents[infector == 1, 
           trans_prob := beta_today*(1-mask.red*wear.mask)*(1-test.red*tested)]
    
    agents[infector == 1 & location == hhid, 
           trans_prob := beta_today*bta_hh*q_bta_red*(1-mask.red*wear.mask*tested)*(1-test.red*tested)] # Assume no mask wearing at home unless confirmed positive, reduction in transmission if quarantining based on income (assigned below in qurantine determination)
    
    if(SiP.active == 1){
      agents[infector == 1 & occp != 1, 
             trans_prob := beta_today*bta_work*(1-mask.red*wear.mask)*(1-test.red*tested)]
    } else {
      agents[infector == 1 & essential == 1, 
             trans_prob := beta_today*bta_work*(1-mask.red*wear.mask)*(1-test.red*tested)]
    }
    
    agents[, FOI:=sum(trans_prob*infector/n_present, na.rm = T), by = location]
    
    # Generate infections, update their state, sample for their nextstate and time until reaching it, 
    agents[FOI > 0 & state == "S", infect:=foi_infect(FOI)]
    agents[infect == 1, state:="E"]
    agents[infect == 1, nextstate:=next_state(state, age)]
    agents[infect == 1, tnext:=t_til_nxt(state)]
    
    # document contacts in proportion to infection risk   
    agents[FOI > 0 & state == "S", contact_prob:=FOI*(1+infect*known_contact_prob)]
    agents[contact_prob > 0, contact := rbinom(.N, 1, contact_prob)]
    agents[contact == 1, t_since_contact:=dt]
    
    # Store detailed infection info
    infection_reports[[t-1]] <- agents[infect == 1,]
    if(verbose){cat(nrow(agents[infect == 1,]), "new infections generated,",
                    nrow(agents[state == "Ih",]), "agents currently hospitalized,",
                    nrow(agents[state == "D",]), "cumulative COVID deaths\n\n")}
    
    # Quarantine probabilities
    #Start from scratch
    agents[, q_prob:=0]
    
    # Determine known residential infections
    agents[state == "Im" | state == "Imh" | (infector == 1 & tested == 1), res_infector:=1]
    agents[, res_inf:=sum(res_infector, na.rm = T), by = hhid] 
    
    # New contact
    agents[contact == 1 & t_since_contact == dt & q_duration == 0,
           q_prob:=q_prob+q_prob_contact*(1-0.5*essential)]  
    # Known residential infection
    agents[res_inf > 0 & q_duration == 0,
           q_prob:=q_prob+q_prob_resinf*res_inf*(1-0.5*essential)]
    # Experiencing symptoms plus influence of recent known contact
    agents[t_symptoms > 0 & q_duration == 0,
           q_prob:=(q_prob+q_prob_symptoms*t_symptoms+q_prob_contact*as.numeric(t_since_contact)>0)*(1-0.5*essential)]  
    # Tested positive
    agents[tested == 1 & infector == 1 & q_duration == 0,
           q_prob:=q_prob+q_prob_testpos*(1-0.5*essential)]  
    # Influence of adaptive site
    agents[adapt_site == 1,
           q_prob:=q_prob*q_prob_adapt]
    
    # Quarantine "decisions"
    agents[q_prob > 1, 
           quarantine:=1]
    agents[q_prob < 1 & q_prob > 0, 
           quarantine:=rbinom(.N, 1, q_prob)]
    
    # Assign isolation duration and reduction in transmission if quarantining at home based on income bracket 
    agents[quarantine == 1 & q_duration == 0, 
           q_duration:=q_dur_fx(.N)]
    agents[quarantine == 1, q_bta_red:=(1-1/res_size)**2]
    
    
    if(verbose){
      
      cat(nrow(agents[quarantine == 1,]), "agents entered isolation\n",
          nrow(agents[q_duration > 0,]), "agents currently isolating out of",
          nrow(agents[t_since_contact > 0 | res_inf > 0 | t_symptoms > 0 | (tested == 1 & infector == 1),]), 
          "with symptoms, suspected contact, residential infection, or positive test\n",
          nrow(agents[infector == 1 & q_duration>0,])/nrow(agents[infector == 1,])*100, "% of",
          nrow(agents[infector == 1,]), "infectious agents are quarantining\n\n")
    }
    
    if(store_extra){
      stay_home[t] <- nrow(agents[location==hhid,])/nrow(agents)*100
      quar_iso[t] <- nrow(agents[q_duration > 0,])
      inf_quar[t] <- nrow(agents[infector == 1 & q_duration > 0,])/nrow(agents[infector == 1,])
      mean_FOI[t] <- mean(agents[, FOI], na.rm = T)
    }  
    
    if(store_extra & other_ill){
      other_ill_i[t] <- nrow(agents[other_ill==1,])
    }
    
    # Remove visiting agents
    if(visitors){
      agents <- na.omit(agents, "hhid") 
    }  
    
    
    # Reset infection & location columns and remove temp quarantine objects
    agents[, c("location", "mobile", "infector", "res_infector",
               "contact_prob", "contact", "n_present", "wear.mask",
               "trans_prob", "FOI", "infect"):=NULL]
    
    if(other_ill){
      agents[, c("FOI_other", "other_ill_infect", "trans_prob_other"):=NULL]
    }  
    
  }
  
  epi_curve[t,] <- sum.inf(agents[,state])
  
  gc()  
  # On to the next one  
}




# Plot sim results --------------  
  
# Hospitalizations   
colnames(epi_curve) <- c("S", "E", "Ip", "Ia", "Im", "Imh", "Ih", "D", "R")

epi_curves_H_sum <- as.data.frame(epi_curve) %>% 
  mutate(I = Ip+Ia+Im+Imh+Ih,
         t_sim = (1:(t.tot/dt))*dt,
         Date = t0+t_sim) %>% 
  group_by(Date) %>% 
  summarise(H = mean(Ih)) 

ggplot() +
  geom_col(data = sf_hosp,
           aes(x = Date, y = HOSP_tot),
           col = "darkblue", fill = "blue", alpha = 0.4) +
  geom_line(data = epi_curves_H_sum,
            aes(x = Date, y = H),
            col = "darkred", size = 1.2) +
  theme_bw() +
  theme(legend.position = "none") +
#  ylim(c(0,200)) +
  labs(title = "Hospitalizations in no testing scenario")
  
abm_infections <- bind_rows(infection_reports)
  abm_infections$inf_loc <- NA
  abm_infections$inf_loc[substr(abm_infections$location,1,1) == 6] <- "Comm"
  abm_infections$inf_loc[substr(abm_infections$location,1,1) == 1] <- "Home"
  abm_infections$inf_loc[substr(abm_infections$location,1,1) == 4] <- "Scl"
  abm_infections$inf_loc[substr(abm_infections$location,1,1) == 5] <- "Work"
    table(abm_infections$inf_loc)

abm_infections %>% 
  group_by(Date) %>% 
  summarise(FOI_mean = mean(FOI)) %>% 
  ggplot(aes(x = Date, y = FOI_mean)) +
    geom_line() +
    theme_bw() +
    labs(y = "Mean FOI of infection events",
         title = "MEan FOI resulting in infection over time")

# Other illnesses
if(other_ill){
  plot(x = 1:(t.tot/dt), y = other_ill_i, type = "l",
     xlab = "Time", ylab = "Other Illnesses")
}


#Mean FOI
mean_FOI_df <- data.frame("time" = (1:(t.tot/dt))*dt, 
                          "FOI" = mean_FOI) %>% 
  mutate(Date = as.Date(as.character(t0+time))) %>% 
  filter(mean_FOI > 0) %>% 
  group_by(Date) %>% 
  summarise(mean_day_FOI = mean(FOI))

mean_FOI_df %>% 
  ggplot() +
    geom_line(aes(x = Date, y = mean_day_FOI)) +
    theme_bw() +
    geom_vline(xintercept = SiP.start, col = "blue", lty = 2) + 
    geom_vline(xintercept = mask.start, col = "green", lty = 2) + 
    geom_vline(xintercept = reopen.start, col = "orange", lty = 2) + 
    geom_vline(xintercept = holidays, col = "red", lty = 2) + 
    labs(title = "Mean daily FOI",
         y = "FOI")

plot(mean_FOI[!mean_FOI==0], type = "l",
     xlab = "Time", ylab = "Mean FOI")

# Percent staying home
stay_home_obs_mean <- stay_home_dt %>% 
  group_by(Date) %>% 
  summarise(mean_home_obs = mean(pct_home))

stay_home_gen_mean <- data.frame("time" = (1:(t.tot/dt))*dt, 
                                 "mean_home_gen" = stay_home) %>% 
  mutate(Date = as.Date(as.character(t0+time))) %>% 
  group_by(Date) %>% 
  summarise(mean_home_gen = mean(mean_home_gen))

ggplot() +
  geom_col(data = stay_home_obs_mean,
           aes(x = Date, y = mean_home_obs*100),
           col = "forestgreen", fill = "forestgreen", alpha = 0.4) +
  geom_line(data = stay_home_gen_mean,
            aes(x = Date, y = mean_home_gen),
            col = "purple", size = 1.2) +
  theme_bw() +
  theme(legend.position = "none") +
#  ylim(c(0,200)) +
  labs(title = "Observed stay home vs generated stay home",
       y = "% Completely Home")


# Infecteds that are quarantining
plot(inf_quar, type = "l")


#Test positive percent
tests_sum <- bind_rows(test_reports)
  tests_sum %>% 
    mutate(month = lubridate::month(Date)) %>% 
    group_by(month, state) %>% 
    summarise(n = n()) %>% 
      ggplot() +
      geom_col(aes(x = month, y = log(n+1), fill = state), position = position_dodge()) +
      theme_classic() +
      scale_fill_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")) +
      labs(y = "Log(tests by state+1)")
  
  table(tests_sum$state)

tests_sum_by_date <- 
  tests_sum %>% 
  group_by(Date) %>% 
  summarise(n_tests = n(),
            n_pos = sum(test_pos),
            per_pos = n_pos/n_tests)

ggplot() +
  geom_col(data = sf_test,
           aes(x = Date, y = pct),
           col = "black", fill = "grey50", alpha = 0.6) +
  geom_line(data = tests_sum_by_date,
            aes(x = Date, y = per_pos),
            col = "goldenrod", size = 1.2) +
  xlim(c(t0, t.end)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Percent positive tests from inital ABM sims",
       subtitle = "Comparison to observed",
       y = "Percent Positive Tests")

#Confirmed cases
ggplot() +
  geom_col(data = sf_test,
           aes(x = Date, y = pos),
           col = "black", fill = "grey50", alpha = 0.6) +
  geom_line(data = tests_sum_by_date,
            aes(x = Date, y = n_pos),
            col = "coral", size = 1.2) +
  xlim(c(t0, t.end)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Number positive tests from inital ABM sims",
       subtitle = "Comparison to observed",
       y = "Confirmed cases")

# Confirmed cases to incident cases
inc_curve <- abm_infections %>% 
  group_by(Date) %>% 
  summarise(n_cases  = n())

ggplot() +
  geom_col(data = inc_curve,
           aes(x = Date, y = n_cases),
           col = "black", fill = "grey50", alpha = 0.6) +
  geom_line(data = tests_sum_by_date,
            aes(x = Date, y = n_pos),
            col = "orange", size = 1.2) +
  xlim(c(t0, t.end)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Confirmed cases to incident cases",
       subtitle = "  Confirmed should lag incident, but follow same general pattern",
       y = "Confirmed cases")

# Percent of active cases identified
active_I <- as.data.frame(epi_curve) %>% 
  mutate(t = row_number(),
         Date = t0 + t*dt,
         active_I = Ip+Ia+Im+Imh+Ih) %>% 
  group_by(as.Date(as.character(Date))) %>% 
  summarise(active_I = first(active_I))

colnames(active_I)[1] <- "Date"

ggplot() +
  geom_line(data = active_I,
            aes(x = Date, y = active_I),
            size = 0.2) +
  geom_line(data = tests_sum_by_date,
            aes(x = Date, y = n_pos),
            col = "darkgreen", size = 0.2) +
  theme_bw() +
    labs(title = "Active to known infections over time")
  
active_I %>% left_join(tests_sum_by_date %>% mutate(Date = as.Date(as.character(Date)))) %>% 
  mutate(known_ratio = n_pos/active_I) %>%
  ggplot() +
    geom_line(aes(x = Date, y = known_ratio),
              size = 1.2, col = "darkred") +
    theme_bw() +
    labs(itle = "Proportion of active cases identified")

# Rt estimates from true incidence curve
Cori_R <- EpiEstim::estimate_R(incid = abm_infections %>% 
                                 group_by(as.character(Date)) %>% 
                                 summarise(inc = n()) %>% pull(inc),
                               method = "parametric_si",
                               config = EpiEstim::make_config(mean_si = 5.2,
                                                              std_si = 2.5))

Cori_R$R %>%
  mutate(Date = min(abm_infections$Date)+t_start) %>% 
  ggplot() +
    theme_bw() +
    geom_line(aes(x = Date, y = `Median(R)`),
              size = 1.2, col = "red") +
    geom_ribbon(aes(x = Date, y = `Median(R)`,
                   ymin = `Quantile.0.025(R)`,
                   ymax = `Quantile.0.975(R)`),
                fill = "red", alpha = 0.4) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_vline(xintercept = SiP.start, col = "blue", lty = 2) + 
    geom_vline(xintercept = mask.start, col = "green", lty = 2) + 
    geom_vline(xintercept = reopen.start, col = "orange", lty = 2) + 
    geom_vline(xintercept = holidays, col = "red", lty = 2) + 
    scale_x_date(date_labels = "%m/%d", 
               date_breaks = "14 day") +
    theme(axis.title = element_text(size = 14,
                                    face = "bold"),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1)) +
    labs(x = "Date",
         y = expression(paste(R[e])),
         title = "Cori et al estimate of R through time",
         subtitle = "Estimated from true incidence curve")

# Rt estimates from true incidence curve
Cori_R2 <- EpiEstim::estimate_R(incid = tests_sum_by_date %>% pull(n_pos),
                               method = "parametric_si",
                               config = EpiEstim::make_config(mean_si = 5.2,
                                                              std_si = 2.5))

Cori_R2$R %>%
  mutate(Date = min(tests_sum_by_date$Date)+t_start) %>% 
  ggplot() +
    theme_bw() +
    geom_line(aes(x = Date, y = `Median(R)`),
              size = 1.2, col = "red") +
    geom_ribbon(aes(x = Date, y = `Median(R)`,
                   ymin = `Quantile.0.025(R)`,
                   ymax = `Quantile.0.975(R)`),
                fill = "red", alpha = 0.4) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_vline(xintercept = SiP.start, col = "blue", lty = 2) + 
    geom_vline(xintercept = mask.start, col = "green", lty = 2) + 
    geom_vline(xintercept = reopen.start, col = "orange", lty = 2) + 
    geom_vline(xintercept = holidays, col = "red", lty = 2) + 
    scale_x_date(date_labels = "%m/%d", 
               date_breaks = "14 day") +
    theme(axis.title = element_text(size = 14,
                                    face = "bold"),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1)) +
    labs(x = "Date",
         y = expression(paste(R[e])),
         title = "Cori et al estimate of R through time",
         subtitle = "Estimated from confirmed cases through testing")
