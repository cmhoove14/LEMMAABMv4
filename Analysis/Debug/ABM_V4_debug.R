

# ABM functions and libraries
devtools::load_all() # library(LEMMAABMv4)

input_pars  <- readRDS(here::here("data/processed/input_pars_debug.rds"))
data_inputs <- readRDS(here::here("data/processed/data_inputs_debug.rds"))
vax_phases  <- readRDS(here::here("data/processed/vax65p_scenario.rds"))

bta_base    <- 0.25
bta_hh      <- 1.2
bta_work    <- 1.2
bta_sip_red <- 1/3

visitors <- TRUE 
testing <- "S" 
vaccination <- FALSE 
verbose <- FALSE 
store_extra <- TRUE 
  
set.seed(430)
  
#Function insides below

#Extract data inputs -------------
agents        <- data_inputs$agents
ct_cdf_list   <- data_inputs$ct_cdf_list
ct_ids        <- data_inputs$ct_ids
stay_home_dt  <- data_inputs$stay_home_dt
visitors_list <- data_inputs$visitors_list
tests_avail   <- data_inputs$tests_avail
vax_per_day   <- data_inputs$vax_per_day

#Function to return number of tests on day t converted from tests_avail df
if(testing != "N"){
  tests_pp_fx <- approxfun(tests_avail$date_num,
                           tests_avail$tests_pp)
}

#Function to return number of vaccinations available on day t
if(vaccination){
  vax_fx <- approxfun(vax_per_day$days,
                      vax_per_day$vax)
  
  # Get dates of vaccination phase onsets
  vax_phase_dates <- vax_phases$dates
  
  vax_phases_active <- 0
}

# Extract parameter inputs then store in object for return with sim outputs
unpack_list(input_pars)
input_pars$trans_pars$bta_base   <- bta_base
input_pars$trans_pars$bta_hh     <- bta_hh
input_pars$trans_pars$bta_work   <- bta_work
input_pars$trans_pars$bta_sip_rd <- bta_sip_red

# Convert bta_parameters into function returning baseline transmission probability on each day
if(t0 > SiP.start){
  bta_fx <- function(...) return(bta_base*bta_sip_red)
} else {
  bta_change_df <- data.frame(dates = c(t0, SiP.start, t.end),
                              btas = c(bta_base, bta_base*bta_sip_red, bta_base*bta_sip_red)) %>% 
    padr::pad() %>% 
    mutate(date_num = as.numeric(dates - ref_date))
  
  bta_change_df$btas[is.na(bta_change_df$btas) & bta_change_df$dates < SiP.start] <- bta_base
  bta_change_df$btas[is.na(bta_change_df$btas) & bta_change_df$dates > SiP.start] <- bta_base*bta_sip_red
  
  bta_fx <- approxfun(bta_change_df$date_num,
                      bta_change_df$btas)
}


# Extract Initial conditions ----------------
N <- nrow(agents)

e.seed   <- input_pars$init_states$E0     #Exposed
ip.seed  <- input_pars$init_states$Ip0    #infected pre-symptomatic
ia.seed  <- input_pars$init_states$Ia0    #infected asymptomatic
im.seed  <- input_pars$init_states$Im0    #infected mildly symptomatic
imh.seed <- input_pars$init_states$Imh0   #infected mildly symptomatic, will become severe
ih.seed  <- input_pars$init_states$Ih0    #infected severely symptomatic
d.seed   <- input_pars$init_states$D0     #dead
r.seed   <- input_pars$init_states$R0     #removed
non.s    <- e.seed + ip.seed + ia.seed + im.seed + imh.seed + ih.seed + d.seed + r.seed
s.seed   <- N - non.s


# Initial infection allocated randomly among non-children/non-retirees
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
#epi_curve <- matrix(NA, nrow = t.tot/dt, ncol = 9)
#epi_curve[1,] <- agents[,.N, by = state]$N
epi_curve <- list()
epi_curve[[1]] <- agents[,.N, by = state] -> epicurve ; epicurve[,date:=t0]

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

# Determine adaptive testing days if adaptive testing ------------------
if(testing == "A" & class(adapt_start) == "Date"){
  adapt_days <- seq(adapt_start, t0+t.tot, by = adapt_freq)
} else if (testing == "A"){
  adapt_days <- seq(t0+adapt_start, t0+t.tot, by = adapt_freq)
} else {
  adapt_days <- NA_real_
}

# Get populations by geographies for use in adaptive testing site placement
if(testing == "A"){
  geo_pops <- agents[, .(pop = .N), by = adapt_site_geo]
}

# Update characteristics of initial infections  
# Transition time
agents[state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), tnext:=t_til_nxt(state)]
# State entering once transition time expires
agents[state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), nextstate:=next_state(state, age)]
#Time initial infections occurred
agents[state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), t_infection:=dt]

# Make sure ct is numeric for matching
agents[, ct:=as.numeric(ct)] 

# Add compliance and sociality metrics, start people with no known contacts, etc. -----------------------------
agents[, mask := mask_fx(.N)] # Probability of wearing a mask
agents[, sociality := social_fx(.N)] # Sociality metric
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
agents[, vax_eligible := 0] # Nobody vaccine eligible to start
agents[, t_til_dose2 := 0] # Nobody waiting on dose 2 to start
agents[, vax1 := 0] # Received vaccination dose 1?
agents[, vax2 := 0] # Received vaccination dose 2?

# Progress bar  
if(!verbose){
  pb <- progress_bar$new(
    format = "  Running simulation [:bar] :percent in :elapsed",
    total  = t.tot/dt, 
    clear  = FALSE, 
    width  = 100
  )  
}

# Run simulation     ---------------------
for(t in 2:(t.tot/dt)){
  
  # Time step characteristics
  date_now <- t0+t*dt
  agents[, Date:=date_now]
  date_num <- as.numeric(floor(date_now-ref_date))
  beta_today <- bta_fx(date_num)
  day_week <- day_of_week_fx[t]
  time_day <- time_of_day_fx[t]
  SiP.active <- ifelse(date_now > SiP.start, 1, 0)
  mask.mandate <- ifelse(date_now > mask.start, 1, 0)
  
  if(verbose){cat(as.character(date_now), time_day, "------------------------------------\n\n")}
  
  # Advance transition times, time since infection,time since symptoms started, quarantine duration, test notification times, vaccination 2nd dose delay
  agents[state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), tnext:=tnext-dt]
  agents[state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), t_infection:=t_infection+dt]
  
  agents[state %in% c("Im", "Imh", "Ih"), t_symptoms:=t_symptoms+dt]
  
  agents[tested == 0, t_since_test:=t_since_test+dt]
  agents[q_duration > 0, q_duration:=q_duration-dt]
  
  agents[t_since_contact > 0, t_since_contact:=t_since_contact+dt]
  
  agents[init_test == 1, t_til_test_note:=t_til_test_note-dt]
  
  agents[vax1 == 1 & vax2 == 0, t_til_dose2 := t_til_dose2-dt]
  
  # Advance expired states to next state, determine new nextstate and time til next state, reset expired quarantines, test notifications, vaccine second dose
  agents[tnext < 0, state:=nextstate]
  agents[tnext < 0 & state %in% c("R", "D"), t_symptoms:=0]
  agents[tnext < 0 & state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), nextstate:=next_state(state, age)]
  agents[tnext < 0 & state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), tnext:=t_til_nxt(state)]
  agents[state %in% c("R", "D") & test_pos == 1, test_pos:=0]
  agents[t_til_test_note < 0 & test_pos == 1, tested:= 1]
  agents[t_til_test_note < 0, init_test:= 0]
  agents[t_til_test_note < 0 & test_pos == 0, q_duration:=0] # Exit quarantine if test negative
  agents[q_duration < 0, q_duration:=0]
  agents[q_duration < 0, q_bta_red:=1]
  agents[t_since_contact > 14, t_since_contact:=0] #Agents stop considering contact relevant after 14 days
  agents[t_til_dose2 < 0, vax2 := 1]
  
  if(verbose){ cat("Infections advanced\n") } 
  
  # Implement testing only in the morning for simplicity and speed ---------------
  if(testing != "N" & time_day == "M" & date_now >= test_start){
    # If adaptive design, use test reports to select site for new test site placement  
    if(testing == "A" & as.character(date_now) %in% as.character(adapt_days)){
      
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
    
    if(n_tests > 0){
      
      # Determine agents who are eligible for testing and get their individual testing probability
      eligible_agents <- agents[t_symptoms < t_since_test & tested == 0 & init_test == 0 & state!= "D", ]
      
      eligible_agents[, test_prob:=test_probs_fx(income            = hhincome, 
                                                 income_mult       = income_mult,
                                                 hpi               = hpi_quartile, 
                                                 essential         = essential, 
                                                 essential_prob    = essential_prob,
                                                 t_symptoms        = t_symptoms,  
                                                 state             = state, 
                                                 t_since_contact   = t_since_contact, 
                                                 res_inf           = res_inf,  
                                                 adapt_site        = adapt_site, 
                                                 adapt_site_mult   = adapt_site_mult, 
                                                 tests_avail       = n_tests,
                                                 case_find_mult    = case_finding_mult,
                                                 symp_mult         = symp_mult, 
                                                 hpi_mult          = hpi_mult, 
                                                 cont_mult         = cont_mult, 
                                                 res_mult          = res_mult, 
                                                 nosymp_state_mult = nosymp_state_mult, 
                                                 symp_state_mult   = symp_state_mult, 
                                                 hosp_mult         = hosp_mult)]
      
      eligible_ids <- eligible_agents[, id]
      eligible_probs <- eligible_agents[, test_prob]
      
      if(length(eligible_ids) < n_tests){ # Remove t_since_test criteria to create more eleigible agents
        warning(cat("\nOnly",length(eligible_ids), "eligible agents for testing and",
                    (n_tests_pub + n_tests_pvt), "tests available\n"))
        
        eligible_agents <- agents[init_test == 0 & state!= "D", ]
        
        eligible_agents[, test_prob:=test_probs_fx(income            = hhincome, 
                                                   income_mult       = income_mult,
                                                   hpi               = hpi_quartile, 
                                                   essential         = essential, 
                                                   essential_prob    = essential_prob,
                                                   t_symptoms        = t_symptoms,  
                                                   state             = state, 
                                                   t_since_contact   = t_since_contact, 
                                                   res_inf           = res_inf,  
                                                   adapt_site        = adapt_site, 
                                                   adapt_site_mult   = adapt_site_mult, 
                                                   tests_avail       = n_tests,
                                                   case_find_mult    = case_find_mult,
                                                   symp_mult         = symp_mult, 
                                                   hpi_mult          = hpi_mult, 
                                                   cont_mult         = cont_mult, 
                                                   res_mult          = res_mult, 
                                                   nosymp_state_mult = nosymp_state_mult, 
                                                   symp_state_mult   = symp_state_mult, 
                                                   hosp_mult         = hosp_mult)]
        eligible_ids <- eligible_agents[, id]
        eligible_probs <- eligible_agents[, test_prob]
        
      }
      
      if(!is.na(n_tests) & n_tests > 0 & n_tests < length(eligible_ids)){
        # Conduct testing via weighted random sampling based on test probabilities
        test_ids <- eligible_ids[wrswoR::sample_int_crank(length(eligible_ids),
                                                          n_tests,
                                                          eligible_probs)]
        
        # tag agents who are tested and sample for delay from test to notification
        agents[id %in% test_ids, init_test:=1]
        agents[init_test==1, t_til_test_note:=test_delay_fx(.N)]
        agents[init_test==1 & adapt_site == 1, t_til_test_note:=adapt_site_delay_fx(.N)]
        agents[init_test==1 & state %in% c("Ip", "Ia", "Im", "Imh", "Ih"), test_pos:=1]
      } else {
        test_ids = NA
        warning(cat("\nPublic testing not conducted due to lack of eligible agents\n"))
      }
      # Tested agents
      # Test results and reset time since last test for those who were tested
      # agents[id %in% testeds, tested:=test_sens(state, t_infection)]
      test_reports[[as.numeric(date_now-t0)]] <- agents[init_test==1,
                                                        c("id", "age", "sex", "race", "occp", "essential", "work", "state", "nextstate",
                                                          "t_infection", "t_symptoms", "t_since_contact", "res_inf", 
                                                          "hhid", "hhincome", "ct", "hpi_quartile", 
                                                          "adapt_site", "Date", "test_pos", "t_til_test_note")]
      
      agents[id %in% test_ids, t_since_test:=0]
      
      if(verbose){ cat(n_tests, "tests conducted,",
                       nrow(agents[id %in% test_ids & test_pos==1,]), "(",
                       nrow(agents[id %in% test_ids & test_pos==1,])/n_tests*100, 
                       "%) positive\n",
                       nrow(agents[id %in% test_ids & test_pos==1 & state %in% c("Ip", "Ia")]), "without symptoms,",
                       nrow(agents[id %in% test_ids & test_pos==1 & state %in% c("Im", "Imh")]), "with mild symptoms, and",
                       nrow(agents[id %in% test_ids & test_pos==1 & state == "Ih"]), "in hospital\n\n")}
      
      rm(list = c("n_tests", "eligible_agents", "eligible_ids", "eligible_probs", "test_ids"))
    }
  }
  
  # Implement vaccination only in the morning for simplicity and speed -----------------
  if(vaccination & time_day == "M" & date_now >= vax_start){
    vax_avail <- vax_fx(date_num)
    
    # IF new phase started, add new eligibles, else skip over and go straight to vaccination
    if(sum(vax_phase_dates <= date_now) > vax_phases_active){
      #Update vaccination phases
      vax_phases_active <- sum(vax_phase_dates <= date_now)
      active_phases <- vax_phase_dates[1:vax_phases_active]
      
      # Identify and label eligible agents by phase 
      for(v in 1:length(active_phases)){
        vax_eligible_ages  <- vax_phases$ages[[v]]
        vax_eligible_occps <- vax_phases$occps[[v]]
        vax_phase_type     <- vax_phases$type[[v]]
        
        #If adapive vaccination targeting essential workers  
        if(vax_phase_type == "A"){
          
          # Examine past month's testing data to determine where to place site
          start <- as.numeric(date_now-t0)-30
          end <- as.numeric(date_now-t0)
          
          test_data <- rbindlist(lapply(start:end, function(d) test_reports[[d]]))
          test_data_sum <- test_data[, 
                                     .(n_tests = .N, n_pos = sum(test_pos), per_pos = sum(test_pos)/.N),
                                     by = ct]
          
          # Make agents in 20 CTs (basically 10% of all cts) with highest test percent positive eligible
          vax_eligible_cts <- test_data_sum$ct[order(-test_data_sum$per_pos)][1:20]
          
          vax_phases$cts[[v]] <- vax_eligible_cts
        } else {
          
          vax_eligible_cts <- vax_phases$cts[[v]]
          
        }
        
        agents[age %in% vax_eligible_ages & occp %in% vax_eligible_occps & ct %in% vax_eligible_cts & vax1 == 0,
               vax_eligible := 1]
      }
      
    } 
    # randomly sample from available agents to vaccinate
    vax_eligible_ids <- agents[vax_eligible == 1, id]
    vax_ids <- vax_eligible_ids[wrswoR::sample_int_crank(length(vax_eligible_ids),
                                                         vax_avail,
                                                         rep(1, length(vax_eligible_ids)))]
    
    # tag agents who are chosen for vaccination and assign delay to second dose
    agents[id %in% vax_ids, vax1:=1]
    agents[id %in% vax_ids, t_til_dose2:=vax2_delay]
    
    if(verbose){ cat(vax_avail,"Vaccinations administered\n") } 
  }
  
  # Simulate infection --------------------
  # If noone transmitting, skip. Assume agents inactive at night
  if(nrow(agents[state %in% c("Ip", "Ia", "Im", "Imh", "Ih")])>0 & time_day != "N"){
    
    # Determine locations ---------------
    # Reset residence infection and pct_home then get Get CT-based stay at home compliance 
    
    if(time_day == "M" | t==2){ # Update daily stay-at home if new day
      agents[, c("res_inf", "pct_home"):=NULL]
      
      home.today <- stay_home_dt[Date == as.character(date_now),]
      home.today[,ct:=as.numeric(CT)]
      home.today[,pct_home:=pct_home^(1/4)] # Correct stay at home all day percentage for four possible time steps
      home.today[,c("Date", "CT"):=NULL]
      home.mean <- mean(home.today$pct_home)
      agents <- agents[home.today, on = "ct"] # merge to get stay home pct by Ct
      agents[is.na(pct_home), pct_home:=home.mean]
    }
    
    # Find locations of those not deceased, in the hospital, in group quarters, or quarantining  
    #Agents that are quarantined stay at home, those in hospital considered in hospital, not contributing to infection. Those in prisons or nursing homes stay there. Everyone else is "mobile"
    agents[q_duration > 0, location:=hhid]
    agents[state != "Ih" & state != "D" & q_duration == 0, 
           mobile:=1]
    
    # Agents that are workers
    agents[occp != 0 & mobile == 1, 
           location:=worker_location(state, 
                                     SiP.active, pct_home, time_day, day_week,
                                     age, essential, sociality,
                                     hhid, work, ct)]
    
    # Agents that are not workers
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
           trans_prob := beta_today*(1-mask_red*wear.mask)*(1-test.red*tested)]
    
    agents[infector == 1 & location == hhid, 
           trans_prob := beta_today*bta_hh*q_bta_red*(1-mask_red*wear.mask*tested)*(1-test.red*tested)] # Assume no mask wearing at home unless confirmed positive, reduction in transmission if quarantining based on income (assigned below in qurantine determination)
    
    agents[infector == 1 & location == work, 
           trans_prob := beta_today*bta_work*(1-mask_red*wear.mask*tested)*(1-test.red*tested)] 
    
    # Get FOI for all agents  
    agents[, FOIi:=0]
    agents[infector==1, FOIi:=trans_prob*infector/n_present]
    agents[, FOI:=sum(FOIi), by = location]
    
    # Reduce probability of infection for vaccinated agents
    if(vaccination & date_now >= vax_start){
      agents[vax1 == 1 & vax2 == 0, 
             FOI:=FOI*(1-vax1_bta_red)]
      
      agents[vax2 == 1, 
             FOI:=FOI*(1-vax2_bta_red)]
    }
    
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
    agents[, res_infector := 0]
    agents[state == "Im" | state == "Imh" | (infector == 1 & tested == 1), res_infector:=1]
    agents[res_infector == 1, res_inf:=.N, by=hhid] 
    
    # New contact
    agents[contact == 1 & t_since_contact == dt & q_duration == 0,
           q_prob:=q_prob+q_prob_contact]  
    # Known residential infection
    agents[res_inf > 0 & q_duration == 0,
           q_prob:=q_prob+q_prob_resinf*res_inf]
    # Experiencing symptoms 
    agents[t_symptoms > 0 & q_duration == 0,
           q_prob:=(q_prob+q_prob_symptoms*t_symptoms)]  
    # Tested positive
    agents[tested == 1 & infector == 1 & q_duration == 0,
           q_prob:=q_prob+q_prob_testpos]  
    #Essential workers
    agents[essential == 1,
           q_prob:=q_prob*(1-q_prob_essential)]
    # Influence of adaptive site
    if(testing == "A"){
      agents[adapt_site == 1,
             q_prob:=q_prob*q_prob_adapt]
    }
    
    # Quarantine "decisions"
    agents[q_prob > 1, 
           choose_quar:=1]
    agents[q_prob < 1 & q_prob > 0, 
           choose_quar:=rbinom(.N, 1, q_prob)]
    
    # Assign isolation duration and reduction in transmission if quarantining at home based on income bracket 
    agents[choose_quar == 1 & q_duration == 0, 
           q_duration:=q_dur_fx(.N)]
    agents[choose_quar == 1, q_bta_red:=(1-1/hhsize)**2]
    
    
    if(verbose){
      
      cat(nrow(agents[choose_quar == 1,]), "agents entered isolation\n",
          nrow(agents[q_duration > 0,]), "agents currently isolating\n",
          nrow(agents[infector == 1 & q_duration>0,])/nrow(agents[infector == 1,])*100, "% of",
          nrow(agents[infector == 1,]), "infectious agents are quarantining\n\n")
    }
    
    if(store_extra){
      stay_home[t] <- nrow(agents[location==hhid,])/nrow(agents)*100
      quar_iso[t] <- nrow(agents[q_duration > 0,])
      inf_quar[t] <- nrow(agents[infector == 1 & q_duration > 0,])/nrow(agents[infector == 1,])
      mean_FOI[t] <- mean(agents[, FOI], na.rm = T)
    }  
    
    # Remove visiting agents
    if(visitors){
      agents <- agents[!is.na(hhid),]
    }  
    
    
    # Reset infection & location columns and remove temp quarantine objects
    agents[, c("location", "mobile", "infector", "res_infector",
               "contact_prob", "contact", "n_present", "wear.mask",
               "trans_prob", "FOIi", "FOI", "infect", "choose_quar"):=NULL]
    
  }
  
  #epi_curve[t,] <- agents[,.N, by = state]$N
  if(time_day == "M"){
    epi_curve[[t]] <- agents[,.N, by = state] -> epicurve ; epicurve[,date:=date_now]
  }
  
  gc()  
  
  #Advance progress bar
  if(!verbose){
    pb$tick()
    Sys.sleep(1/100)
  }    
  
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
