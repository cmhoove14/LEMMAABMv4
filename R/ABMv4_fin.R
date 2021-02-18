#' @title ABM V4 with testing 
#' 
#' @description Function to run agent based model
#' 
#' @param data_inputs List of data inputs for model including, see `details`. Compiled in `01-Prep-Data-Inputs.R`
#' @param input_pars LIST of LISTS containing parameters, dates, functions to support simulation, see `details`.  Compiled in `02-Prep-Par-Inputs.R`
#' @param vax_phases list with list describing vaccine phases and their eligibility criteria, see `details.  Compiled in `03-Prep-Vax-Phases.R`
#' 
#' @param visitors TRUE/FALSE of whether to model outside visitors
#' @param testing character in N, S, or A for whether to conduct no testing ("N"), standard testing ("S"), or adaptive ("A") testing
#' @param vaccination TRUE/FALSE of whether to model vaccination using inputs in `input_pars` and `vax_phases`
#' @param verbose TRUE/FALSE should detailed info at each time step be printed?
#' @param store_extra TRUE/FALSE should extra metrics including % staying home and % isolating be stored and returned? Good for debugging
#' @param initial TRUE/FALSE of whether initial run or run picking up from where another left off. If `TRUE`, will initialize infections from `input_pars` and commence from beginning of pandemic. If `FALSE`, `data_inputs$agents` should contain an agents data.table produced from previous model run and input_pars$time_pars should encode parameters for when previous simulation ended and this one began
#' @param output_path file path relative to `here::here` root to save outputs
#' 
#' @details 
#' `data_inputs` is a list of lists containing `agents_dt`: data.table of agents with necessary characteristics including: "hhsize" "hhincome"   "sex"        "age"        "occp"       "race"       "hhid"       "id" "ct"  "essential"  "work_ct"    "work"    "state"    "nextstate"  "tnext"  "t_symptoms" ;  `ct_cdf_list`: list of matrices of dim cts x cts in which entries represent probabilities of moving from CT i to CT j on day t where t subsets the list to corresponding day (e.g. ct_cdf_list[[t]] = ct x ct matrix). Used in `GetCTVisit`/`GetCTVisit_cpp` function ; `ct_ids` vector relating row numbers of `ct_cdf_list` to actual ct codes ; `stay_home_dt` data table with columns `Date`, `CT`, and `pct_home` (E.g. derived from safegraph data) to use for social distancing/stay at home compliance ; `visitors_list` list of data frames/tables with columns corresponding to census tract, number of external visitors to that ct, county from which they visited, and population and infection characteristics of that county. column inf_prob represents incidence in that county (new cases identified per population) and is used as probability a visiting agent is infectious along with `visitor_mult_testing` to correct for ratio of true incidence to incidence from testing data and `visitor_mult_sfgrph` to correct for ratio of safegraph devices tracked to total population ;  `test_avail` data frame with columns date_num and tests_pp corresponding to number of tests availabe per person on day date_num. Used to generate function returning the number of tests available per agent on day t since test start. 
#' 
#' `input_pars` is a list of lists corresponding to parameters that shouldn't need to be edited frequently e.g. for model calibration or simulations. It includes a LIST `trans_pars` containing `bta_base` baseline transmission probability `bta_hh` household transmission probability multiplier `bta_work` work transmission probability multiplier `bta_sip_red` reduction in transmission probability following SiP to account for microbehaviors such as 6 feet apart, less time indoors, etc.Another LIST `time_pars` containing: `t0`: start date of simulation ;  `t.tot` total time to run simulation (in days) ; `dt` time step (defaults to 4/24, needs editing if otherwise) ; `day_of_week_fx` function which takes t returns character of day of week (U,M,T,W,R,F,S) ; `SiP.start` Date that shelter in place started ; `mask.start` Date that mask mandate was introduced. Another LIST `init_states` containing : `E0` number of initial exposed agents ; `Ip0` number of initial pre-symptomatic infections agents ; `Ia0` number of initial asymptomatic infectious agents ; `Im0` number of initial mildy symptomatic agents ; `Imh0` number of initial mildly symptomatic, will become severely symptomatic agents ; `Ih0` number of initial hospitalized agents ; `R0` number of initial recovered agents ; `D0` number of initial diceased agents. Another LIST `quar_pars` containing: `q_prob_contact` probability an agent will quarantine because of known contact with infectious agent ; `q_prob_resinf` probability an agent will quarantine because of shared residence with infectious agent ; `q_prob_symptoms` probability an agent will quarantine because of symptoms on a per day basis: e.g. one full day of symptoms = q_prob_symptoms probability of isolating. Note that agents will consider this 4 times per day, so probability should adjust for multiple opportunities ; `q_prob_testpos` probability an agent will quarantine because of positive test result ; `q_prob_adapt` probability multiplier agent will quarantine if "exposed" to an adaptive testing site ; `q_dur_fx` function to generate random quarantine durations ; `known_contact_prob` proportional increase in probability that a contact with an infectious individual is known. 0 implies known contact occurs at same rate as FOI, implying very low probability that an infectious contact will be known 1 implies probability is double FOI. Since FOIs will be quite small, larger multipliers are suggested. LIST `test_pars` containing: `test_start` Date at which testing begins ; `test_delay_fx` function to generate time delay from test to notification ; `tests_wknd` percent of weekday tests available on weekends ; `test_probs_fx` function which returns testing probability for individuals to get tested ; `cont_mult` multiplier for test probability for agents with suspeceted contact ; `symp_mult` multiplier for test probability for time exeriencing symptoms; `res_mult` multiplier for test probability for agents with known infection in their residence ; `nosymp_state_mult` multiplier for test probability for agents who are infected but not shopwing symptoms (Ia or Ip) ; `symp_state_mult` multiplier for test probability for agents who are infected andshowing symptoms (Im or Imh) ; `hosp_mult` multiplier for test probability for agents who are infected and hospitalized ; `test.red` reduction in transmission probability if agents has tested positive. LIST `other_pars` containing `visitor_mult_testing` multiplier to adjust probability visiting agent is infected based on ratio of true incidence to incidence identified via testing ; `visitor_mult_sfgrph` multiplier to adjust total number of visitors from safegraph number of visitors based on ratio of tracked safegraph devices to total movement of people ; `mask_fx` a function to return individual agent probabilities of wearing a mask outside the household ; `mask_red` reduction in transmission probability if agent wears mask ; `social_fx` function to generate agent sociality metrics. LIST `adapt_pars` containing: `adapt_start` time point at which to start adaptive testing ; `adapt_freq` frequency with which to check test reports and generate new test site ; `adapt_site_fx` function to determine when and where to place adaptive testing sites ; `adapt_site_geo` geography to base adaptive sites on only option is `ct` for now ; `n_adapt_sites` how many adaptive sites available to place ; `adapt_site_test_criteria` character either `n_pos`or `pct_pos`for whteher targeted sites should be added based on census tract with highest number positive tests or highest percent positive tests ; `adapt_site_mult` increase in testing probability if test site is placed in residence ct ; `adapt_site_delay_fx` function to generate time delay for test conducted to disclosure for agents in cts with  adaptive sites.
#' `vax_phases` should contain three lists each of the same length: `dates` containing start dates of new vaccination eligibility phases, `ages` with entries containing ages of eligilibility for vaccination, and `occps` containing numerical codes for occupations eligible for vaccination. All three lists should be same length and are additive, i.e. agents in phase 1 still unvaccinated in phase two will remain eligible for phase 2. agents must also meet both age and occp criteria for eligibility. each entry should define a group eligible for vaccination, so if two groups becoming eligible at same time, separate entries required by replicating date in `dates` and defining separate criteria in `ages` and `occps`. See `Analysis/0-Prep-Agents.R` for occp codes.
#' 
#' @return list with two objects: an epi curve with counts of agents in each state through time and a dataframe with infection reports for every new infection (e.g. entry created whenever agent transitions from S to E)
#' @export

covid_abm_v4 <- function(data_inputs, input_pars, vax_phases,
                         visitors, testing, vaccination,
                         verbose, store_extra, initial, output_path){
  # Stop messages for invalid inputs -----------------
  if(!testing %in% c("N", "S", "A")){
    stop("Invalid testing scenario, testing must be 'S', 'A', or 'N'")
  }
  
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
  
  # Extract Initial conditions ----------------
  N <- nrow(agents)
  
  if(initial){
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
    set.seed(430)
    init.Es   <- sample(agents[!age %in% c(5,15,75,85), id], e.seed)   
    init.Ips  <- sample(agents[!age %in% c(5,15,75,85), id], ip.seed)   
    init.Ias  <- sample(agents[!age %in% c(5,15,75,85), id], ia.seed)   
    init.Ims  <- sample(agents[!age %in% c(5,15,75,85), id], im.seed)   
    init.Imhs <- sample(agents[!age %in% c(5,15,75,85), id], imh.seed)   
    init.Ihs  <- sample(agents[!age %in% c(5,15,75,85), id], ih.seed)   
    init.Ds   <- sample(agents[!age %in% c(5,15,75,85), id], d.seed)   
    init.Rs   <- sample(agents[!age %in% c(5,15,75,85), id], r.seed)   
    
    agents[id %in% init.Es, state:="E"]
    agents[id %in% init.Ips, state:="Ip"]
    agents[id %in% init.Ias, state:="Ia"]
    agents[id %in% init.Ims, state:="Im"]
    agents[id %in% init.Imhs, state:="Imh"]
    agents[id %in% init.Ihs, state:="Ih"]
    agents[id %in% init.Ds, state:="D"]
    agents[id %in% init.Rs, state:="R"]
    
  }  
  # Keep track of everyone's infection status through time   
  #epi_curve <- matrix(NA, nrow = t.tot/dt, ncol = 9)
  #epi_curve[1,] <- agents[,.N, by = state]$N
  epi_curve <- list()
  epi_curve[[1]] <- agents[,.N, by = state] -> epicurve ; epicurve[,date:=t0]
  
  # Keep track of test data through time
  test_reports <- list()
  
  # Keep track of extra metrics if store_extra = TRUE  
  if(store_extra){
    stay_home <- numeric(length = (t.tot/dt))
    quar_iso  <- numeric(length = (t.tot/dt))
    inf_quar  <- numeric(length = (t.tot/dt))
    mean_FOI  <- list()
    
    stay_home[1] = quar_iso[1] = inf_quar[1] = 0
    
    mean_FOI[[1]] <- data.table(loc_type = c("H", "C", "W"),
                                V1 = c(0,0,0),
                                Date = t0)
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
  
  if(initial){
    # Update characteristics of initial infections  
    # Transition time
    agents[state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), tnext:=t_til_nxt(state)]
    # State entering once transition time expires
    mort_mult_init <- 1
    agents[state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), nextstate:=next_state(state, age, sex, mort_mult_init)]
    #Time initial infections occurred
    agents[state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), t_infection:=dt]
    
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
    agents[, t_death := NA_character_]  # Time of death record
    agents[, t_hosp := NA_character_]  # Time of hospitalization record
    agents[, adapt_site := 0]   # No adaptive testing sites to start
    agents[, vax_eligible := 0] # Nobody vaccine eligible to start
    agents[, t_til_dose2 := 0] # Nobody waiting on dose 2 to start
    agents[, vax1 := 0] # Received vaccination dose 1?
    agents[, vax2 := 0] # Received vaccination dose 2?
  }  
  
  # Make sure ct is numeric for matching
  agents[, ct:=as.numeric(ct)] 
  
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
  t_start <- ifelse(initial, 2, 1)
  
  for(t in t_start:(t.tot/dt)){
    
    # Time step characteristics
    date_now      <- t0+t*dt
    agents[, Date:=date_now]
    date_num      <- as.numeric(floor(date_now-ref_date))
    day_week      <- day_of_week_fx[t]
    time_day      <- time_of_day_fx[t]
    SiP.active    <- ifelse(date_now > SiP.start, 1, 0)
    mask.mandate  <- ifelse(date_now > mask.start, 1, 0)
    mort_mult_now <- ifelse(date_now > mort_red_date, mort_mult, 1)
    beta_today    <- ifelse(date_now > SiP.start, bta_base*bta_sip_red, bta_base)
    
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
    agents[tnext < 0 & state == "D", t_death := date_now]  # Record death events
    agents[tnext < 0 & state == "Ih", t_hosp := date_now]  # Record hospitalization events
    agents[tnext < 0 & state %in% c("R", "D"), t_symptoms:=0]
    agents[tnext < 0 & state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), nextstate:=next_state(state, age, sex, mort_mult_now)]
    agents[tnext < 0 & state %in% c("E", "Ip", "Ia", "Im", "Imh", "Ih"), tnext:=t_til_nxt(state)]
    agents[tnext < 0 & state == "D", tested := 1]          # Assume agents are tested to confirm infection at death if not confirmed positive previously
    agents[tnext < 0 & state == "D", tnext := 0]           # Reset state progression so only recording deaths once
    agents[state %in% c("R", "D") & test_pos == 1, test_pos:=0]
    agents[t_til_test_note < 0 & test_pos == 1, tested:= 1]
    agents[t_til_test_note < 0, init_test:= 0]
    agents[t_til_test_note < 0 & test_pos == 0, q_duration:=0] # Exit quarantine if test negative
    agents[q_duration < 0, q_duration:=0]
    agents[q_duration < 0, q_bta_red:=1]
    agents[t_since_contact > 7, t_since_contact:=0] #Agents stop considering contact relevant after 7 days
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
                                                   hpi_hc            = hpi_healthcareaccess, 
                                                   hpi_mult          = hpi_mult, 
                                                   essential         = essential, 
                                                   essential_prob    = essential_prob,
                                                   t_symptoms        = t_symptoms,  
                                                   symp_mult         = symp_mult, 
                                                   state             = state, 
                                                   nosymp_state_mult = nosymp_state_mult, 
                                                   symp_state_mult   = symp_state_mult, 
                                                   t_since_contact   = t_since_contact, 
                                                   cont_mult         = cont_mult, 
                                                   res_inf           = res_inf,  
                                                   res_mult          = res_mult, 
                                                   adapt_site        = adapt_site, 
                                                   adapt_site_mult   = adapt_site_mult, 
                                                   tests_avail       = n_tests,
                                                   case_find_mult    = case_finding_mult,
                                                   hosp_mult         = hosp_mult)]
        
        eligible_ids <- eligible_agents[, id]
        eligible_probs <- eligible_agents[, test_prob]
        
        if(length(eligible_ids) < n_tests){ # Remove t_since_test criteria to create more eligible agents
          warning(cat("\nOnly",length(eligible_ids), "eligible agents for testing and",
                      n_tests, "tests available\n"))
          
          eligible_agents <- agents[init_test == 0 & state!= "D", ]
          
          eligible_agents[, test_prob:=test_probs_fx(income            = hhincome, 
                                                     income_mult       = income_mult,
                                                     hpi_hc            = hpi_healthcareaccess, 
                                                     hpi_mult          = hpi_mult, 
                                                     essential         = essential, 
                                                     essential_prob    = essential_prob,
                                                     t_symptoms        = t_symptoms,  
                                                     symp_mult         = symp_mult, 
                                                     state             = state, 
                                                     nosymp_state_mult = nosymp_state_mult, 
                                                     symp_state_mult   = symp_state_mult, 
                                                     t_since_contact   = t_since_contact, 
                                                     cont_mult         = cont_mult, 
                                                     res_inf           = res_inf,  
                                                     res_mult          = res_mult, 
                                                     adapt_site        = adapt_site, 
                                                     adapt_site_mult   = adapt_site_mult, 
                                                     tests_avail       = n_tests,
                                                     case_find_mult    = case_finding_mult,
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
        test_reports[[as.numeric(date_now-ref_date)]] <- agents[id %in% test_ids,
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
          
          #If adaptive vaccination 
          if(vax_phase_type == "A"){
            
            adapt_type = vax_phases$metric[[v]]
            
            if(adapt_type == "CASE"){
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
              
            } else if(adapt_type == "HOSP"){
              # Examine past 2 month's hospitalizations data to determine where to place site
              start <- as.Date(date_now-60)
              end <- as.Date(date_now)
              
              hosp_data <- agents[t_hosp <= end & t_hosp >= start,]
              hosp_data_sum <- hosp_data[, .(n_hosp = .N), by = ct]
              
              # Make agents in 20 CTs (basically 10% of all cts) with highest test percent positive eligible
              vax_eligible_cts <- hosp_data_sum$ct[order(-hosp_data_sum$n_hosp)][1:20]
              
              vax_phases$cts[[v]] <- vax_eligible_cts
              
            } else {
              stop("Vaccination phase", v, "is adaptive, but must be either CASE or HOSP")
            }
            
          } else {
            
            vax_eligible_cts <- vax_phases$cts[[v]]
            
          }
          
          agents[age %in% vax_eligible_ages & occp %in% vax_eligible_occps & ct %in% vax_eligible_cts & vax1 == 0,
                 vax_eligible := 1]
        }
        
      } 
      # randomly sample from available agents to vaccinate
      vax_eligible_ids <- agents[vax_eligible == 1 & vax1 == 0, id]
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
      
      if(time_day == "M" | t %in% c(1:2)){ # Update daily stay-at home if new day
        agents[, "pct_home":=NULL]
        
        home.today <- stay_home_dt[Date == as.character(date_now),]
        home.today[,ct:=as.numeric(CT)]
        home.today[,pct_home:=pct_home^(1/4)] # Correct stay at home all day percentage for four possible time steps
        home.today[,c("Date", "CT"):=NULL]
        home.mean <- mean(home.today$pct_home)
        agents <- agents[home.today, on = "ct"] # merge to get stay home pct by Ct
        agents[is.na(pct_home), pct_home:=home.mean]
      }
      
      # Find locations of those not deceased, in the hospital, or quarantining  
      #Agents that are quarantined stay at home, those in hospital considered in hospital, not contributing to infection. Everyone else is "mobile"
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
        agents_visit[, c("hhid", "work"):=0]
        
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
      
      agents[infector == 1 & location != hhid & location != work, 
             trans_prob := beta_today*(1-mask_red*wear.mask)*(1-test.red*tested)]
      
      # Assume no mask wearing at home unless confirmed positive, reduction in transmission if quarantining based on income (assigned below in qurantine determination)
      agents[infector == 1 & location == hhid, 
             trans_prob := beta_today*bta_hh*q_bta_red*(1-mask_red*wear.mask*tested)*(1-test.red*tested)] 
      
      agents[infector == 1 & location == work, 
             trans_prob := beta_today*bta_work*(1-mask_red*wear.mask)*(1-test.red*tested)] 
      
      # Higher transmission in lower hpi CTs, scaled such that highest quartile unaffected
      agents[location == ct, trans_prob:=trans_prob*(1+hpi_bta_mult*(hpi_quartile-1))] 
      
      # Get primary FOI for all agents  
      agents[, FOIi:=0]
      agents[infector==1, FOIi:=trans_prob*infector/n_present]
      agents[, FOI:=sum(FOIi), by = location]
      
      # Get secondary FOI for workers (workers can be infected at work or by community members)  
      agents[location != hhid, location2 := location] # 
      agents[location2 == work, location2 := work_ct]
      agents[, n_present2:=.N, by = location2]
      
      agents[infector == 1 & !is.na(location2), 
             trans_prob2 := beta_today*(1-mask_red*wear.mask)*(1+hpi_bta_mult*(hpi_quartile-1))*(1-test.red*tested)]
      
      agents[, FOIi2:=0]
      agents[infector==1, FOIi2:=trans_prob2*infector/n_present2]
      agents[, FOI2:=sum(FOIi2), by = location2]
      
      
      # Reduce probability of infection for vaccinated agents
      if(vaccination & date_now >= vax_start){
        agents[vax1 == 1 & vax2 == 0, 
               FOI:=FOI*(1-vax1_bta_red)]
        agents[vax1 == 1 & vax2 == 0, 
               FOI2:=FOI2*(1-vax1_bta_red)]
        
        agents[vax2 == 1, 
               FOI:=FOI*(1-vax2_bta_red)]
        agents[vax2 == 1, 
               FOI2:=FOI2*(1-vax2_bta_red)]
      }
      
      # Generate infections, update their state, sample for their nextstate and time until reaching it, 
      agents[FOI > 0 & state == "S", infect:=foi_infect(FOI)]
      agents[FOI2 > 0 & state == "S", infect:=foi_infect(FOI2) + infect]
      agents[infect >= 1, state:="E"]
      agents[infect >= 1, nextstate:=next_state(state, age, sex, mort_mult_now)]
      agents[infect >= 1, tnext:=t_til_nxt(state)]
      agents[infect >= 1, inf_date:=date_now]
      
      # document contacts in proportion to infection risk (assume community/work contacts are unknown)
      agents[FOI > 0 & state == "S", contact_prob:=FOI*known_contact_prob]
      agents[contact_prob > 1, contact := 1] # Agents with very high FOI end up with very high contact prob, assume they're contact is known
      agents[contact_prob > 0 & contact_prob < 1 & FOI > 0, contact := rbinom(.N, 1, contact_prob)]
      agents[contact == 1, t_since_contact:=dt] # Assumes most recent contact overwrites any old contact
      
      if(verbose){cat(nrow(agents[infect == 1,]), "new infections generated,",
                      nrow(agents[state == "Ih",]), "agents currently hospitalized,",
                      nrow(agents[state == "D",]), "cumulative COVID deaths\n\n")}
      
      # Quarantine probabilities
      #Start from scratch
      agents[, q_prob:=0]
      
      # Determine known residential infections
      agents[, res_infector := 0] # Reset from previous
      agents[, res_inf := 0]      # Reset from previous
      agents[state == "Im" | state == "Imh" | (infector == 1 & tested == 1), res_infector:=1] #Identify known residential infections
      agents[res_infector == 1, res_inf:=.N, by=hhid]  # Sum residential infectors by household
      
      # Get quarantine probabilities
      agents[q_duration == 0, 
             q_prob := q_prob_fx(contact, t_since_contact, q_prob_contact,
                                 res_inf, q_prob_resinf,
                                 t_symptoms, q_prob_symptoms,
                                 tested, infector, q_prob_testpos,
                                 essential, q_prob_essential)]
      
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
             q_duration:=q_dur_fx(.N, q_dur_mean)]
      agents[choose_quar == 1, q_bta_red:=(1-1/hhsize)**q_bta_red_exp]
      
      
      if(verbose){
        
        cat(nrow(agents[choose_quar == 1,]), "agents entered isolation\n",
            nrow(agents[q_duration > 0,]), "agents currently isolating\n",
            nrow(agents[infector == 1 & q_duration>0,])/nrow(agents[infector == 1,])*100, "% of",
            nrow(agents[infector == 1,]), "infectious agents are quarantining\n\n")
      }
      
      if(store_extra & time_day != "N"){
        stay_home[t] <- nrow(agents[location==hhid,])/nrow(agents)*100
        quar_iso[t] <- nrow(agents[q_duration > 0,])
        inf_quar[t] <- nrow(agents[infector == 1 & q_duration > 0,])/nrow(agents[infector == 1,])
        
        agents[,loc_type:="C"]
        agents[location == hhid, loc_type:="H"]
        agents[location == work, loc_type:="W"]
        
        foi_init <- agents[FOI > 0, mean(FOI), by=loc_type]
        foi_init$Date <- date_now
        
        mean_FOI[[t]] <- foi_init
        
        agents[,loc_type:=NULL]
      }  
      
      # Remove visiting agents
      if(visitors){
        agents <- agents[!is.na(id),]
      }  
      
      
      # Reset infection & location columns and remove temp quarantine objects
      agents[, c("location", "location2", "mobile", "infector", "res_infector",
                 "contact_prob", "contact", "wear.mask",
                 "trans_prob", "FOIi", "FOI", "n_present", "infect", 
                 "trans_prob2", "FOIi2", "FOI2","n_present2","choose_quar"):=NULL]
      
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
  
  gc()  
  
  # Export sim outputs
  fin_out                           <- list()
  fin_out[["epi_curve"]]            <- rbindlist(epi_curve, fill=TRUE)
  fin_out[["linelist_tests"]]       <- rbindlist(test_reports,fill = TRUE)
  fin_out[["input_pars"]]           <- input_pars
  fin_out[["agents"]]               <- agents
  
  if(store_extra){
    fin_out[["stay_home"]] <- stay_home
    fin_out[["quar_iso"]]  <- quar_iso
    fin_out[["inf_quar"]]  <- inf_quar
    fin_out[["FOIs"]]      <- rbindlist(mean_FOI, fill = TRUE)
  }
  
  if(vaccination){
    fin_out[["vax_phases"]] <- vax_phases
  }
  
  if(!dir.exists(output_path)){
    dir.create(output_path)
  }
  
  sim1_file <- paste0(output_path, 
                      "ABMv4_", 
                      "testing", testing,
                      "_vax", vaccination,
                      "_", t0,"-", t.end,
                      "sim1.rds")
  
  if(!file.exists(sim1_file)){
    
    saveRDS(fin_out, sim1_file)
    
  } else {
    past_files <- list.files(output_path)
    last_sim <- max(unlist(lapply(past_files, function(i){
      as.numeric(str_match(i, "sim\\s*(.*?)\\s*.rds")[,2])
    })))
    
    saveRDS(fin_out, paste0(output_path, 
                            "ABMv4_", 
                            "testing", testing,
                            "_vax", vaccination,
                            "_", t0,"-", t.end,
                            "sim", last_sim+1, ".rds"))
  }
  
  rm(fin_out)
  gc()
  
  return(NULL)
  
}
