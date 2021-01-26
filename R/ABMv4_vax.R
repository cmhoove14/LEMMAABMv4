#------------------
# ABM function with testing
#------------------
#' @title ABM V4 with testing 
#' 
#' @description Function to run agent based model
#' 
#' @param bta_fx function that takes time step as input returns beta at that time step
#' @param bta_hh houshold transmission probability multiplier
#' @param bta_work work transmission probability multiplier
#' @param E0 number of initial exposed agents
#' @param Ip0 number of initial pre-symptomatic infections agents
#' @param Ia0 number of initial asymptomatic infectious agents
#' @param Im0 number of initial mildy symptomatic agents
#' @param Imh0 number of initial mildly symptomatic, will become severely symptomatic agents
#' @param Ih0 number of initial hospitalized agents
#' @param R0 number of initial recovered agents
#' @param D0 number of initial diceased agents
#' @param agents_dt data.table of agents with necessary characteristics including: TODO: FILL THESE IN
#' @param ct_cdf_list list of matrices of dim cts x cts in which entries represent probabilities of moving from ct i to ct j. Used in `GetCTVisit`/`GetCTVisit_cpp` function
#' @param ct_ids vector relating row numbers of `ct_cdf_list` to actual ct codes
#' @param stay_home_dt data table with columns `Date`, `origin_census_block_group`, and `pct_home` (E.g. derived from safegraph data) to use for social distancing/stay at home compliance
#' @param visitors logical of whether to model outside visitors
#' @param visitors_list list of data frames/tables with columns corresponding to census block group, number of external visitors to that ct, county from which they visited, and population and infection characteristics of that county. column inf_prob represents incidence in that county (new cases identified per population) and is used as probability a visiting agent is infectious along with `visitor_mult_testing` to correct for ratio of true incidence to incidence from testing data and `visitor_mult_sfgrph` to correct for ratio of safegraph devices tracked to total population
#' @param visitor_mult_testing multiplier to adjust probability visiting agent is infected based on ratio of true incidence to incidence identified via testing
#' @param visitor_mult_sfgrph multiplier to adjust total number of visitors from safegraph number of visitors based on ratio of tracked safegraph devices to total movement of people. THis is quite subjective as we don't really know much about Safegraph's panel and how it realtes to underlying population characteristics
#' @param t0 start date of simulation
#' @param t.tot total time to run simulation (in days)
#' @param dt time step (defaults to 1/6, needs editing if otherwise)
#' @param day_of_week_fx function which takes t returns character of day of week (U,M,T,W,R,F,S)
#' @param SiP.start Date that shelter in place started
#' @param part.reopen Date that partial reopen allows essential workers back to work
#' @param mask.start Date that mask mandate was introduced
#' @param mask.red Reduction in transmission probability from wearing mask
#' @param mask_fx fnction to assign probabilities of wearing a mask following mask mandate
#' @param social_fx function to generate random sociality metrics
#' @param q_prob_contact probability an agent will quarantine because of known contact with infectious agent
#' @param q_prob_resinf probability an agent will quarantine because of shared residence with infectious agent
#' @param q_prob_symptoms probability an agent will quarantine because of symptoms on a per day basis: e.g. one full day of symptoms = q_prob_symptoms probability of isolating. Note that agents will consider this 4 times per day, so probability should adjust for multiple opportunities 
#' @param q_prob_testpos probability an agent will quarantine because of positive test result
#' @param q_prob_adapt probability multiplier agent will quarantine if "exposed" to an adaptive testing site
#' @param q_dur_fx function to generate random quarantine durations
#' @param known_contact_prob proportional increase in probability that a contact with an infectious individual is known. 0 implies known contact occurs at same rate as FOI, implying very low probability that an infectious contact will be known 1 implies probability is double FOI. Since FOIs will be quite small, larger multipliers are suggested 
#' @param testing TRUE/FALSE for whether to conduct testing
#' @param test_start Date at which testing begins
#' @param test_delay_fx function to generate time delay from test to notification
#' @param tests_pp_fx function returning the number of tests available per agent on day t since test start
#' @param tests_pub proportion of tests that are public vs private
#' @param tests_wknd percent of weekday tests available on weekends
#' @param test_probs_pub_fx function which returns testing probability for individuals to get tested at public site
#' @param test_probs_pvt_fx function which returns testing probability for individuals to get tested at prvate site
#' @param symp_mult multiplier for test probability for agents with symptoms. Acts on a per-day with symptoms basis, so 2 days with symptoms * `symp_mult=10` would lead to 20x increase in test prob
#' @param cont_mult multiplier for test probability for agents with suspeceted contact
#' @param race_test_mults vector of race group testing multipliers
#' @param res_mult multiplier for test probability for agents with known infection in their residence
#' @param nosymp_state_mult multiplier for test probability for agents who are infected but not shopwing symptoms (Ia or Ip)
#' @param symp_state_mult multiplier for test probability for agents who are infected andshowing symptoms (Im or Imh)
#' @param hosp_mult multiplier for test probability for agents who are infected and hospitalized
#' @param test.red reduction in transmission probability if agents has tested positive
#' @param adaptive TRUE/FALSE for whether adaptive testing should be implemented with `adapt_site_fx` in simulation
#' @param adapt_start time point at which to start adaptive testing
#' @param adapt_freq frequency with which to check test reports and generate new test site
#' @param adapt_site_fx function to determine when and where to place adaptive testing sites
#' @param adapt_site_geo geography to base adaptive sites on. Can be `ct`, `nbhd`, or `zip`
#' @param n_adapt_sites how many adaptive sites available to place
#' @param adapt_site_test_criteria character either `n_pos`or `pct_pos`for whteher targeted sites should be added based on census tract with highest number positive tests or highest percent positive tests
#' @param adapt_site_mult increase in testing probability if test site is placed in residence ct
#' @param adapt_site_delay_fx function to generate time delay for adaptive sites
#' @param verbose should detailed info at each time step be printed?
#' @param store_extra should extra metrics including number of other illnesses, % staying home, and % isolating be stored and returned? Good for debugging
#' 
#' @return list with two objects: an epi curve with counts of agents in each state through time and a dataframe with infection reports for every new infection (e.g. entry created whenever agent transitions from S to E)
#' @export

covid_abm_v4 <- function(bta_fx, bta_hh, bta_work, E0, Ip0, Ia0, Im0, Imh0, Ih0, R0, D0,                  
                         agents_dt, ct_cdf_list, ct_ids, stay_home_dt, visitors, visitors_list, visitor_mult_testing, visitor_mult_sfgrph,
                         t0, t.tot, dt, day_of_week_fx, time_of_day_fx, SiP.start, scl.close, part.reopen, mask.start, mask.red, 
                         mask_fx, social_fx, q_prob_contact, q_prob_resinf, q_prob_symptoms, q_prob_testpos, q_prob_adapt,
                         q_dur_fx, known_contact_prob, 
                         testing, test_start, test_delay_fx, tests_pp_fx, tests_pub, tests_wknd, test_probs_pub_fx, test_probs_pvt_fx, 
                         symp_mult, race_test_mults, cont_mult, res_mult, nosymp_state_mult, symp_state_mult, hosp_mult, test.red, 
                         adaptive, adapt_start, adapt_freq, adapt_site_fx, 
                         adapt_site_geo, n_adapt_sites,  
                         adapt_site_test_criteria, adapt_site_mult, adapt_site_delay_fx, 
                         verbose, store_extra){
  
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
      home.today[,"Date":=NULL]
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
      
      agents[infector == 1 & essential == 1, 
             trans_prob := beta_today*bta_work*(1-mask.red*wear.mask)*(1-test.red*tested)]
      
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
  gc()  
  
  fin_out <- list()
  fin_out[["epi_curve"]] <- epi_curve
  fin_out[["infections"]] <- rbindlist(infection_reports,fill=TRUE)
  fin_out[["linelist_tests"]] <- rbindlist(test_reports,fill = TRUE)
  
  if(store_extra){
    fin_out[["stay_home"]] <- stay_home
    fin_out[["quar_iso"]] <- quar_iso
    fin_out[["inf_quar"]] <- inf_quar
  }
  
  if(store_extra & other_ill){
    fin_out[["other_ill"]] <- other_ill_i
  }
  
  return(fin_out)
  
}
