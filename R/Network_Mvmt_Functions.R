#-------------------------
# Network/movement functions
#-------------------------
#' @title Simulate worker locations
#'  
#' @description Simulate location of workers depending on agent characteristics, NPIs, and time of day
#' 
#' @param inf.state Infection state, one of S, E, Ip, Ia, Im, Imh, Ih, R
#' @param SiP shelter in place active? 1/0
#' @param reopen whether partial reopen is in effect
#' @param home_prob CT-based probability of being at home
#' @param time_day time of day (night or day)
#' @param day_week day of the week (U, M, T, W, R, F, or S)
#' @param age age of person
#' @param essential is the individual in an essential workforce? 1/0
#' @param sociality agent sociality
#' @param hhid id of this individual's residence
#' @param ct id of this individual's community
#'  
#' @return location of this individual in the time step with options being the id corresponding to their home (H) or community (C)
#' @export
#'        
worker_location <- function(inf.state, 
                            SiP, reopen, home_prob, time_day, day_week,
                            age, essential, sociality,
                            hhid, ct){
  
  n <- length(inf.state)
  rn <- dqrunif(n)
  
  # home probability adjusted for age: older age groups more likely to be at home
  prob_home <- (age/100)*(1-sociality)
  # Normalize to  mean probability of being home
  prob_home <- (scale(prob_home)*sd(prob_home))+home_prob
  
  # Workers location community or home during the week in the morning/evening
  if(SiP == 0 & time_day %in% c("M", "E") & day_week %in% c("M", "T", "W", "R", "F")){
    
    p_home = prob_home
    
    # Workers at work during the weekday outside of shelter in place
  } else if(SiP == 0 & time_day == "D" & day_week %in% c("M", "T", "W", "R", "F")){
    
    p_home = 0
    
    # Workers location (community, home, or work) during the weekend, non-night
  } else if(SiP == 0 & day_week %in% c("S", "U")){
    
    p_home = prob_home
    
    # Workers location during SiP weekday non-nights
  } else if(SiP == 1 & time_day %in% c("M", "E") & day_week %in% c("M", "T", "W", "R", "F")){
    
    p_home = prob_home
    
    # Workers at work after partial reopening if essential
  } else if(SiP == 1 & reopen == 1 & time_day == "D" & day_week %in% c("M", "T", "W", "R", "F")){
    
    p_home = prob_home*(1-essential)
    
    # Workers at work after partial reopening if essential
  } else if(SiP == 1 & reopen == 0 & time_day == "D" & day_week %in% c("M", "T", "W", "R", "F")){
    
    p_home = prob_home*(1-essential)
    
    # Workers location community or home during the weekend, non-night
  } else if(SiP == 1 & day_week %in% c("S", "U")){
    
    p_home = prob_home
    
  } else {
    stop("Worker situation not recognized")
  }
  
  # Determine locations    
  
  location <- ct
  location[rn<p_home] <- hhid

  return(location)
  
}

#' @title Simulate location of individuals who are not workers or school-aged
#'  
#' @description Simulate location of non workers or school children
#' 
#' @param inf.state Infection state, one of S, E, Ip, Ia, Im, Imh, Ih, R
#' @param SiP shelter in place active? 1/0
#' @param time_day time of day (night or day)
#' @param age age of person
#' @param sociality agent sociality
#' @param hhid id of this individual's residence
#' @param comm_id id of this individual's community
#'  
#' @return location of this individual in the time step with options being the id corresponding to their home (H), work (W), school (S), or community (C)
#' @export
#'        
other_location <- function(inf.state, 
                           SiP, home_prob,  
                           age, sociality,   
                           hhid, ct){
  n <- length(inf.state)
  # home probability adjusted for age: older age groups more likely to be at home
  prob_home <- 1-sociality
  age_home <- rep(1,n)
  age_home[age >= 65] <- 1+SiP
  prob_home <- prob_home*age_home
  # Normalize to  mean probability of being home
  p.home <- (scale(prob_home)*sd(prob_home))+home_prob
  
  # Assign locations  
  location <- ct  

  l.probs <- dqrunif(n)
  at.home <- l.probs < p.home
  location[at.home] <- hhid[at.home]
  
  return(location)
}

#' @title Community location function
#'  
#' @description Draws random uniform variable between 0 and 1, looks in CDF matrix to determine corresponding probability of visiting CBG, finds corresponding CBG visited in index matrix. See also `GetCBGVisit_cpp` for faster implementation
#' 
#' @param mat_cdf cumulative probability matrix with rows corresponding to CDF of individual residing in row and visiting column
#' @param cbgs cbg names coresponding to rows of cbg_cdf
#' @param agent_cbgs cbgs of agents which are candidates to interact in the community
#' 
#' @return 
#' @export
#' 

GetCTVisit <- function(mat_cdf, cts, agent_cts) {
  ct_indices <- fmatch(agent_cts, cts)
  n <- length(ct_indices)
  r <- dqrng::dqrunif(n)
  mat_cdf_expand <- mat_cdf[ct_indices,]
  cts[max.col(r < mat_cdf_expand, "first")]
}

# microbenchmark::microbenchmark(match(agents[, ct], sf_ct_ids), fmatch(agents[, ct], sf_ct_ids))

#' @title Convert safegraph visitor panel to agents
#'  
#' @description takes number of visitors from county to ct and converts to a data table of agents to merge with resident agents. Assumes agents spend time in community and are either infected or not. All other characteristics left NA
#' 
#' @param visits_dt data.table of visitors to census tracts/block groups
#' @param visitor_mult_testing multiplier on true infection rate of visitorsto account for fact that not all infections are tested
#' @param visitor_mult_sfgrph multiplier on true number of visitors to account for fact that not all people are captured in safegraph paenel data
#' 
#' @return dataframe of visiting agents to append to regular agents
#' @export
#' 

visitors_to_agents <- function(visits_dt, visitor_mult_testing, visitor_mult_sfgrph){
  
  # adjust for testing and sfgrph devices to estimate of number of visitors who are infectious  
  visits_dt[, visits_adjust:=Visits*visitor_mult_sfgrph]
  visits_dt[, inf_adjust:=newcountconfirmed*visitor_mult_testing]
  visits_dt[, inf_prob_adjust:=inf_adjust/pops]
  visits_dt[, inf_visits:=rpois(.N, visits_adjust*inf_prob_adjust)]
  
  inf_visits <- visits_dt[inf_visits > 0,]
  
  # Convert number of visiting agents to data table with necessary characteristics to incorporate into FOI estimate for CT location  
  vals <- inf_visits$visits_adjust - inf_visits$inf_visits
  agent_visits <- as.data.frame(inf_visits)[rep(sequence(nrow(inf_visits)), vals), ]
  
  inf_vals <- inf_visits$inf_visits
  inf_agent_visits <- as.data.frame(inf_visits)[rep(sequence(nrow(inf_visits)), inf_vals), ]
  
  if(nrow(inf_agent_visits) > 0){
    agent_visits$location <- as.numeric(agent_visits$ct)
    agent_visits$infector <- 0
    agent_visits$tested <- 0
    agent_visits$mask <- mask_fx(nrow(agent_visits))
    
    inf_agent_visits$location <- as.numeric(inf_agent_visits$ct)
    inf_agent_visits$infector <- 1
    inf_agent_visits$tested <- 0
    inf_agent_visits$mask <- mask_fx(nrow(inf_agent_visits))
    
    out <- as.data.table(rbind(agent_visits, inf_agent_visits) %>% 
                           dplyr::select(ct, location, infector, tested, mask))
  }  else {
    out <- data.frame(location = NA, infector = NA)
  }
  
  return(out)
}