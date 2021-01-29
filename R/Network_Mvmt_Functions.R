#-------------------------
# Network/movement functions
#-------------------------
#' @title Sample location
#'  
#' @description Function to determine agent's location given present model conditions. Works for agents with 3 possible locations residence/home, community and either work or school. Function passes location ids plus probability of home and community, with probability work/ school then implied
#' 
#' @param l_res residence location id
#' @param l_com community location id
#' @param l_scl work/school location id
#' @param p_res probability of being at residence
#' @param p_com probability of being in community
#' 
#' @return vector of location ids
#' @export
#' 
scl_wrk_loc <- function(l_res, l_com, l_scl_wrk, p_res, p_com){
  n <- length(l_res)
  #print(n)
  u <- dqrunif(n)
  samp <- l_com
  index <- u < p_res
  samp[index] <- l_res[index]
  index <- u > (p_res + p_com)
  samp[index] <- l_scl_wrk[index]
  return(samp)
  
}


#' @title Simulate school-aged children location
#'  
#' @description Simulate location of school-aged children depending on agent characteristics, NPIs, and time of day
#' 
#' @param inf.state Infection state, one of S, E, Ip, Ia, Im, Imh, Ih, R
#' @param scl schools closed? 1/0
#' @param SiP shelter in place active? 1/0
#' @param time_day time of day (night or day)
#' @param day_week day of the week (U, M, T, W, R, F, or S)
#' @param age age of person
#' @param sociality agent sociality
#' @param res_id id of this individual's residence
#' @param scl_id id of this individual's school
#' @param comm_id id of this individual's community
#'  
#' @return location of this individual in the time step with options being the id corresponding to their home (H), school (S), or community (C)
#' @export
#'        
sac_location <- function(inf.state, 
                         scl, SiP, home_prob, time_day, day_week,
                         age, sociality,
                         res_id, scl_id, comm_id){
  
  n <- length(inf.state)
  
  # Children always at home at night and in the mornings  
  if(time_day == "M"){
    probs <- cbind(rep(1,n), rep(0,n))
    
    # Children are in school during the day if it's open and it's a weekday    
  } else if(scl == 0 & time_day == "D" & day_week %in% c("M", "T", "W", "R", "F")){
    probs <- cbind(rep(0,n), rep(0,n))
    
    # Children are randomly either still at school, in the community or at home in the evening during the week if schools are open  
  } else if(scl == 0 & time_day == "E" & day_week %in% c("M", "T", "W", "R", "F")){
    probs <- cbind(rep(0.33333,n), rep(0.33333,n))
    
    # Weekend-like dynamics if school is closed, but SiP not in effect or if it's the weekend: children can be at home or in the community during the day and evening
  } else if((scl == 1 & SiP == 0) | (day_week %in% c("S", "U") & SiP == 0)){
    probs <- cbind(rep(0.5,n), rep(0.5,n))
    
    # Shelter in place, sac most likely at home, but age and community dependent chance of being in community    
  } else if(SiP == 1){
    age_prob <- 1/(20-age) # More likely to be in community if older
    
    prob_home <- (1-age_prob)*(1-sociality)
    prob_home <- scale(prob_home)*sd(prob_home)+home_prob
    
    probs <- cbind(prob_home, 1-prob_home)
    
  } else {
    stop("Situation not recognized for School agent")
  }
  
  # School agents who are sick stay home
  at.home <- inf.state %in% c("Im", "Imh")
  location <- rep(NA_real_, n)
  location[at.home] <- res_id[at.home]
  location[!at.home] <- scl_wrk_loc(res_id[!at.home],comm_id[!at.home], scl_id[!at.home],  probs[!at.home, 1], probs[!at.home, 2])
  
  return(location)
}

#' @title Simulate worker locations
#'  
#' @description Simulate location of school-aged children depending on agent characteristics, NPIs, and time of day
#' 
#' @param inf.state Infection state, one of S, E, Ip, Ia, Im, Imh, Ih, R
#' @param SiP shelter in place active? 1/0
#' @param home_prob probability agent in ct is at home from safegraph metrics
#' @param time_day time of day (night or day)
#' @param day_week day of the week (U, M, T, W, R, F, or S)
#' @param age age of person
#' @param essential is the individual in an essential workforce? 1/0
#' @param sociality agent sociality
#' @param res_id id of this individual's residence
#' @param work_id id of this individual's workplace
#' @param comm_id id of this individual's community
#'  
#' @return location of this individual in the time step with options being the id corresponding to their home (H), work (W), or community (C)
#' @export
#'        
worker_location <- function(inf.state, 
                            SiP, home_prob, time_day, day_week,
                            age, essential, sociality,
                            res_id, work_id, comm_id){
  
  n <- length(inf.state)
  
  # home probability adjusted for age: older age groups more likely to be at home
  prob_home <- (age/100)*(1-sociality)
  # Normalize to  mean probability of being home
  prob_home <- (scale(prob_home)*sd(prob_home))+home_prob
  
  # Workers location community or home during the week in the morning/evening
  if(SiP == 0 & time_day %in% c("M", "E") & day_week %in% c("M", "T", "W", "R", "F")){
    
    probs = cbind(prob_home, 1-prob_home)
    
    # Workers at work during the weekday outside of shelter in place
  } else if(SiP == 0 & time_day == "D" & day_week %in% c("M", "T", "W", "R", "F")){
    
    probs = cbind(rep(0,n), rep(0,n))
    
    # Workers location (community, home, or work) during the weekend, non-night
  } else if(SiP == 0 & day_week %in% c("S", "U")){
    
    probs = cbind(prob_home, 1-prob_home)
    
    # Workers location during SiP weekday non-nights
  } else if(SiP == 1 & time_day %in% c("M", "E") & day_week %in% c("M", "T", "W", "R", "F")){
    
    probs = cbind(prob_home, 1-prob_home)
    
    # Workers at work during day during SiP if essential
  } else if(SiP == 1 & time_day == "D" & day_week %in% c("M", "T", "W", "R", "F")){
    
    probs = cbind(prob_home*(1-essential), (1-prob_home)*(1-essential))
    
    # Workers location community or home during the weekend, non-night during SiP
  } else if(SiP == 1 & day_week %in% c("S", "U")){
    
    probs = cbind(prob_home, 1-prob_home)
    
  } else {
    stop("Worker situation not recognized")
  }
  
  # Determine locations    
  
  location <- scl_wrk_loc(res_id, comm_id, work_id, probs[, 1], probs[, 2])
  
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
    
    out <- rbindlist(list(agent_visits, inf_agent_visits))[, c("location", "infector", "tested", "mask")]
  }  else {
    out <- data.table(location = NA, infector = NA)
  }
  
  return(out)
}