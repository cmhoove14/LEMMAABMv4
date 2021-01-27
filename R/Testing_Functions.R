#-------------------------
# Testing functions
#-------------------------

#' @title Test Probability function for public tests
#' 
#' @description Function to generate probability of getting tested based on income, race, symptoms, and known contacts
#' 
#' @param income vector of agent income (1 = <50k ; 2 = 50-100k per year ; 3 = >100k per year)
#' @param race vector of agent races
#' @param essential binary of whether agent is an essential worker
#' @param t_symptoms vector of time experiencing symptoms
#' @param state disease state
#' @param t_since_contact vector of time left that person with known contact will consider contact a factor in testing decision
#' @param res_inf Number of identified infectious individuals in residence
#' @param adapt_site binary of whether there's a test site in this census tract
#' @param adapt_site_mult multiplier of test probability for having adaptive test site in residence census tract
#' @param tests_avail number of tests available - creates multiplier to serve as proxy for how good identification of infected individuals is
#' @param symp_mult multiplier for test probability for agents with symptoms. Acts on a per-day with symptoms basis, so 2 days with symptoms * `symp_mult=10` would lead to 20x increase in test prob
#' @param cont_mult multiplier for test probability for agents with suspeceted contact
#' @param race_mult vector with same length as number of race groups with change in baseline probability of being tested
#' @param res_mult multiplier for test probability for agents with known infection in their residence
#' @param nosymp_state_mult multiplier for test probability for agents who are infected but not shopwing symptoms (Ia or Ip)
#' @param symp_state_mult multiplier for test probability for agents who are infected andshowing symptoms (Im or Imh)
#' @param hosp_mult multiplier for test probability for agents who are infected and hospitalized
#' 
#' @return vector of length n_agents with generated test probabilities
#' @export

test_probs_pub_fx <- function(income, race, essential, t_symptoms, state, t_since_contact, res_inf, adapt_site, adapt_site_mult, tests_avail,
                              symp_mult, race_mult, cont_mult, res_mult, nosymp_state_mult, symp_state_mult, hosp_mult){
  n <- length(income) # number agents
  adapt_prob <- 1+adapt_site*adapt_site_mult
  
  income_prob <- income
  income_prob[adapt_site == 1] <- 1 # Adaptive site removes barriers to testing
  
  ess_prob <- 1+essential*-0.5
  ess_prob[adapt_site == 1] <- 1   # Adaptive site removes barriers to testing
  
  race_prob <- rep(1,n)
  
  test_mult <- 1+tests_avail/100 # test availability improves case identification, contact tracing, etc.
  
  symp_prob <- 1+t_symptoms*symp_mult*adapt_prob*test_mult # Longer symptoms leads to higher probability of testing (same as quar probability)
  cont_prob <- rep(1,n)
  cont_prob[t_since_contact>5] <- cont_mult*adapt_prob*test_mult # Don't want very recent contacts getting tested. Because of fairly crude way that contacts are generated, this serves as proxy for more realistic processes where infected agent that tests positive contacts agent they had contact with or agent that knows they had contact waits to get tested
  res_prob <- 1+res_inf*res_mult*adapt_prob*test_mult
  
  state_prob <- rep(1,n)
  state_prob[state %in% c("Ia", "Ip")] <- nosymp_state_mult*adapt_prob*test_mult # Slight increase in probability for pre/asymptomatics: meant to better reflect agents' knowledge of exposure events
  state_prob[state %in% c("Im", "Imh")] <- symp_state_mult*adapt_prob*test_mult # Infected symptomatics very likely to get tested
  state_prob[state == "Ih"] <- hosp_mult*adapt_prob # hospitalized exceedingly likely to be tested if haven't been confirmed already
  
  # Have to get around fact that non-household residers don't have income and race attributes
  race_prob[race == 1] <- race_mult[1]
  race_prob[race == 2] <- race_mult[2]
  race_prob[race == 3] <- race_mult[3]
  race_prob[race == 4] <- race_mult[4]
  race_prob[race == 5] <- race_mult[5]
  race_prob[race == 6] <- race_mult[6]
  race_prob[race == 7] <- race_mult[7]
  race_prob[race == 8] <- race_mult[8]

  test_prob <- income_prob*race_prob*ess_prob*(cont_prob+res_prob+state_prob+symp_prob)
  #normalize to 1
  test_prob <- test_prob/max(test_prob, na.rm = T)
  return(test_prob)
}

#' @title Test Probability function for private tests
#' 
#' @description Function to generate probability of getting tested based on income, race, symptoms, and known contacts
#' 
#' @param income vector of agent income (1 = <50k ; 2 = 50-100k per year ; 3 = >100k per year)
#' @param race vector of agent races
#' @param essential binary of whether agent is an essential worker
#' @param t_symptoms vector of time experiencing symptoms
#' @param t_since_contact vector of time left that person with known contact will consider contact a factor in testing decision
#' @param res_inf Number of identified infectious individuals in residence
#' @param res_type vector of residence types, "H", "N", "P"
#' @param tests_avail number of tests available - creates multiplier to serve as proxy for how good identification of infected individuals is
#' @param symp_mult multiplier for test probability for agents with symptoms. Acts on a per-day with symptoms basis, so 2 days with symptoms * `symp_mult=10` would lead to 20x increase in test prob
#' @param race_mult vector with same length as number of race groups with change in baseline probability of being tested
#' @param cont_mult multiplier for test probability for agents with suspeceted contact
#' @param res_mult multiplier for test probability for agents with known infection in their residence
#' @param nosymp_state_mult multiplier for test probability for agents who are infected but not shopwing symptoms (Ia or Ip)
#' @param symp_state_mult multiplier for test probability for agents who are infected andshowing symptoms (Im or Imh)
#' @param hosp_mult multiplier for test probability for agents who are infected and hospitalized
#' 
#' @return vector of length n_agents with generated test probabilities
#' @export

test_probs_pvt_fx <- function(income, race, essential, t_symptoms, state, t_since_contact, res_inf, res_type, tests_avail,
                              symp_mult, race_mult, cont_mult, res_mult, nosymp_state_mult, symp_state_mult, hosp_mult){
  n <- length(income) # number agents
  
  income_prob <- income^2 # Stronger influence of income on private testing
  
  ess_prob <- (1+essential*-0.5)^2 # Stronger influence of essential worker status on private testing
  
  race_prob[race == 1] <- race_mult[1]
  race_prob[race == 2] <- race_mult[2]
  race_prob[race == 3] <- race_mult[3]
  race_prob[race == 4] <- race_mult[4]
  race_prob[race == 5] <- race_mult[5]
  race_prob[race == 6] <- race_mult[6]
  race_prob[race == 7] <- race_mult[7]
  race_prob[race == 8] <- race_mult[8]

  test_mult <- 1+tests_avail/100 # test availability improves case identification, contact tracing, etc.
  
  symp_prob <- 1+t_symptoms*symp_mult*test_mult # Longer symptoms leads to higher probability of testing (same as quar probability)
  cont_prob <- rep(1,n)
  cont_prob[t_since_contact>5] <- cont_mult*test_mult # Don't want very recent contacts getting tested. Because of fairly crude way that contacts are generated, this serves as proxy for more realistic processes where infected agent that tests positive contacts agent they had contact with or agent that knows they had contact waits to get tested
  res_prob <- 1+res_inf*res_mult*test_mult
  
  state_prob <- rep(1,n)
  state_prob[state %in% c("Ia", "Ip")] <- nosymp_state_mult*test_mult # Slight increase in probability for pre/asymptomatics: meant to better reflect agents' knowledge of exposure events
  state_prob[state %in% c("Im", "Imh")] <- symp_state_mult*test_mult # Infected symptomatics very likely to get tested
  state_prob[state == "Ih"] <- hosp_mult # hospitalized exceedingly likely to be tested if haven't been confirmed already

  test_prob <- income_prob*race_prob*ess_prob*(cont_prob+res_prob+state_prob+symp_prob)
  #normalize to 1
  test_prob <- test_prob/max(test_prob, na.rm = T)
  return(test_prob)
}

#' @title Adaptively generate new test site
#' 
#' @description Function to look in testing data and find census tract in which to place new testing site
#' 
#' @param test_reports previous testing data to base adaptive decision on
#' @param adapt_freq timeframe to look back at test reports to make decision
#' @param n_sites number of adaptive sites to add
#' @param site_geo geography to place tests by. either `cbg`, `ct`, `zip`, or `nbhd`
#' @param geo_pops data.table of site geo ids/names and their population, necessary if using `wper_pos` as testing criteria
#' @param t0 start time of simulation as a date
#' @param date_now current date of simulation
#' @param test_criteria either `per_pos` to place sites based on areas with highest percent positivity, `n_pos` to place sites based on areas with highest raw number of positive cases, `wper_pos` for percent positive times the site geo population (weighted percent positive), or `n_tests` to place sites based on areas with least number of tests conducted
#' 
#' @return Census tract code of new test site(s)
#' @export

adapt_site_fx <- function(test_reports, adapt_freq, n_sites, site_geo, geo_pops, t0, date_now, test_criteria){
  # Process test data on which to base deicision
  start <- floor(as.numeric(date_now-t0-1))
  end <- floor(as.numeric(date_now-t0)-adapt_freq-1)
  
  test_data <- rbindlist(lapply(end:start, function(d) test_reports[[d]]))
  test_data_sum <- test_data[, 
                             .(n_tests = .N, n_pos = sum(test_pos), per_pos = sum(test_pos)/.N),
                             by = get(site_geo)]
  
  # Add weight percentage positive metric  
  colnames(test_data_sum)[1] <- site_geo  
  
  test_data_sum <- merge.data.table(test_data_sum, geo_pops, by = site_geo)
  test_data_sum[, wper_pos:=per_pos*pop]
  
  #Summarise test data and get geographies for site placement
  
  criterias <- test_data_sum[[test_criteria]]  
  
  other_criteria <- ifelse(test_criteria %in% c("n_pos", "n_tests"),
                           "per_pos", "n_pos")
  
  other_criterias <- test_data_sum[[other_criteria]]
  
  if(test_criteria == "n_tests"){
    
    sorted_geos <- test_data_sum[[site_geo]][order(criterias, -other_criterias)][1:n_sites]
    
  } else {
    
    sorted_geos <- test_data_sum[[site_geo]][order(-criterias, -other_criterias)][1:n_sites]
    
  } 
  
  return(sorted_geos)
  
}