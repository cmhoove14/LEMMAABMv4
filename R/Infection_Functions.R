#-------------------------
# Infection functions
#-------------------------
#' @title latent period
#'  
#' @description Draw from distribution of proposed latent periods. Used in `next.state`
#' 
#' @param n_E Number of draws
#' @param shape.l Shape of gamma distribution
#' @param scale.l Scale of gamma distribution
#' 
#' @return numeric of time spent in latent period (E)
#' @export
#'        

t_latent <- function(n_E, shape.l=4, scale.l=1){
  rgamma(n_E, shape.l, scale.l)
} 

#' @title Presymptomatic period
#'  
#' @description Draw from distribution of proposed presymptomatic periods (basically incubation-latent periods) to model presymptomatic transmission. Used in `next.state`
#' 
#' @param n_Ip Number of draws
#' @param shape Shape of gamma distribution
#' @param scale Scale of gamma distribution
#' 
#' @return numeric of time spent in presymptomatic period (Ip)
#' @export
#'        
t_presymp <- function(n_Ip, shape.ip=10,scale.ip=7){
  rgamma(n_Ip, shape.ip, scale.ip)
} 

#' @title Asymptomatic period
#'  
#' @description Draw from distribution of proposed asymptomatic period. Used in `next.state`
#' 
#' @param n_Ia Number of draws
#' @param shape.ia Shape of gamma distribution
#' @param scale.ia Scale of gamma distribution
#' 
#' @return numeric of time spent in presymptomatic period (Ip)
#' @export
#'        
t_asymp <- function(n_Ia, shape.ia = 12, scale.ia = 2){
  rgamma(n_Ia, shape.ia, scale.ia)
} 

#' @title Mild infection duration
#'  
#' @description Draw from distribution of proposed length spent with minor symptoms. Used in `next.state`
#' 
#' @param n_Im number of draws
#' @param shape.im Shape parameter of gamma distn
#' @param scale.im Scale parameter of gamma distn
#' 
#' @return numeric of time spent in mildly symptomatic (Im) state
#' @export
#'        
t_msymp <- function(n_Im, shape.im = 6, scale.im = 1) {
  rgamma(n_Im, shape.im, scale.im)
}

#' @title Symptom onset to hospitalization
#'  
#' @description Draw from distribution of proposed time spent with minor symptoms before being hospitalized for severe infections (i.e. those with B(1,p.sevsymp==1)). Used in `next.state`
#' 
#' @param n_Imh number of draws
#' @param shape.imh shape of gamma distn
#' @param scale.imh scale of gamma distn
#' 
#' @return numeric of time spent mildly symptomatic before hospitalization (Imh)
#' @export
#'        
t_mtosev <- function(n_Imh,shape.imh = 3, scale.imh = 1){
  rgamma(n_Imh, shape.imh, scale.imh)
}

#' @title time spent severely symptomatic (in hospital)
#'  
#' @description Draw from distribution of proposed time spent with severe symptoms before dying or being discharged. Used in `next.state`
#' 
#' @param n_Ih number of draws
#' @param shape.ih shape of gamma distn
#' @param scale.ih scale of gamma distn
#' 
#' @return numeric of time spent hospitalized (Ih)
#' @export
#'        
t_sevsymp <- function(n_Ih, shape.ih = 6, scale.ih = 0.5){
  rgamma(n_Ih, shape.ih, scale.ih)
}

#' @title Probability of symptomatic infection
#'  
#' @description Age-dependent probabilities of having clinical (symptomatic) infection. Used in `next.state`. From https://www.nature.com/articles/s41591-020-0962-9/figures/8
#' 
#' @param age age of person
#' 
#' @return numeric of probability of clinical (symptomatic).
#' @export
#'
p_symp <- function(age){
  n <- length(age)
  n[age %in% c(0:9)] <- 0.29
  n[age %in% c(10:19)] <- 0.21
  n[age %in% c(20:29)] <- 0.27
  n[age %in% c(30:39)] <- 0.33
  n[age %in% c(40:49)] <- 0.4
  n[age %in% c(50:59)] <- 0.49
  n[age %in% c(60:69)] <- 0.63
  n[age %in% c(70:79)] <- 0.69
  n[age >= 80] <- 0.69
  
  return(n)
}

#' @title Probability of severely symptomatic (will be hospitalized) infection
#'  
#' @description Age-dependent probabilities of having severe symptoms. Used in `next.state`
#' 
#' @param age age of person
#' 
#' @return numeric of probability of having severe symptoms.
#' @export
#'
p_sevsymp <- function(age){
  n <- length(age)
  n[age %in% c(0:9)] <- 0.004
  n[age %in% c(10:19)] <- 0.004
  n[age %in% c(20:29)] <- 0.01
  n[age %in% c(30:39)] <- 0.04
  n[age %in% c(40:49)] <- 0.09
  n[age %in% c(50:59)] <- 0.13
  n[age %in% c(60:69)] <- 0.19
  n[age %in% c(70:79)] <- 0.2
  n[age >= 80] <- 0.25 
}

#' @title Probability of death
#'  
#' @description Age-dependent probabilities of dying given hospitalization (Ih) Used in `next.state`; from https://www.bmj.com/content/369/bmj.m1923 Fig 4 mean of sex-stratified
#' 
#' @param age age of person
#' 
#' @return numeric of probability of dying.
#' @export
#'
p_mort <- function(age) {
  n <- length(age)
  n[age %in% c(0:9)] <- 0.0005#0.01 Probabilities altered due to too high death rate in abm sims 2020-10-28
  n[age %in% c(10:19)] <- 0.001#0.0205
  n[age %in% c(20:29)] <- 0.01#0.031
  n[age %in% c(30:39)] <- 0.02#0.0475
  n[age %in% c(40:49)] <- 0.03#0.0785
  n[age %in% c(50:59)] <- 0.06#0.12
  n[age %in% c(60:69)] <- 0.1 #0.186
  n[age %in% c(70:79)] <- 0.15 #0.3
  n[age >= 80] <- 0.25#0.45
}

#' @title Time til Next state
#' 
#' @description Take input infection state the time spent in that state  
#' 
#' @param pre.status Infection status that has expired 
#' 
#' @return vector time to be spent in infection state. Time should be converted to `numeric` for subsequent use
#' @export

t_til_nxt <- function(pre.status){
  n <- vector("numeric", length(pre.status))
  n_E <- sum(pre.status == "E")
  n_Ip <- sum(pre.status == "Ip")
  n_Ia <- sum(pre.status == "Ia")
  n_Im <- sum(pre.status == "Im")
  n_Imh <- sum(pre.status == "Imh")
  n_Ih <- sum(pre.status == "Ih")
  
  n[pre.status == "E"] <- t_latent(n_E)
  n[pre.status == "Ip"] <- t_presymp(n_Ip)
  n[pre.status == "Ia"] <- t_asymp(n_Ia)
  n[pre.status == "Im"] <- t_msymp(n_Im)
  n[pre.status == "Imh"] <- t_mtosev(n_Imh)
  n[pre.status == "Ih"] <- t_sevsymp(n_Ih)
  
  return(n)
}  

#' @title Next State
#' 
#' @description Take input infection state and age and return the next infection state and the time spent in that state  
#' 
#' @param pre.status Infection status that has expired 
#' @param age age of individual for probability of moving to different states
#' 
#' @return vector the next state
#' @export

next_state <- function(pre.status, age){
  n <- length(pre.status)
  post.status <- rep("R", n)
  p_symps <- p_symp(age)
  p_sevsymps <- p_sevsymp(age)
  p_deads <- p_mort(age)
  p1 <- dqrunif(n)
  p2 <- dqrunif(n)
  
  post.status[pre.status == "E"] <- "Ip"
  post.status[pre.status == "Ip" & p1>p_symps] <- "Ia"
  post.status[pre.status == "Ip" & p1<p_symps & p2>p_sevsymps] <- "Im"
  post.status[pre.status == "Ip" & p1<p_symps & p2<p_sevsymps] <- "Imh"
  post.status[pre.status == "Imh"] <- "Ih"
  post.status[pre.status == "Ih" & p1<p_deads] <- "D"
  
  return(post.status)
}

#' @title Simulate Infection
#'  
#' @description Randomly determine if infection occurs from FOI
#' 
#' @param foi  Person's FOI
#'  
#' @return binary of infection occurring or not
#' @export
#'        

foi_infect <- function(foi){
  p <- dqrunif(length(foi))
  return(as.numeric(p<(1-exp(-foi))))
}