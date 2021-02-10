# ---------------------
# Create latin hypercube for ABM calibration
# Chris Hoover Feb 2021
# ---------------------

library(lhs)

set.seed(430)
n_theta <- 1000 # Number of parameter sets 

# Parameter samples -----------------
bta_base     <- runif(n_theta, 0.2, 0.3)
bta_hh       <- runif(n_theta, 1.0, 1.5)
bta_work     <- runif(n_theta, 1.0, 1.2)
bta_sip_red  <- runif(n_theta, 0.1, 0.9)
hpi_bta_mult <- runif(n_theta, 0, 0.33)

# Quarantine parameters
# Probabilities of choosing to qurantine given different events (contact, residence infection, symptoms, test positive) all adjusted for 4 decision points per day
q_prob_contact     <- runif(n_theta,0.1^4, 0.5^4)
q_prob_resinf      <- runif(n_theta,0.25^4,0.75^4)
q_prob_symptoms    <- runif(n_theta,0.1^4,0.5^4)
q_prob_testpos     <- runif(n_theta,0.5^4,1.0^4)

q_prob_essential   <- runif(n_theta,0.1,1)         # Reduction in probability of qurantining if essential worker 
q_bta_red_exp      <- runif(n_theta,0.5,3)           # Exponent on reduction in transmission probability based on household size where reduction = (1-1/hhsize)^q_bta_red_exp

# Probability of known contact. Multiplier on the location specific FOI that determines if agent is aware of potential exposure
known_contact_prob <- runif(n_theta,1,10)

# function for length of time quarantining agent remains at home
q_dur_mean         <- runif(n_theta, 3, 10) 

# Initial states
E0 <- rpois(n_theta, 3)

# Join together and tweak with lhs -----------------
lhs <- cbind(bta_base, bta_hh, bta_work, bta_sip_red, hpi_bta_mult,
             q_prob_contact, q_prob_resinf, q_prob_symptoms, q_prob_testpos, q_prob_essential, q_bta_red_exp,
             known_contact_prob, q_dur_mean, mort_mult, E0)

saveRDS(lhs, here::here("data", "processed", "Calibration_LHS2_Wynton.rds"))