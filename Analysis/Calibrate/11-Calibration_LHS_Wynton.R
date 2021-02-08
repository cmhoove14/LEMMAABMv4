# ---------------------
# Create latin hypercube for ABM calibration
# Chris Hoover Feb 2021
# ---------------------

library(lhs)

set.seed(430)
n_theta <- 1440 # Number of parameter sets = 72*20 (20 jobs each with 72 cores (3 24 core nodes))

# Parameter samples -----------------
bta_base     <- runif(n_theta, 0.15, 0.45)
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

# Testing parameters
hpi_mult          <- runif(n_theta,0.5,2)                      # Multiplier for testing probability on hpi quartile (1=lowest, 4 highest), so 1 means highest hpi 4 times more likely to get tested
income_mult       <- runif(n_theta,0.5,2)                      # Multiplier for testing probability on income category (1=lowest, 3 highest), so 1 means highest income 3x more likely to get tested
case_finding_mult <- runif(n_theta,0.001,0.01)                   # Per test available improvement in case finding meant to capture test availability, improved case identification, contact tracing, etc.
cont_mult         <- runif(n_theta,1,20)                     # Multiplier for testing probability for agents with known contact
symp_mult         <- runif(n_theta,1,20)                     # Multiplier for testing probability for time experiencing symptoms (increases probability by symp_mult*t_symptoms, e.g. longer period experiencing symptoms increases probability of testing)
res_mult          <- runif(n_theta,1,200)                    # Multiplier for testing probability for agents with known residential infection
symp_state_mult   <- runif(n_theta,100,2000)                   # Multiplier for testing probability for agents infected with symptoms (Im, Imh, Ih)
hosp_mult         <- runif(n_theta,1000,20000)                  # Multiplier for testing probability for hospitalized agents
essential_prob    <- runif(n_theta,0.5,2)                    # Multiplier on testing probability for essential workers (e.g. 0.5 would reduce test_pob by half)


# Miscellaneous
mask_red             <- runif(n_theta,0.2,0.8)         
visitor_mult_testing <- runif(n_theta,1,5)           
visitor_mult_sfgrph  <- runif(n_theta,5,12)          
mort_mult            <- runif(n_theta,0.1,1)         

# Initial states
E0 <- rpois(n_theta, 3)

# Join together and tweak with lhs -----------------
lhs <- cbind(bta_base, bta_hh, bta_work, bta_sip_red, hpi_bta_mult,
             q_prob_contact, q_prob_resinf, q_prob_symptoms, q_prob_testpos, q_prob_essential, q_bta_red_exp,
             known_contact_prob, q_dur_mean,hpi_mult,income_mult,case_finding_mult,cont_mult,symp_mult,res_mult,symp_state_mult,hosp_mult,essential_prob,
             mask_red, visitor_mult_testing, visitor_mult_sfgrph, mort_mult, E0)

saveRDS(lhs, here::here("data", "processed", "Calibration_LHS_Wynton.rds"))