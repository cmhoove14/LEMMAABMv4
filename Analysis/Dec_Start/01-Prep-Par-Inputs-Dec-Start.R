# ---------------------------------------------------------
# Pre-process data and sim setup
# Chris Hoover Jan 2021
# ---------------------------------------------------------

input_pars <- list()

# TIME FRAME, time step, and other temporal characteristics ---------------------
input_pars$time_pars <- list()
t0                   <- as.Date("2020-12-01")
dt                   <- 4/24
today                <- Sys.Date()

# Key change dates
SiP.start     <- as.Date("2020-03-15")  # Shelter in Place started
mask.start    <- as.Date("2020-04-17")  # Mask mandate initiated
reopen.start  <- as.Date("2020-05-17")  # Start of reopening initiatives in SF
SiP2.start    <- as.Date("2020-12-06")  # Start of second shelter in place
reopen2.start <- as.Date("2021-01-26")  # Start of second reopeninig in SF
holidays      <- c(seq.Date(as.Date("2020-05-23"), as.Date("2020-05-25"), by = "day"), # Memorial Day
                  seq.Date(as.Date("2020-07-03"), as.Date("2020-07-05"), by = "day"), # 4th of July
                  seq.Date(as.Date("2020-09-05"), as.Date("2020-09-07"), by = "day"), # Labor day
                  seq.Date(as.Date("2020-11-26"), as.Date("2020-11-29"), by = "day"), # Thanksgiving
                  seq.Date(as.Date("2020-12-24"), as.Date("2020-12-26"), by = "day"), # Christmas 
                  seq.Date(as.Date("2020-12-31"), as.Date("2021-01-01"), by = "day")) # New Years

ref_date      <- as.Date(t0-1)
t.end         <- as.Date("2021-03-01")       # Simulation end date

t.tot         <- as.numeric(t.end - t0)

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

# Break up day into 6 parts, Morning, Dayx2, Evening, Nightx2: SHOULD UPDATE IF timestep!=4/24
time_of_day <- rep(c("M", "D", "D", "E", "N", "N"), times = t.tot)  

# Store
input_pars$time_pars$t0             <- t0
input_pars$time_pars$t.end          <- t.end
input_pars$time_pars$t.tot          <- t.tot
input_pars$time_pars$ref_date       <- ref_date
input_pars$time_pars$dt             <- dt
input_pars$time_pars$day_of_week_fx <- day_of_week_expand
input_pars$time_pars$SiP.start      <- SiP.start
input_pars$time_pars$mask.start     <- mask.start
input_pars$time_pars$time_of_day_fx <- time_of_day


# Initial infection characteristics ---------------------
#Base starting conditions on CA observed cases plus correction factor for underreporting
source(here::here("data", "get","COVID_CA_get_latest.R"))

  CA_start <- CA_cases %>% 
    filter(date == t0 & county == "San Francisco") %>% 
    bind_cols(CA_hosp %>% 
                filter(todays_date == t0 & county == "San Francisco"))

# parameters to convert observed to model starting conditions (fairly subjective section)   
  prop_asymp <- 0.4  # Proportion cases asymptomatic
  prop_psymp <- 0.05  # Proportion cases presymptomatic
  underreport <- 0.2   # Proportion of cases not pre or asymptomatic that go unreported
  prop_hosp <- 0.05
  
  active_true <- round(CA_start$newcountconfirmed*(1+prop_asymp+prop_psymp+underreport))
  recovered_true <- round(CA_start$totalcountconfirmed*(1+prop_asymp+prop_psymp+underreport) )
  
input_pars$init_states <- list()

input_pars$init_states$E0   <- CA_start$newcountconfirmed # New cases in the pipelin proportional to new observed cases
input_pars$init_states$Ip0  <- round(active_true*prop_psymp)   # New observed times underreport times mean presymp period
input_pars$init_states$Ia0  <- round(active_true*prop_asymp)
input_pars$init_states$Im0  <- round(active_true*(1-(prop_asymp+prop_psymp+prop_hosp)))
input_pars$init_states$Imh0 <- round(active_true*prop_hosp)
input_pars$init_states$Ih0  <- CA_start$hospitalized_covid_patients 
input_pars$init_states$R0   <- recovered_true
input_pars$init_states$D0   <- CA_start$totalcountdeaths

# Quarantine parameters ---------------------
input_pars$quar_pars <- list()

# Probabilities of choosing to qurantine given different events (contact, residence infection, symptoms, test positive) all adjusted for 4 decision points per day
input_pars$quar_pars$q_prob_contact     <- 0.2^4 
input_pars$quar_pars$q_prob_resinf      <- 0.5^4
input_pars$quar_pars$q_prob_symptoms    <- 0.5^4 
input_pars$quar_pars$q_prob_testpos     <- 0.9^4
input_pars$quar_pars$q_prob_adapt       <- 0.9^4

# Probability of known contact. Multiplier on the location specific FOI that determines if agent is aware of potential exposure
input_pars$quar_pars$known_contact_prob <- 9

# function for length of time quarantining agent remains at home
q_dur_fx <- function(n_agents){
  rgamma(n_agents, 14, 2)
}
input_pars$quar_pars$q_dur_fx           <- q_dur_fx 

# Testing parameters ----------------------------
input_pars$test_pars <- list()

input_pars$test_pars$test_start        <- as.Date("2020-03-01")  # Testing started
input_pars$test_pars$tests_wknd        <- 0.5                    # Proportional reduction in tests conducted on weekend days
input_pars$test_pars$hpi_mult          <- 1                      # Multiplier for testing probability on hpi quartile (1=lowest, 4 highest), so 1 means highest hpi 4 times more likely to get tested
input_pars$test_pars$income_mult       <- 1                      # Multiplier for testing probability on income category (1=lowest, 3 highest), so 1 means highest income 3x more likely to get tested
input_pars$test_pars$case_finding_mult <- 0.01                      # Per test available improvement in case finding meant to capture test availability, improved case identification, contact tracing, etc.
input_pars$test_pars$cont_mult         <- 10                     # Multiplier for testing probability for agents with known contact
input_pars$test_pars$symp_mult         <- 10                     # Multiplier for testing probability for time experiencing symptoms (increases probability by symp_mult*t_symptoms, e.g. longer period experiencing symptoms increases probability of testing)
input_pars$test_pars$res_mult          <- 100                    # Multiplier for testing probability for agents with known residential infection
input_pars$test_pars$nosymp_state_mult <- 1                      # Multiplier for testing probability for agents infected but not symptomatic (Ia and Ip)
input_pars$test_pars$symp_state_mult   <- 1000                   # Multiplier for testing probability for agents infected with symptoms (Im, Imh, Ih)
input_pars$test_pars$hosp_mult         <- 10000                  # Multiplier for testing probability for hospitalized agents
input_pars$test_pars$test.red          <- 0                      # Reduction in transmissibility for agents who have tested positive
input_pars$test_pars$essential_prob    <- 0.5                    # Multiplier on testing probability for essential workers (e.g. 0.5 would reduce test_pob by half)


# Delay between getting tested and getting results
test_delay_fx <- function(n_agents){
  rgamma(n_agents, 4, 2)
}

input_pars$test_pars$test_delay_fx     <- test_delay_fx

# Adaptive testing parameters ------------------------------
input_pars$adapt_pars <- list()

input_pars$adapt_pars$adapt_start              <- as.Date("2020-04-01")    # When adaptive testing begins
input_pars$adapt_pars$adapt_freq               <- 14                       # How frequently to reassess and place adaptive testing sites
input_pars$adapt_pars$adapt_site_geo           <- "ct"                     # Geography on which to base assessment and site placement (ct only option for now)
input_pars$adapt_pars$n_adapt_sites            <- 1                        # Number of adaptive sites to place
input_pars$adapt_pars$adapt_site_test_criteria <- "per_pos"                # Criteria on which to choose where to place sites
input_pars$adapt_pars$adapt_site_mult          <- 4                        # Multiplier on test probability for everyone in area with an adaptive test site

# Function for delay from test to disclosure for agents tested at adaptive site
adapt_site_delay_fx <- function(n_agents){
  rep(0.1, n_agents)
}

input_pars$adapt_pars$adapt_site_delay_fx      <- adapt_site_delay_fx            

# Vaccination parameters ------------------------------------
input_pars$vax_pars <- list()

input_pars$vax_pars$vax_start         <- as.Date("2020-12-15")   # When does vaccination begin?
input_pars$vax_pars$vax1_bta_red      <- 0.60                    # Reduction in probability of infection following first dose
input_pars$vax_pars$vax2_bta_red      <- 0.95                    # Reduction in probability of infection following second dose
input_pars$vax_pars$vax2_delay        <- 28                      # Delay between first and second doses

# Miscellaneous --------------------
input_pars$other_pars <- list()

mask_fx <- function(n_agents){
  rbeta(n_agents, 8, 2)
}

# hist(mask_fx(1e5))

social_fx <- function(n_agents){
  rbeta(n_agents, 2, 2)
}

# hist(social_fx(1e5))

input_pars$other_pars$mask_fx              <- mask_fx     # Generates probability individual agents will wear mask
input_pars$other_pars$mask_red             <- 0.6         # Reduction in transmission if infectious is wearing mask
input_pars$other_pars$social_fx            <- social_fx   # Generate sociality metrics
input_pars$other_pars$visitor_mult_testing <- 4           # Multiplier on true number of infectious agents compared to number confirmed positive in testing. Likely varies through time in reality, but here 4 assumes 1 in 4 agents who are infectious are actually confirmed positive
input_pars$other_pars$visitor_mult_sfgrph  <- 10          # Multiplier on number of visitors from safegraph to reflect true number of visitors (safegraph panel is ~10% of population)
input_pars$other_pars$q_prob_essential     <- 0.5         # Reduction in probability of qurantining if essential worker 

saveRDS(input_pars, here::here("data", "processed", "input_pars_Dec_start.rds"))
