library(data.table)
library(tidyverse)
library(ggplot2)
library(here)
devtools::load_all()

# Get simulation output file
sims_run <- readRDS(here::here("data", "outputs", ))

# Get CA & SF data
source(here("data", "get", "COVID_CA_get_latest.R"))

# Get input pars
input_pars  <- readRDS(here::here("data/processed/input_pars.rds"))
unpack_list(input_pars)

# Additional Variables from simulation (make sure they match inputs from simulation)
states <- c("S", "E", "Ip", "Ia", "Im", "Imh", "Ih", "D", "R")

t.sim <- as.numeric((t.end-t0)/dt)
n_sims_per_par <- 5
bta_sweeps <- rep(c(0.125, 0.15, 0.175), each = n_sims_per_par)
sim.date <- "2020-10-27"

# Unpack and process sims -------------------
n_sims <- length(sims_run)

abm_cases <- bind_rows(lapply(1:n_sims, function(i){
  df <- as.data.frame(sims_run[[i]][["infections"]]) %>% 
    mutate(iter = i,
           bta = bta_sweeps[i])
  
  return(df)
  
}))

epi_curves <- bind_rows(lapply(1:n_sims, function(i){
  df <- as.data.frame(sims_run[[i]][["epi_curve"]])
  
  colnames(df) <- states
  
  df.out <- df %>% 
    mutate(I = Ip+Ia+Im+Imh+Ih,
           t_sim = (1:t.sim)*dt,
           Date = t0+t_sim,
           iter = i,
           bta = bta_sweeps[i])
  
  return(df.out)
}))

stay_homes <- bind_rows(lapply(1:n_sims, function(i){
  df <- data.frame(Date = rep(seq(t0, t.end-1, 1), each = 6),
                   home = sims_run[[i]][["stay_home"]]) %>% 
    mutate(iter = i,
           bta = bta_sweeps[i])
  
  return(df)
}))

quar_isos <- bind_rows(lapply(1:n_sims, function(i){
  df <- data.frame(Date = rep(seq(t0, t.end-1, 1), each = 6),
                   quar = sims_run[[i]][["quar_iso"]]) %>% 
    mutate(iter = i,
           bta = bta_sweeps[i])
}))

tests <- bind_rows(lapply(1:n_sims, function(i){
  df <- as.data.frame(sims_run[[i]][["linelist_tests"]]) %>% 
    mutate(iter = i,
           bta = bta_sweeps[i])
  
  return(df)
}))

tests_sum_by_date <- 
  tests %>% 
  group_by(iter, Date, bta) %>% 
  summarise(n_tests = n(),
            n_pos = sum(test_pos),
            n_Ih = sum(state == "Ih"),
            n_Im = sum(state %in% c("Im", "Imh")),
            n_Ipa = sum(state %in% c("Ip", "Ia")),
            per_pos = n_pos/n_tests)


rm("sims_run") ; gc()

# Plot incident cases  
abm_cases_day <- abm_cases %>% 
  group_by(iter, Date, bta) %>% 
  summarise(I = n())

abm_cases_day %>% 
  ggplot() +
  geom_line(aes(x = Date, y = I, group = factor(iter), col = factor(bta)),
            alpha = 0.6) +
  theme_bw()

# Plot hospitalizations 
epi_curves_H_sum <- epi_curves %>% 
  group_by(iter, Date, bta) %>% 
  summarise(H = mean(Ih)) 

epi_curves_H_sum %>% 
  ggplot() +
  geom_col(data = sf_hosp,
           aes(x = Date, y = HOSP_tot),
           col = "darkblue", fill = "blue", alpha = 0.4) +
  geom_line(aes(x = Date, y = H, group = factor(iter), col = factor(bta)),
            alpha = 0.5) +
  theme_bw()

#Percent of people staying home
stay_homes %>% 
  mutate(home7day = zoo::rollmean(home, 7, fill = NA, align = "left")) %>% 
  ggplot() +
  geom_line(aes(x = Date, y = home7day, group = iter, col = factor(bta))) +
  theme_bw()

#Percent of infected people staying home
quar_isos %>% 
  mutate(quar7day = zoo::rollmean(quar, 7, fill = NA, align = "left")) %>% 
  ggplot() +
  geom_line(aes(x = Date, y = quar7day, group = iter, col = factor(bta))) +
  theme_bw()

# Sums of testing data
tests_sum_by_date %>% 
  ggplot() +
  geom_col(data = sf_test,
           aes(x = Date, y = pos),
           fill = "grey50", alpha = 0.5) +
  geom_line(aes(x = as.Date(Date), y = n_pos, col = factor(bta), group = iter)) +
  theme_classic() +
  labs(y = "Positive Tests",
       title = "Confirmed cases compared to observed")

tests_sum_by_date %>% 
  ggplot() +
  geom_col(data = sf_test,
           aes(x = Date, y = pct),
           fill = "grey50", alpha = 0.5) +
  geom_line(aes(x = as.Date(Date), y = per_pos, col = factor(bta), group = iter)) +
  theme_classic() +
  labs(title = "Percent positive compared to observed")

# Positive tests by infection state
tests_sum_by_date %>% 
  pivot_longer(n_Ih:n_Ipa) %>% 
  ggplot() +
  geom_line(aes(x = Date, y = value, col = name, group = iter)) +
  theme_bw()

