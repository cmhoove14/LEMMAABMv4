# ---------------------------------------
# Plot best fitting runs that were rerun in duplicate
# Chris Hoover Feb 2021
# ---------------------------------------
library(tidyverse)
library(ggplot2)
library(data.table)

# Get fits and lhs to refence fits and pars
fits <- readRDS(here::here("data", "processed", "LHS_Fits1_summary.rds"))
lhs  <- readRDS(here::here("data/processed/Calibration_LHS_Wynton.rds")) 

# Remove fits that resulted in 0 Hospitalizations
fits <- fits[fits$hosp_fit >0,]

# Sims with best fits to individual categories and best overall
best_hosp           <- fits$sim[which.min(fits$hosp_fit)]
best10_norm         <- fits$sim[which(fits$overall_fit_norm <= quantile(fits$overall_fit_norm, 10/nrow(fits), na.rm = T))]

lhs_reruns <- lhs[c(best_hosp, best10_norm),]
lhs_reruns_expand <- rbind(lhs_reruns[rep(1:nrow(lhs_reruns),each=10),])

rerun_sims <- rep(c(best_hosp, best10_norm),each=10)

# Get CA & SF data
load(here::here("data", "get", "got", "CA_SF_data2021-02-10.Rdata"))

# Util to grab simulation/fit files
sims_root <- here::here("data","outputs","Calibration_Reruns")

get_sim <- function(sim){
  # Get sim file  
  sim_folder <- here::here(sims_root, sim)
  sim <- readRDS(paste0(sim_folder,"/",list.files(sim_folder)[1]))
  
  return(sim)
}

# Plot hospitalizations -------------------
hosp_sums <- bind_rows(lapply(0:10, function(i){
# Get all simulations with same parameter set  
  hosp_sims <- bind_rows(lapply(c((i*10+1):(i*10+10)), function(j){
    sim <- get_sim(j)
    sim_hosp <- as_tibble(sim$epi_curve[state == "Ih",]) %>% 
      mutate(Date = as.Date(as.character(date)),
             sim = rerun_sims[j])
    return(sim_hosp)
  }))
#Summarise simulations from same parameter set  
  hosp_sims_sum <- hosp_sims %>% 
    group_by(Date, sim) %>% 
    summarise(H_mean = mean(N),
              H_med = median(N),
              H_q25 = quantile(N, 0.25),
              H_q75 = quantile(N, 0.75),
              H_sd = sd(N))
  
  return(hosp_sims_sum)
}))


hosp_reps_plot <- ggplot() +
  geom_col(data = sf_hosp,
           aes(x = Date, y = HOSP_max),
           col = "darkblue", fill = "lightblue", alpha = 0.4) +
  geom_col(data = sf_hosp,
           aes(x = Date, y = HOSP_tot),
           col = "darkblue", fill = "blue", alpha = 0.6) +
  geom_line(data = hosp_sums,
            aes(x = Date, y = H_med), 
            col = "darkred", size = 1.2) +
  geom_ribbon(data = hosp_sums,
              aes(x = Date, ymin = H_q25, ymax = H_q75),
              col = "red", fill = "red", alpha = 0.3) +
  facet_wrap("sim", nrow = 6, ncol = 2) +
  theme_classic() +
  #  ylim(c(0,200)) +
  labs(title = "Hospitalizations fits in best parameter sets")

  ggsave(plot = hosp_reps_plot, 
         filename = here::here("Plots/LHS_Calibration/Fits1/replicate_best_hosp_10x.jpg"),
         width = 8, height = 8)
  
# Plot deaths -------------------
  dths_sums <- bind_rows(lapply(0:10, function(i){
    # Get all simulations with same parameter set  
    dths_sims <- bind_rows(lapply(c((i*10+1):(i*10+10)), function(j){
      sim <- get_sim(j)
      sim_dths <- as_tibble(sim$agents[state == "D", c("id", "sex", "age", "race", "t_death")]) %>% 
        mutate(
          tod = as.Date(as.character(zoo::as.Date.numeric(t_death))),
          sim = rerun_sims[j],
          dths = 1
        ) %>% 
        padr::pad() %>% 
        mutate(dths = replace_na(dths, 0),
               cum_dths = cumsum(dths))
      
      return(sim_dths)
    }))
    #Summarise simulations from same parameter set  
    dths_sims_sum <- dths_sims %>% 
      group_by(tod, sim) %>% 
      summarise(D_mean = mean(cum_dths),
                D_med = median(cum_dths),
                D_q25 = quantile(cum_dths, 0.25),
                D_q75 = quantile(cum_dths, 0.75),
                D_sd = sd(cum_dths))
    
    return(dths_sims_sum)
  }))
  
dths_reps_plot <-   ggplot() +
    geom_col(data = sf_case,
             aes(x = Date, y = cum_death),
             col = "black", fill = "grey50", alpha = 0.5) +
    geom_line(data = dths_sums,
              aes(x = tod, y = D_med), 
              col = "darkred", size = 1.2) +
    geom_ribbon(data = dths_sums,
                aes(x = tod, ymin = D_q25, ymax = D_q75),
                col = "red", fill = "red", alpha = 0.3) +
    facet_wrap("sim", nrow = 6, ncol = 2) +
    theme_classic() +
    #  ylim(c(0,200)) +
    labs(title = "Cumulative deaths fits in best parameter sets")
  
  ggsave(plot = dths_reps_plot,
         filename = here::here("Plots/LHS_Calibration/Fits1/replicate_best_dths_10x.jpg"),
         width = 8, height = 8)
  