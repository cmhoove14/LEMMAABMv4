# ---------------------------------------------------------
# Vaccination eligibility lists
# Chris Hoover Jan 2021
# ---------------------------------------------------------

vax_start <- as.Date("2020-12-15")

# Vaccination scenario for healthcare workers followed by 65+ followed by anyone -----------------
vax_65p <- list()
vax_65p$dates <- vax_start + c(0,35,77)
vax_65p$ages <- list()
  vax_65p$ages[[1]] <- c(15:85)
  vax_65p$ages[[2]] <- c(65:85)
  vax_65p$ages[[3]] <- c(15:85)

vax_65p$occps <- list()
  vax_65p$occps[[1]] <- 10
  vax_65p$occps[[2]] <- 0:23
  vax_65p$occps[[3]] <- 0:23
  
vax_65p$cts <- list()
  vax_65p$cts[[1]] <- 
  vax_65p$cts[[2]] <- 
  vax_65p$cts[[3]] <- 
  
  
saveRDS(vax_65p, here::here("data", "processed", "vax65p_scenario.rds"))  
  
# Vaccination scenario for healthcare workers followed by 65+ AND essential workers followed by anyone -----------------
vax_ess <- list()
vax_ess$dates <- vax_start + c(0,35,35,77)
vax_ess$ages <- list()
  vax_ess$ages[[1]] <- c(15:85)
  vax_ess$ages[[2]] <- c(65:85)
  vax_ess$ages[[3]] <- c(15:85)
  vax_ess$ages[[4]] <- c(15:85)
  
vax_ess$occps <- list()
  vax_ess$occps[[1]] <- 10
  vax_ess$occps[[2]] <- 0:23
  vax_ess$occps[[3]] <- c(6,12:14,18:23)
  vax_ess$occps[[4]] <- 0:23
  
vax_ess$cts <- list()
  vax_ess$cts[[1]] <- 
  vax_ess$cts[[2]] <- 
  vax_ess$cts[[3]] <- 
    
  
saveRDS(vax_ess, here::here("data", "processed", "vaxess_scenario.rds"))  
  