# ---------------------------------------------------------
# Vaccination eligibility lists
# Chris Hoover Jan 2021
# ---------------------------------------------------------

vax_start <- as.Date("2020-12-15")

agents <- readRDS(here::here("data", "processed", "SF_agents_processed.rds"))
  agent_cts <- unique(agents$ct)

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
  
# Default to all cts  
vax_65p$cts <- list()
  vax_65p$cts[[1]] <- agent_cts
  vax_65p$cts[[2]] <- agent_cts
  vax_65p$cts[[3]] <- agent_cts

vax_65p$type <- list()  
  vax_65p$type[[1]] <- "S"
  vax_65p$type[[2]] <- "S"
  vax_65p$type[[3]] <- "S"

saveRDS(vax_65p, here::here("data", "processed", "vax65p_scenario.rds"))  
  
# Vaccination scenario for healthcare workers followed by 65+ AND essential workers followed by anyone -----------------
vax_essS <- list()
vax_essS$dates <- vax_start + c(0,35,35,77)
vax_essS$ages <- list()
  vax_essS$ages[[1]] <- c(15:85)
  vax_essS$ages[[2]] <- c(65:85)
  vax_essS$ages[[3]] <- c(15:85)
  vax_essS$ages[[4]] <- c(15:85)
  
vax_essS$occps <- list()
  vax_essS$occps[[1]] <- 10
  vax_essS$occps[[2]] <- 0:23
  vax_essS$occps[[3]] <- c(6,12:14,18:23)
  vax_essS$occps[[4]] <- 0:23
  
# Default to all cts  
vax_essS$cts <- list()
  vax_essS$cts[[1]] <- agent_cts
  vax_essS$cts[[2]] <- agent_cts
  vax_essS$cts[[3]] <- agent_cts
  vax_essS$cts[[4]] <- agent_cts
  
vax_essS$type <- list()  
  vax_essS$type[[1]] <- "S"
  vax_essS$type[[2]] <- "S"
  vax_essS$type[[3]] <- "S"
  vax_essS$type[[4]] <- "S"

saveRDS(vax_essS, here::here("data", "processed", "vaxessS_scenario.rds"))  
  
# Vaccination scenario for healthcare workers followed by 65+ AND essential workers in high risk cts ("A" for adpative assignment) followed by anyone -----------------
vax_essA <- list()
vax_essA$dates <- vax_start + c(0,35,35,77)
vax_essA$ages <- list()
  vax_essA$ages[[1]] <- c(15:85)
  vax_essA$ages[[2]] <- c(65:85)
  vax_essA$ages[[3]] <- c(15:85)
  vax_essA$ages[[4]] <- c(15:85)

vax_essA$occps <- list()
  vax_essA$occps[[1]] <- 10
  vax_essA$occps[[2]] <- 0:23
  vax_essA$occps[[3]] <- c(6,12:14,18:23)
  vax_essA$occps[[4]] <- 0:23

# Default to all cts  
vax_essA$cts <- list()
  vax_essA$cts[[1]] <- agent_cts
  vax_essA$cts[[2]] <- agent_cts
  vax_essA$cts[[3]] <- agent_cts
  vax_essA$cts[[4]] <- agent_cts

vax_essA$type <- list()  
  vax_essA$type[[1]] <- "S"
  vax_essA$type[[2]] <- "S"
  vax_essA$type[[3]] <- "A"
  vax_essA$type[[4]] <- "S"

saveRDS(vax_essA, here::here("data", "processed", "vaxessA_scenario.rds"))  


# Vaccination scenario for healthcare workers followed by 65+ AND anyone in high risk groups (note: this mush be modeled by assigning "A" in vaccination variable call to covid_abm function) followed by anyone -----------------
vax_geoA <- list()
vax_geoA$dates <- vax_start + c(0,35,35,77)
vax_geoA$ages <- list()
  vax_geoA$ages[[1]] <- c(15:85)
  vax_geoA$ages[[2]] <- c(65:85)
  vax_geoA$ages[[3]] <- c(15:85)
  vax_geoA$ages[[4]] <- c(15:85)
  
vax_geoA$occps <- list()
  vax_geoA$occps[[1]] <- 10
  vax_geoA$occps[[2]] <- 0:23
  vax_geoA$occps[[3]] <- 0:23
  vax_geoA$occps[[4]] <- 0:23
  
# Default to all cts  
vax_geoA$cts <- list()
  vax_geoA$cts[[1]] <- agent_cts
  vax_geoA$cts[[2]] <- agent_cts
  vax_geoA$cts[[3]] <- agent_cts
  vax_geoA$cts[[4]] <- agent_cts
  
vax_geoA$type <- list()  
  vax_geoA$type[[1]] <- "S"
  vax_geoA$type[[2]] <- "S"
  vax_geoA$type[[3]] <- "A"
  vax_geoA$type[[4]] <- "S"
  
  
saveRDS(vax_geoA, here::here("data", "processed", "vaxgeoA_scenario.rds"))  
  