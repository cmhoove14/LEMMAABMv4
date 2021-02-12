# ---------------------------------------
# Assess fit scores
# Chris Hoover Feb 2021
# ---------------------------------------
library(tidyverse)
library(data.table)

# Get CA & SF data
load(here::here("data", "get", "got", "CA_SF_data2021-02-10.Rdata"))

# Utils to grab simulation/fit files
sims_root <- here::here("Scratch")

get_sim <- function(sim){
  # Get sim file  
  sim_folder <- here::here(sims_root, sim)
  sim <- readRDS(paste0(sim_folder,"/",list.files(sim_folder)[1]))
  
  return(sim)
}

# Get input pars for reference dates
sim1 <- get_sim(1)
input_pars  <- sim1$input_pars
LEMMAABMv4::unpack_list(input_pars)

# Plot vars

# Compare daily hospitalizations ----------------

jpeg(here::here("Scratch", "hosp_sims_30sims_bta028.jpg"),
     height = 6, width = 8, units = "in", res = 200)
par(mar = c(3,2,2,0.25),
    mgp = c(1.25,0.5,0))
par(mfrow = c(3,1))

for(i in 0:2){
  #Plot of observed data with range due to PUI
  plot(x    = sf_hosp$Date[which(sf_hosp$Date <= t.end)], 
       y    = sf_hosp$HOSP_max[which(sf_hosp$Date <= t.end)], 
       type = "h", 
       lwd  = 1, 
       col  = "grey50",
       ylab = "Hospitalizations")
  
    points(x    = sf_hosp$Date[which(sf_hosp$Date <= t.end)], 
           y    = sf_hosp$HOSP_tot[which(sf_hosp$Date <= t.end)],
           type = "h")
  
  # Add sims  
    for(s in c((i*10+1):(i*10+10))){
      hosp_sim <- get_fit(s)$hosp
      lines(hosp_sim$Date, hosp_sim$N, col = "red", lwd = 0.5)
    }
}
  
dev.off()  

# Look at mean FOI through time
jpeg(here::here("Scratch", "FOIs_30sims_bta028.jpg"),
     height = 6, width = 8, units = "in", res = 200)
par(mar = c(3,2,2,0.25),
    mgp = c(1.25,0.5,0))
par(mfrow = c(3,1))


for(i in 0:2){
   
  plot(0, ylim = c(0,0.07), xlim = c(t0, t.end),
       type = "n",
       ylab = "FOI")
  
  # Add sims  
  for(s in c((i*10+1):(i*10+10))){
    # Get data
    fois <- get_sim(s)$FOIs[Date >= as.Date("2020-03-15"),] # Wait until things stabilize a bit
    colnames(fois)[2] <- "FOI"
    
    fois_wide <- dcast(fois, Date~loc_type, value.var = "FOI")
    lines(x    = fois_wide$Date, 
          y    = fois_wide$H,
          col = 1)
    
    lines(x    = fois_wide$Date, 
          y    = fois_wide$C,
          col = 2)
    
    lines(x    = fois_wide$Date, 
          y    = fois_wide$W,
          col = 3)
  }
  
  legend("topleft", lwd = 1, col = c(1,2,3),
         legend = c("Home", "Comm", "Work"),
         bty = "n", cex = 0.75)
}
  
dev.off()
