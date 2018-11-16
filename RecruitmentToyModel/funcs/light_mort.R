

#getting solar radiation data for 1994 for bci

#bci_solar_1994 <- solar_bci[solar_bci$dates >= as.Date("1994-01-01") & solar_bci$dates <= as.Date("1994-12-31"),]

#attach(bci_solar_1994)

#plot(bci_solar_1994$dates, bci_solar_1994$MJm2)
#plot(bci_solar_1994$dates, FATES_vars$FSDS[1:365]/1e6)

#the mean TOC solar insolation per day per meter at bci in 1994 (Joules). 
#mean_TOC_1994 <- mean(bci_solar_1994$MJm2) * 1e6



#light is going to be total solar load (MJ per square meter) per square meter at the forest floor over the prior two 60 days (time window based on Augspurger)

light_mort <- function(light = 5000000*60, seedpool.x = 750000){
  
  pct_light <- (light / (15750113 * 90)) * 100 #the percent RI equivalent at Kobe's site in Costa Rica
  
  seedlings_N <- seedpool.x / Z0_seedling[PFT]
  
  A <- P1light_mort[PFT]
  B <- P2light_mort[PFT]
        
  ifelse((test = PFT == "latedi" | PFT == "latedt" | (PFT == "earlydi" & pct_light <= 18.98) | (PFT == "earlydt" & pct_light <= 18.98)), 
         yes = Ml <- A * exp(-B*pct_light),
         no = Ml <- A * exp(-B*18.98))
  
  Pm_yr <- 1 - exp(-Ml*3)
  
  Pm_day <- Pm_yr / 90
  
  #N_mort <- Pm_day * seedlings_N
  
  #frac_mort <- (N_mort * Z0_seedling[PFT]) / seedpool.x
  
  return(Pm_day)
}

light_mort(light = 5000000*90)












