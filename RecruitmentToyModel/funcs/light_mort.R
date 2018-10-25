

#getting solar radiation data for 1994 for bci

#bci_solar_1994 <- solar_bci[solar_bci$dates >= as.Date("1994-01-01") & solar_bci$dates <= as.Date("1994-12-31"),]

#attach(bci_solar_1994)

#plot(bci_solar_1994$dates, bci_solar_1994$MJm2)
#plot(bci_solar_1994$dates, FATES_vars$FSDS[1:365]/1e6)

#the mean TOC solar insolation per day per meter at bci in 1994 (Joules). 
#mean_TOC_1994 <- mean(bci_solar_1994$MJm2) * 1e6

light_mort <- function(light = 5000000, seedlings_C = 750000, PFT = "earlydi", N_smp = 200){
  
  pct_light <- (light / 15750113) * 100
  
  ifelse(PFT == "latedi" | PFT == "latedt", Z0_seedling <- Z0_seedling_l, Z0_seedling <- Z0_seedling_e)
  
  seedlings_N <- seedlings_C / Z0_seedling
  
  
  A <- P1light_mort[PFT]
  B <- P2light_mort[PFT]
        
  
  ifelse((test = PFT == "latedi" | PFT == "latedt" | (PFT == "earlydi" & pct_light <= 18.98) | (PFT == "earlydt" & pct_light <= 18.98)), 
         yes = Ml <- A * exp(-B*pct_light),
         no = Ml <- A * exp(-B*18.98))
  
  Pm_yr <- 1 - exp(-Ml*12)
  
  Pm_day <- Pm_yr / 365
  
  N_mort <- Pm_day * seedlings_N
  
  frac_mort <- (N_mort * Z0_seedling) / seedlings_C
  
  return(frac_mort)
}

#light_mort(light = 1e5)












