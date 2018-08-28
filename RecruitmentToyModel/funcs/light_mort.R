

#getting solar radiation data for 1994 for bci

class(solar_bci$dates)

bci_solar_1994 <- solar_bci[solar_bci$dates >= as.Date("1994-01-01") & solar_bci$dates <= as.Date("1994-12-31"),]

attach(bci_solar_1994)

plot(bci_solar_1994$dates, bci_solar_1994$MJm2)
plot(bci_solar_1994$dates, FATES_vars$FSDS[1:365]/1e6)

#the mean TOC solar insolation per day per meter at bci in 1994 (Joules). 
mean_TOC_1994 <- mean(bci_solar_1994$MJm2) * 1e6

undebug(light_mort)

light_mort <- function(light = 5000000, seedlings_C = 750000, PFT = "early", N_smp = 200){
  
  pct_light <- (light / 15750113) * 100
  
  ifelse(PFT == "late", Z0_seedling <- Z0_seedling_l, Z0_seedling <- Z0_seedling_e)
  
  seedlings_N <- seedlings_C / Z0_seedling
  
  
  ifelse(PFT == "late", 
         A <- l_mort_params$value[1], 
         A <- l_mort_params$value[7])
  
  ifelse(PFT == "late", 
         B <- l_mort_params$value[4],
         B <- l_mort_params$value[10])
  
  
  ifelse(PFT == "late", 
         A_sd <- (l_mort_params$value[2] - l_mort_params$value[1])/2, 
         A_sd <- (l_mort_params$value[8] - l_mort_params$value[7])/2)
  
  ifelse(PFT == "late", 
         B_sd <- (l_mort_params$value[5] - l_mort_params$value[4])/2, 
         B_sd <-(l_mort_params$value[11] - l_mort_params$value[10])/2)
  
  #A <- rnorm(mean = A, sd = A_sd, n = 1)
  
  #B <- rnorm(mean = A, sd = B_sd, n = 1)
  
  ifelse((test = PFT == "late" | (PFT == "early" & pct_light <= 18.98)), 
         yes = Ml <- A * exp(-B*pct_light),
         no = Ml <- A * exp(-B*18.98))
  
  Pm_yr <- 1 - exp(-Ml*12)
  
  Pm_day <- Pm_yr / 365
  
  N_mort <- Pm_day * seedlings_N
  
  frac_mort <- (N_mort * Z0_seedling) / seedlings_C
  
  return(frac_mort)
}

light_mort(light = 1e4)

debug(light_mort)





undebug(light_mort)


c(1e6, 2e6, 3e6, 4e6, 8e6, 10e6, 15e6, 20e6)
light_mort(light = , PFT = "early", N_smp = 20) 


light_mort(light = 1:100, PFT = "early")





