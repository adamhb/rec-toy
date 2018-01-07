light_mort <- function(light = 20, seedlings_C = 750000, PFT = "early", N_smp = 200){
  
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
  
  
  ifelse(PFT == "late" | (PFT == "early" & light <= 18.98),
         Ml <- A * exp(-B*light),
         Ml <- A * exp(-B*18.98))
  
  Pm_yr <- 1 - exp(-Ml*12)
  
  Pm_day <- Pm_yr / 365
  
  N_mort <- Pm_day * seedlings_N
  
  frac_mort <- (N_mort * Z0_seedling) / seedlings_C
  
  return(frac_mort)
}







