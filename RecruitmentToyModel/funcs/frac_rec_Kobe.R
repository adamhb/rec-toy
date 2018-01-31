#The goal of this script is to represent the relationship between light and seedling transition rates into the 1 cm size class. The function is "frac_rec_Kobe" and using light dependent growth rates to determine recruitment rates. The first part of this script is all allometric calculations for the purpose of converting growth rates to recruitment rates over a chosen time window. The second part determines the relationship between light and seedling transition rates into the 1 cm size class using light dependent growth rates as an intermediary step.




#function to calculate the fraction of the seed pool recruiting into the 1 cm size class each day.
frac_rec <- function(light = 20, t_window = 2, PFT = "early", seedlings_C = 750000, N_smp = 20){
  
  ifelse(PFT == "early", 
         BD_start <- 1.3,
         BD_start <- 1.46)
  
  
  ifelse(PFT == "early",
         BD_finish <- 2.32,
         BD_finish <- 2.29)
  
  BD_start <- as.numeric(BD_start)
  
  ifelse(PFT == "early", 
         seedlings_N <- seedlings_C / 162.2, #if early PFT
         seedlings_N <- seedlings_C / 175.35) #if late PFT
  

  #pct_light <-   #the corresponding percent light (percent of full sun) level at la selva during Kobe 1999 study 
  
  p1e <- 0.7618
  p1e_sd <- (0.9315 - p1e)/2
  
  p1l <- 0.2211
  p1l_sd <- (0.3483 - p1l) / 2
  
  p2e <- 0.0039
  p2e_sd <- 0.0051 - p2e
  
  p2l <- 0.0145
  p2l_sd <- 0.0278 - p2l
  
  
  ifelse(PFT == "early", p1 <- p1e, p1 <- p1l)
  ifelse(PFT == "early", p2 <- p2e, p2 <- p2l)
  ifelse(PFT == "early", p1_sd <- p1e_sd, p1_sd <- p1l_sd)
  ifelse(PFT == "early", p2_sd <- p2e_sd, p2_sd <- p2l_sd)

  
  p1 <- rnorm(mean = p1, sd = p1_sd, n = N_smp)
  
  p2 <- rnorm(mean = p2, sd = p2_sd, n = N_smp)
  
  G_L <- c()
  for(i in 1:length(p1)){
    G_L[i] <- (p1[i] * light) / ((p1[i]/p2[i]) + light)
  }
  
  Rad_start <- (BD_start * 10) / 2 #starting radius in mm
  
  Rad_after_1_yr <- (Rad_start) * (1 + G_L)^12
  
  BD_after_1_yr <- (Rad_after_1_yr/10) * 2
  
  BD_end_of_window <- (((BD_after_1_yr - BD_start) / 12) * t_window) + BD_start
  
  #a distribution of the radii of the stems (mm) after growth for the number of months in the window. 
  
  frac_recruiting_per_t_window <- sum(BD_end_of_window >= BD_finish) / N_smp
  
  daily_rec_rate <- frac_recruiting_per_t_window / (t_window*30.4)
  
  return(daily_rec_rate)
}






  
  







