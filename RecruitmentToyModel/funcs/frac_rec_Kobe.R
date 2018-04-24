#The goal of this script is to represent the relationship between light and seedling transition rates into the 1 cm size class. The function is "frac_rec_Kobe" and using light dependent growth rates to determine recruitment rates. The first part of this script is all allometric calculations for the purpose of converting growth rates to recruitment rates over a chosen time window. The second part determines the relationship between light and seedling transition rates into the 1 cm size class using light dependent growth rates as an intermediary step.


#function to calculate the fraction of the seed pool recruiting into the 1 cm size class each day.
frac_rec <- function(light = 20, t_window.x = c(2,2), PFT = "early", seedlings_C = 750000, N_smp = 20, p1e.x = 0.7618, p1l.x = 0.2211, p2e.x = 0.0039, p2l.x = 0.0145, g_param = 1.00){
  
  #ifelse(test = light < 50, yes = t_window <- t_window.x, no = t_window <- t_window.x * 0.7)
  
  names(g_param) <- c("early", "late")
  
  ifelse(test = PFT == "early", yes =  t_window <- t_window.x / (log(light)), no = t_window <- t_window.x)
  
  
  names(t_window) <- c("early", "late")
  
  ifelse(PFT == "early", 
         BD_start <- 1.3,
         BD_start <- 1.46)
  
  
  ifelse(PFT == "early",
         BD_finish <- 2.32,
         BD_finish <- 2.29)
  
  BD_start <- as.numeric(BD_start)
  
  ifelse(PFT == "early", 
         seedlings_N <- seedlings_C / 65, #assuming 60 grams of carbon in a seedling of basal diameter 1.3 cm. if early PFT
         seedlings_N <- seedlings_C / 70) #assuming 70 grams of carbon in a seedling of basal diameter 1.46 cm if late PFT.
  

  #pct_light <-   #the corresponding percent light (percent of full sun) level at la selva during Kobe 1999 study 
  
  p1e <- p1e.x
  p1e_sd <- p1l_se * p1e
  
  p1l <- p1l.x
  p1l_sd <- (p1l_se * p1l) / 2
  
  p2e <- p2e.x
  p2e_sd <- p2e_se * p2e
  
  p2l <- p2l.x
  p2l_sd <- (p2l_se * p2l) /2
  
  
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
  
  Rad_after_1_yr <- (Rad_start) * (g_param[PFT] + G_L)^12
  
  BD_after_1_yr <- (Rad_after_1_yr/10) * 2
  
  BD_end_of_window <- (((BD_after_1_yr - BD_start) / 12) * t_window[PFT]) + BD_start
  
  #a distribution of the radii of the stems (mm) after growth for the number of months in the window. 
  
  frac_recruiting_per_t_window <- sum(BD_end_of_window >= BD_finish) / N_smp
  
  daily_rec_rate <- frac_recruiting_per_t_window / (t_window[PFT]*30.4)
  
  return(daily_rec_rate)
}




#recruitment
#BENCHMARKING
#creating benchmarking data, late PFT
l  <- 1:100

a <- log10(0.011*25*0.9) 
b <- 0.23

Rs <- 10^(a + b * log10(l)) * 400
plot(l,Rs, main = "Recruits per ha per year; Late PFT" ,xlab = "relative irradiance", ylab = "number of recruits")

#late_rec_bench <- Rs

#recruitment
#PARAMETERIZATION, late
lx <- 1:100
N_recs <- c()
frac <- c()
  for(i in 1:length(lx)){
    N_recs[i] <- frac_rec(light = lx[i], t_window = c(5,8), PFT = "late", N_smp = 50, p1e.x = 0.718, p2e.x = 0.0043, p1l.x = 0.2211, p2l.x = 0.0145, g_param = c(1.07, 1.0)) * 25000 / (Z0*0.4) * 365
    frac[i] <- frac_rec(light = lx[i], t_window = c(5,8), PFT = "late", N_smp = 50, p1e.x = 0.718, p2e.x = 0.0043, p1l.x = 0.2211, p2l.x = 0.0145, g_param = c(1.07,1.0))
  }
 
plot(lx, frac*5*30)
plot(lx, N_recs, main = "Recruits per ha per year; Late PFT" ,xlab = "relative irradiance", ylab = "number of recruits")







#early
#BENCHMARKING early recruitment with ruger model
l  <- 1:100
a <- log10(0.011*25*0.1) 
b <- 1.082

Rse <- 10^(a + b * log10(l)) * 400
plot(l,Rse, main = "Recruits per ha per year; Early PFT" ,xlab = "relative irradiance", ylab = "number of recruits")

#early_rec_bench <- Rse

#CORRECT PARAMETERIZATION earlylx <- 1:100
N_recs <- c()
frac <- c()
for(i in 1:length(lx)){
  N_recs[i] <- frac_rec(light = lx[i], t_window = c(5,8), PFT = "early", N_smp = 50, p1e.x = 0.7618, p2e.x = 0.0039, p1l.x = 0.2211, p2l.x = 0.0145, g_param = c(1.07, 1.0)) * 25000 / (Z0*0.4) * 365
  frac[i] <- frac_rec(light = lx[i], t_window = c(5,8), PFT = "early", N_smp = 50, p1e.x = 0.7618, p2e.x = 0.0039, p1l.x = 0.2211, p2l.x = 0.0145, g_param = c(1.07,1.0))
}

plot(lx, frac*5*30)
plot(lx, N_recs, main = "Recruits per ha per year; Early PFT" ,xlab = "relative irradiance", ylab = "number of recruits")










#debug(frac_rec)


#frac_rec(light = 80, t_window = c(2,2), PFT = "early", N_smp = 50, p1e.x = 0.718, p2e.x = 0.0043, p1l.x = 0.2211, p2l.x = 0.0145, g_param = 1.05)




#frac_rec(light = 3, t_window = c(2,2), PFT = "late", N_smp = 200) * 25000 / (Z0*0.4) * 365



#exploring different parameterizations on the light dependence of growth


#early
light <- seq(from = 1, to = 100, length = 100)
p1 <- 0.718
p2 <- 0.004
G_L <- (p1 * light) / ((p1/p2) + light)
Radial_growth <- (5 * (1.0 + G_L)^12) - 0.7
plot(light, Radial_growth)

#late
light <- seq(from = 1, to = 100, length = 100)
p1 <- 0.2211
p2 <- 0.0145
G_L <- (p1 * light) / ((p1/p2) + light)
Radial_growth <- (5 * (1.0 + G_L)^12) - 0.7
plot(light, Radial_growth)






G_L[i] <- (p1[i] * light) / ((p1[i]/p2[i]) + light)

#1.375



























