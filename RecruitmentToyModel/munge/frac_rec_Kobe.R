#The goal of this script is to represent the relationship between light and seedling transition rates into the 1 cm size class. The function is "frac_rec_Kobe" and using light dependent growth rates to determine recruitment rates. The first part of this script is all allometric calculations for the purpose of converting growth rates to recruitment rates over a chosen time window. The second part determines the relationship between light and seedling transition rates into the 1 cm size class using light dependent growth rates as an intermediary step.

###############ALLOMETRY#####################

#Step1. Determining the height of the seedling layer
#Seedling height (cm) is the mean seedling height in all of the date 1s for each PFT. Early vs. Late
Hght_E <- sdbci %>%
  filter(sp == Early) %>%
  select(sp, hght1) %>%
  summarise(H = mean(hght1)/10)

#species used to calc height for early PFTs, n = 12
sp_E <- sdbci %>%
  filter(sp == Early) %>%
  select(sp) %>%
  unique(.)

#height of late PFTs in cm
Hght_L <- sdbci %>%
  filter(sp == Late) %>%
  select(sp, hght1) %>%
  summarise(H = mean(hght1)/10)

#species used for late PFT, n = 29
sp_L <- sdbci %>%
  filter(sp == Late) %>%
  select(sp) %>%
  unique(.)


#STEP 2. Determining relationship between height and the basal diameter of each seedling from data from Breugal et. al.
#adding species names to the breug data
breug <- merge(breug, breug_codes)

#adding the bci codes to the breug data
breug <- merge(sp_code,breug, by = "Latin")

#taking out the basal diameter values above 8.
Hgt <- breug %>%
  select(sp.x, var, mean) %>%
  filter(var == "H")

BD <- breug %>%
  select(sp.x, var, mean) %>%
  filter(var == "BD")

dfBD_H <- data.frame(H = Hgt$mean, BD = BD$mean)

BD_H_mod <- lm(data = dfBD_H, BD ~ H)

#plot(Hgt$mean, BD$mean, xlab = "height (m)", ylab = "basal diameter (cm)", main = "Basal diameter vs. height; n = 49 sp.")
#abline(BD_H_mod)

#this function is not PFT specific. Only four species overlap. 
Hgh2BD <- function(H){
  BD <- coef(BD_H_mod)[2]*H + coef(BD_H_mod)[1]
  return(BD)
}

sp_overlap <- unique(breug$Latin) %in% pfts$Latin
sp_overlap_sp <- unique(breug$Latin)[sp_overlap]
sp_overlap_sp %in% pfts$Latin
pfts[pfts$Latin == "Brosimum alicastrum",]

#determining the basal diamter
BDe <- Hgh2BD(H = Hght_E/100)
BDl <- Hgh2BD(H = Hght_L/100)

#STEP 3. determining the height of a 1 cm size class individual from the below paper:

#Hubbell, S. P., & Condit, R. (1995). Diameter, Height, Crown, and Age Relationships in Eight Neotropical Tree Species. Ecology.

#writing down parameters for ln-ln relationship between dbh (mm) and H (m) found in the Hubbell and Condit paper (1995) above.
#parameters for species that are a proxy for late PFT
slope_l <- mean(0.684, 0.736) # mean of Prioria copaifera and Faramea occidentalis
int_l <- mean(-0.327, -0.492) # mean of Prioria copaifera and Faramea occidentalis

#parameters for species that is a proxy for early PFT
slope_e <- 0.640 #used data for this is Ocotea whitei
int_e <- -0.209

#this is the dbh to height allometric equation for those above species. The species parameters were in the middle of the range, so should be pretty representative. This data does include lower diameter individuals (i.e. 1 cm dbh). Its for the most common species on bci, but this could probably be improved by adding using a paratmers based off of canopy species when they are at a young age.

dbh2H <- function(PFT = "early", dbh_mm){
  ifelse(test = PFT =="early",
    lnH <- log(dbh_mm) * slope_e + int_e,
    lnH <- log(dbh_mm) * slope_l + int_l)
  return(exp(lnH))
}


Hgt_1_cm_e <- dbh2H(PFT = "early", dbh_mm = 10) #Height in the 1 cm size class for the early PFT is 3.54 m 
Hgt_1_cm_l <- dbh2H(PFT = "late", dbh_mm = 10) #Height in the 1 cm size class for the late PFT is 3.48


#STEP 4. Converting height of the 1 cm size class to basal diameter of the 1 cm size class using the equation from:
#King, D. A. (2015). Allometry of saplings and understorey trees of a panamanian forest, 4(1), 27–32.
#As an input this allometric equation takes H in meters. The output: diameter (cm) at 1/10th the tree height (m).

slp <- mean(c(0.813,0.755,0.670)) #this is the average of the parameter values for three canopy / subcanopy species: Alseis blackiana, Virola sebifera, Trichilia tuberculata

King_allom <- function(H){
  lnTD <-  log(H) * slp - 0.1
  return(exp(lnTD))
}

King_allom(H = 3.5)

BD_1_cm_e <- King_allom(H = Hgt_1_cm_e)
BD_1_cm_l <- King_allom(H = Hgt_1_cm_l)


#STEP 5, determining the amount of growth that the seedlings have to put on in units of basal diameter is
BD_delta_e <- BD_1_cm_e - BDe
BD_delta_l <- BD_1_cm_l - BDl

#STEP 6, determining the relationship between absolute light value and radial growth using Cecropia obtusifolia for early and Trophus racemosa for late (both of these species reach the canopy and have trees with dbh in the high 30s cm in the bci forest dynamics plot.

load('./data/bcifull.RData')

tropra <- bci.full %>%
  select(sp, dbh) %>%
  filter(sp == "tropra") %>%
  na.omit(.)

max(tropra$dbh) #max dbh in bci census data from tropra is 38.2 cm


#determining the amount of carbon in each seedling
#using the allometric equations in Cole, T. G., & Ewel, J. J. (1995). Allometric equations for four valuable tropical tree species. Ecology, 229, 351–360. https://doi.org/10.1016/j.foreco.2006.04.017
#because they include equations specifically for individuals that are too small to have a dbh.
#using Cordia alliodora for all PFTs (sample for equations included height ranges at small as 10 cm tall.)

#BD in cm to biomass (kg)
#see the func.R file to see or change this function

Z0_seedling_e <- seedling_BD2biomass(BD = BDe)
Z0_seedling_l <- seedling_BD2biomass(BD = BDl)


#function to calculate the fraction of the seed pool recruiting into the 1 cm size class each day.
frac_rec <- function(light = 20, t_window = 2, PFT = "early", seedlings_C = 750000, N_smp = 20){
  
  ifelse(PFT == "early", 
         BD_start <- BDe,
         BD_start <- BDl)
  
  
  ifelse(PFT == "early",
         BD_finish <- BD_1_cm_e,
         BD_finish <- BD_1_cm_l)
  
  BD_start <- as.numeric(BD_start)
  
  ifelse(PFT == "early", 
         seedlings_N <- seedlings_C / Z0_seedling_e, #if early PFT
         seedlings_N <- seedlings_C / Z0_seedling_l) #if late PFT
  

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






  
  







