library(tidyverse)
library(magrittr)
library(ggplot2)
library(lme4)

#creating a serial number for each observation
seed_dyn$OBnum <- seq(1:length(seed_dyn$TAGF))

#converting the shade index to relative irradiance using the approach Ruger et al 2009 used with the 1993 RI data. 

#creating the data to convert shd index into IR
shd_quantiles <- quantile(seed_dyn$shd, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = FALSE,
         names = TRUE, type = 7)
shd_quantiles2 <- shd_quantiles^2
shd_quantiles3 <- shd_quantiles^3
lnRI_quantiles <- c(0, -3.1, -3.8, -4.1, -4.5, -5.2 , -6.1) #this I believe is from emprical data. See ruger on this.
l_shd_data <- data.frame(shd = shd_quantiles, shd2 = shd_quantiles2, shd3 = shd_quantiles3, lnRI = lnRI_quantiles)



#creating linear conversion to RI
l_shd_m <- lm(data = l_shd_data, lnRI ~ shd)
summary(l_shd_m)
ggplot(data = l_shd_data, mapping = aes(x = shd, y = lnRI)) +
  geom_point() +
  geom_line(mapping = aes(x = shd, y = predict(l_shd_m, newdata = l_shd_data)))

#creating a quadratic conversion to RI
l_shd_m_quad <- lm(data = l_shd_data, lnRI ~ shd + shd2)
summary(l_shd_m_quad)

#creating third order polynomial fit of shade index to RI
l_shd_m_3 <- lm(data = l_shd_data, lnRI ~ shd + shd2 + shd3)
summary(l_shd_m_3)


#THE CUBIC MODEL WAS THE BEST FIT.


#converting shade index to RI with cubic conversion
seed_dyn$RI3 <- exp(predict(l_shd_m_3, newdata = data.frame(x = 1:length(seed_dyn$shd), shd = seed_dyn[,"shd"], shd2 = (seed_dyn[,"shd"])^2, shd3 = (seed_dyn[,"shd"])^3)))


#THESE BELOW CONVERSIONS ARE SHOWN JUST FOR COMPARISON
#converting shade index to RI with linear conversion
seed_dyn$RI <- exp(predict(l_shd_m, newdata = data.frame(x = 1:length(seed_dyn$shd), shd = seed_dyn[,"shd"])))

#converting shade index to RI with quadratic conversion

seed_dyn$RI_quad <- exp(predict(l_shd_m_quad, newdata = data.frame(x = 1:length(seed_dyn$shd), shd = seed_dyn[,"shd"], shd2 = (seed_dyn[,"shd"])^2)))




#adding in the real light values to the seedling dynamics data using the RI3 numbers
seed_dyn$start.date <- as.Date(seed_dyn$start, origin = as.Date("2001-1-15", format = "%Y-%m-%d"))
seed_dyn$stop.date <- as.Date(seed_dyn$stop, origin = as.Date("2001-1-15", format = "%Y-%m-%d"))



#creating a variable in the seedling dynamics data to indicate a recruit
R <- as.numeric(as.numeric(seed_dyn$d) >= 10)
R[is.na(R)] <- 0
seed_dyn$R <- R

#adding pfts to the seedling dynamics data
#cleaning the PFT data and creating lists of early PFTs vs. late PFTs.
pfts$sp <- paste0(pfts$g," ",pfts$s)
names(pfts)[9] <- "Latin"
sp_code <- bci.spptable[,c(1,2)]
pfts <- merge(sp_code, pfts, by = "Latin")[,-c(3,4)]
Early <- pfts[pfts$pft == "e",]$sp
Late <- pfts[pfts$pft == "l",]$sp


#Checking that pft designated species are in the seedling dynamcis plots
pfts$SPP <- toupper(pfts$sp)
seed_spp <- unique(seed_dyn$SPP)
pfts$SPP %in% seed_spp


#adding pft factor levels to the seedling dynamimcs data
seed_dyn$pft <- rep("unk", length(seed_dyn$TAGF))
seed_dyn[seed_dyn$SPP %in% pfts[pfts$pft == "e",]$SPP,]$pft <- "e"
seed_dyn[seed_dyn$SPP %in% pfts[pfts$pft == "l",]$SPP,]$pft <- "l"
seed_dyn[seed_dyn$SPP %in% pfts[pfts$pft == "ml",]$SPP,]$pft <- "ml"
seed_dyn[seed_dyn$SPP %in% pfts[pfts$pft == "me",]$SPP,]$pft <- "me"

seed_dyn$pft <- factor(seed_dyn$pft, levels = c("e", "me", "ml", "l", "unk"))


#removing observations where a recruit was censused more than once
rec_dupes <- seed_dyn %>%
  select(OBnum, TAGF, census, start, stop, Q20P5, x, y, h, d, R, mort, shd, dmin, ra, ra.d) %>%
  arrange(TAGF, census) %>%
  filter(R == 1) %>%
  mutate(dupes = duplicated(TAGF)) %>%
  filter(dupes == TRUE) %>%
  .$OBnum

seed_dyn <- seed_dyn[!seed_dyn$OBnum %in% rec_dupes,]


#reorganizing the data so that it can be explored and queried statistically for light dependent recruitment rates. For early PFTS
seed_dyn_c_e <- seed_dyn %>%
  filter(pft == "e") %>%
  select(TAGF, census, start, start.date, stop, stop.date, Q20, Q20P5, R, shd, RI, RI_quad, RI3, pft) %>%
  group_by(census, Q20P5) %>%
  summarise(start.x = mean(start), start.date.x = min(start.date), stop.x = mean(stop), stop.date.x = min(stop.date), shade = mean(shd), RI = mean(RI), RI_quad = mean(RI_quad), RI3 = mean(RI3), Nt_1 = length(unique(TAGF)), N_trans = sum(R)) %>%
  mutate(t_rate = N_trans / Nt_1, daily_t_rate = t_rate/(stop.x-start.x))


#cleaning the top of canopy solar insolation data at bci so that it can be covnerted to photon flux density and appended to the seedling dynamics data.
dates <- as.Date(solar_bci$ndays, origin = strptime(as.character("1980-1-1"), format = "%Y-%m-%d"), format = "%Y-%m-%d")
solar_bci$dates <- dates

solar_bci_c <- dplyr::filter(solar_bci, dates >= as.Date("2000-1-1"))
solar_bci_jdate <- as.numeric(solar_bci_c$dates, origin = as.Date("2001-1-15", format = "%Y-%m-%d"))

#converting solar irradiance in MJ per day to photon flux density (assuming a 12 hour day) averaged over those 12 hours. The conversion factor I used to concert wm2 to PFD comes from: http://www.egc.com/useful_info_lighting.php
solar_bci_c_w_PFD <- solar_bci_c %>%
  mutate(PFD = MJm2 * 1e6 / (12*60*60) * 4.57)




#creating a function that calculates the mean top of canopy photon flux density given specific start and stop dates. The function is used as part of a code chunk (below) to calculate mean TOC PFD values over specific censuses that are quadrat specific.
solar_calc <- function(data, start.date, stop.date){
  
  light_data <- data %>%
    filter(dates >= start.date & dates <= stop.date)
  
  light_val <- mean(light_data$PFD)
  
  return(light_val)
}


#switch the line below on or off depending if you want to generate new TOC PFD vals for each census interval (the census intervals are quadrat specific) from this script or just read them in from the RDS file that was already generated.

#saveRDS(PFDvals, "PFDvals.RDS")

PFDvals <- readRDS("PFDvals.RDS")




#PFDvals <- c()
#for(i in 1:length(seed_dyn_c_e$census)){
  
  #strdate <- seed_dyn_c_e$start.date.x[i]
  #stpdate <- seed_dyn_c_e$stop.date.x[i]
  
  #PFDvals[i] <- solar_calc(data = solar_bci_c_w_PFD, start.date = strdate, stop.date = stpdate)
  #print(i/length(seed_dyn_c_e$census))
#}



#adding the TOC PFD values to the seedling dynamcis data.
seed_dyn_c_e$TOC_PFD <- PFDvals

#multiplying TOC PFD by relative irradiance in each quadrat to get the average PFD in each quadrat averaged over each census interval. 
seed_dyn_c_e <- seed_dyn_c_e %>%
  mutate(PFD = TOC_PFD * RI3)

#adding the PFT variable to this form of the cleaned seedling dyanmics data
seed_dyn_c_e$pft <- rep("e", length(seed_dyn_c_e$census))







#########LATE PFTS####################





#selecting the late PFTs.

seed_dyn_c_l <- seed_dyn %>%
  filter(pft == "l") %>%
  select(TAGF, census, start, start.date, stop, stop.date, Q20, Q20P5, R, shd, RI, RI_quad, RI3, pft) %>%
  group_by(census, Q20P5) %>%
  summarise(start.x = mean(start), start.date.x = min(start.date), stop.x = mean(stop), stop.date.x = min(stop.date), shade = mean(shd), RI = mean(RI), RI_quad = mean(RI_quad), RI3 = mean(RI3), Nt_1 = length(unique(TAGF)), N_trans = sum(R)) %>%
  mutate(t_rate = N_trans / Nt_1, daily_t_rate = t_rate/(stop.x-start.x))





#switch the line below on or off depending if you want to generate new TOC PFD vals for each census interval (the census intervals are quadrat specific) from this script or just read them in from the RDS file that was already generated.

#saveRDS(PFDvals_l, file = "PFDvals_l")
PFDvals_l <- readRDS(file = "PFDvals_l")



#The below code chunk generates TOC PFD values for each census interval in each quadrat for LATE PFTs.

#PFDvals_l <- c()
#for(i in 1:length(seed_dyn_c_l$census)){
  #strdate <- seed_dyn_c_l$start.date.x[i]
  #stpdate <- seed_dyn_c_l$stop.date.x[i]
  
  #PFDvals_l[i] <- solar_calc(data = solar_bci_c_w_PFD, start.date = strdate, stop.date = stpdate)
  #print(i/length(seed_dyn_c_l$census))
#}


#adding the TOC PFD data to the seedling dynamcis data
seed_dyn_c_l$TOC_PFD <- PFDvals_l


##multiplying TOC PFD by relative irradiance in each quadrat to get the average PFD in each quadrat averaged over each census interval. 
seed_dyn_c_l <- seed_dyn_c_l %>%
  mutate(PFD = TOC_PFD * RI3)

#adding the PFT variable to this form of the cleaned seedling dyanmics data
seed_dyn_c_l$pft <- rep("l", length(seed_dyn_c_l$census))

#combinin the early and late seedling dynamics data together
seed_dyn_c_e_l <- rbind(seed_dyn_c_l, seed_dyn_c_e)






#reorganizing the seedling dynamics data in a similar way to above, but for all PFTS. This allows statistical analysis of drivers of seedling transition rates.
seed_dyn_c <- seed_dyn %>%
  select(TAGF, census, start, start.date, stop, stop.date, Q20, Q20P5, R, shd, RI, RI_quad, RI3, pft) %>%
  group_by(census, Q20P5) %>%
  summarise(start.x = mean(start), start.date.x = min(start.date), stop.x = mean(stop), stop.date.x = min(stop.date), shade = mean(shd), RI = mean(RI), RI_quad = mean(RI_quad), RI3 = mean(RI3), Nt_1 = length(unique(TAGF)), N_trans = sum(R)) %>%
  mutate(t_rate = N_trans / Nt_1, daily_t_rate = t_rate/(stop.x-start.x))





#THE BELOW CODE CHUNKS DO THE SAME THINGS AS THE CODE ABOVE, BUT FOR ALL individuals regardless of PFT. 
#saveRDS(PFDvals_all, file = "PFDvals_all")
PFDvals_all <- readRDS(file = "PFDvals_all")
#PFDvals_all <- c()
#for(i in 1:length(seed_dyn_c$census)){
  #strdate <- seed_dyn_c$start.date.x[i]
  #stpdate <- seed_dyn_c$stop.date.x[i]
  
  #PFDvals_all[i] <- solar_calc(data = solar_bci_c_w_PFD, start.date = strdate, stop.date = stpdate)
  #print(i/length(seed_dyn_c$census))
#}
seed_dyn_c$TOC_PFD <- PFDvals_all

seed_dyn_c <- seed_dyn_c %>%
  mutate(PFD = TOC_PFD * RI3)




























