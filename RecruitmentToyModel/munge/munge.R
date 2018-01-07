#this munge script cleans raw data and creates time series, dataframes, and variables that are used in the toy model


#generating time series infrastructure data
#month_day <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

#month_series <- rep(c(rep(1, 31*48), rep(2, 28*48), rep(3, 31*48), rep(4, 30*48), rep(5, 31*48), rep(6, 30*48), rep(7, 31*48), rep(8, 31*48), rep(9, 30*48), rep(10, 31*48), rep(11, 30*48), rep(12, 31*48)), years)





#creating a time series of precipitation data at BCI for an average year.
monthly_rain_bci <- monthly_rain_bci$Average[-13]
daily_rain <- monthly_rain_bci/30.4

daily_rain_4_one_year <- c()
for(i in 1:length(daily_rain)){
  
  temp <- rep(daily_rain[i],30) 
  daily_rain_4_one_year <- append(daily_rain_4_one_year, temp)
  
}
daily_rain_4_one_year <- append(daily_rain_4_one_year, daily_rain_4_one_year[356:360])





#cleaning the PFT data and creating lists of early PFTs vs. late PFTs.
pfts$sp <- paste0(pfts$g," ",pfts$s)
names(pfts)[9] <- "Latin"
sp_code <- bci.spptable[,c(1,2)]
pfts <- merge(sp_code, pfts, by = "Latin")[,-c(3,4)]
Early <- pfts[pfts$pft == "e",]$sp
Late <- pfts[pfts$pft == "l",]$sp





#calculating the amount of biomass in each seedling
#using the allometric equations in Cole, T. G., & Ewel, J. J. (1995). Allometric equations for four valuable tropical tree species. Ecology, 229, 351–360. https://doi.org/10.1016/j.foreco.2006.04.017
#because they include equations specifically for individuals that are too small to have a dbh.
#using Cordia alliodora for all PFTs (sample for equations included height ranges at small as 10 cm tall.)
Z0_seedling_e <- seedling_BD2biomass(BD = BDe)
Z0_seedling_l <- seedling_BD2biomass(BD = BDl)





#cleaning the bci soil moisture data and creating 2 soil moisture datasets. 1) A full year of daily soil moisture content in matric potential from site 9 of the Lutz water catchment area in 2013 (a non-ENSO year), and 1998 a strong ENSO year.


date <- strptime(as.character(bci_soil_moisture$Date), format = "%m/%d/%Y")

date <- as.Date(date)

bci_soil_moisture$Date <- date

site_avgs <- bci_soil_moisture %>% #These are the average soil grav water contents (by dry) at each of the 10 sample locations 
  filter(Site == "LUTZ *", Depth.cm. == "0-10")%>%#, #Date >= "2013-1-1" & Date <= "2013-12-31") %>%
  group_by(Sample.) %>%
  summarise(mean(H2O_by_dry...))


#SOIL MOISTURE DATA FOR 2013

bci_soil_mois_2013_site9 <- bci_soil_moisture %>% 
  filter(Site == "LUTZ *", Depth.cm. == "0-10", Date >= "2013-1-1" & Date <= "2013-12-31", Sample. == 9) %>%
  mutate(day = format(Date, "%j")) %>%
  select(day, H2O_by_dry...)


bci_sm_2013_Lutz_9 <- predict(loess(data = bci_soil_mois_2013_site9, H2O_by_dry...~day), newdata = seq(from = 1, to = 365))

bci_sm_2013_Lutz_9[1:2] <- bci_sm_2013_Lutz_9[3]
bci_sm_2013_Lutz_9[365] <- bci_sm_2013_Lutz_9[364]

bci_sm_2013_Lutz_9 <- PTF_func(sgwc = (bci_sm_2013_Lutz_9/100))
pos <- bci_sm_2013_Lutz_9 >= 0 
bci_sm_2013_Lutz_9[pos] <- 0


#SOIL MOISTURE DATA FOR 1998

bci_soil_mois_1998_site9 <- bci_soil_moisture %>% 
  filter(Site == "LUTZ *", Depth.cm. == "0-10", Date >= "1998-1-1" & Date <= "1998-12-31", Sample. == 9) %>%
  mutate(day = format(Date, "%j")) %>%
  select(day, H2O_by_dry...)

bci_sm_1998_Lutz_9 <- predict(loess(data = bci_soil_mois_1998_site9, H2O_by_dry...~day), newdata = seq(from = 1, to = 365))

bci_sm_1998_Lutz_9[1:7] <- bci_sm_1998_Lutz_9[8]
bci_sm_1998_Lutz_9[356:365] <- bci_sm_1998_Lutz_9[354]

bci_sm_1998_Lutz_9 <- PTF_func(sgwc = (bci_sm_1998_Lutz_9/100))
pos <- bci_sm_1998_Lutz_9 >= 0 
bci_sm_1998_Lutz_9[pos] <- 0


#CLEANING AND ORGANIZING THE BCI PRECIP DATA

bci_precip

date_pre <- strptime(as.character(bci_precip$Date), format = "%m/%d/%Y")

date_pre <- as.Date(date_pre)

bci_precip$Date <- date_pre

#for a normal year 2013
bci_precip_2013 <- bci_precip %>% 
  filter(Date >= "2013-1-1" & Date <= "2013-12-31") %>%
  mutate(day = format(Date, "%j")) %>%
  select(day, rain_mm)

negvals <- bci_precip_2013$rain_mm < 0
bci_precip_2013$rain_mm[negvals] <- 0 

#for the el nino year - 1998
bci_precip_1998 <- bci_precip %>% 
  filter(Date >= "1998-1-1" & Date <= "1998-12-31") %>%
  mutate(day = format(Date, "%j")) %>%
  select(day, rain_mm)

negvals <- bci_precip_1998$rain_mm < 0
bci_precip_1998$rain_mm[negvals] <- 0 











