#this data script has 2 components:
  
  #1) default parameter values
  #2) load all necessary data

#1) default parameters

#Pre-NPP abiotic parameters (just for creating NPP in the toy model)#
  epsilon <- (0.99 /1e6) #the amount of carbon (g) converted to GPP per MJ of solar insolation. Source: https://www.ncbi.nlm.nih.gov/pubmed/18244937
  dry_season_length <- 120 #length of the dry season in days
  wet_season_length <- 365 - dry_season_length
  avg_solar_flux_wet <- 347.2 #from literature at bci I calculated 347.2
  avg_solar_flux_dry <- 462.963 ##from literature at bci I calculated 463.963
  total_area <- 10000 #the total area being modeled, assumes that total LAI on the model area cannot exceed 10,000
  

#Pre-NPP biotic parameters#
  Dmax <- c(575,321) #maximum diamater (mm)
      names(Dmax) <- c("early", "late")
  frac_repro <- 0.10 #For trees that are of reproductive status, this is the amount of available NPP for growth and reproduction (after all respiration) that is allocated to reproduction
  frac_NPP2GPP <- 0.3 #ratio of NPP to GPP
  frac_NPP2growth <- 0.5
  LAI_cohort <- 0.3 #A percent of total LAI in the model area. Its a cap on the total LAI that the cohort can take.
  totalbiomass2agb <- 1.7 #the ratio between total biomass and the aboveground biomass. This is important for converting between amount of carbon in an individual and the individuals dbh because I use an allometric equation developed by Chave that converts between AGB and dbh.


#Post-NPP biotic parameters#
background_seedling_mort <- 0.18 #from Dan Johnson 
frac_emerg <- 0.5 #the fraction of carbon that emerges from the seedbank at each timestep
decay_rate <- 0.51 #the annual decay rate of the seedbank
seed_frac <- 0.5 #fracion of reproductive carbon that goes to seeds
Z0 <- 200 #the amount of biomass it takes to make a new recruit at 1 cm of dbh
pct_light_seedling_layer <- 0.02
#user_defined_water_def <- c(0, 100000, 150000, 200000, 0, 0, 0,0,0,0,0,0)

#OTHER OMNIPRESENT PARAMETERS

c_frac <- 0.4 # the fraction of biomass that is carbon


#generating time series infrastructure
month_day <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

month_series <- rep(c(rep(1, 31*48), rep(2, 28*48), rep(3, 31*48), rep(4, 30*48), rep(5, 31*48), rep(6, 30*48), rep(7, 31*48), rep(8, 31*48), rep(9, 30*48), rep(10, 31*48), rep(11, 30*48), rep(12, 31*48)), years)




#Importing Data

monthly_rain_bci <- read.csv("bci_rain.csv")
engelbrecht_mort_data <- read.csv("engelbrecht_mort_data.csv")
pfts <- read.csv("PFTs.csv")
load("bci.spptable.RData")
sdbci <- read.csv("BCI_seedling_data_Johnson.csv")
breug <- read.csv("breug.csv")
breug_codes <- read.csv("sp_table_brueg.csv")
cole_table <- read.csv("cole_table.csv")[1:4,]
l_mort_params <- read.csv("light_mort_params_kobe.csv")
bci_soil_moisture <- read.csv("bci_soil_moisture.csv")
bci_precip <- read.csv("BCIprecipdata.csv")
