#Driver Data
library(ncdf4)
library(ncdf.tools)
library(magrittr)
library(tidyverse)
library(lubridate)
library(scales)
library(plotrix)
library(ggplot2)
library(reshape2)

#unzip the input data
#untar(tarfile = "C:/Users/ahanb/OneDrive/Documents/RecruitmentToyModel/RecruitmentToyModel/FATES_output_7_16_2018/july14th-bci-variable-sla.tgz")

#set path to driver data
driver_data_path <- "C:/Users/ahanb/OneDrive/Documents/data/"
#set path to subfunction scripts
subfunctions_path <- "C:/Users/ahanb/OneDrive/Documents/rec_submodel/rec_submodel_scripts/RecruitmentToyModel/funcs"

bci_precip_new <- read.csv(paste0(driver_data_path,"bci_driver_data/bci_rain_new.csv")) #read in the input precipitation data

bci_precip_new$date <- strptime(as.character(bci_precip_new$date), format = "%m/%d/%Y") %>% as.Date(.)


#add working directory to the location of the FATES input driver data
setwd(paste0(driver_data_path,"bci_driver_data/FATES_output_May_2018/"))


#file1 <- nc_open(dir()[1])
#infoNcdfDims(dir()[1], extended = TRUE)


#soil matric potential (mm) = MPa * 1e5
SMP <- c()
for(i in dir()){
  open_nc_file <- nc_open(i)
  tmp <- ncvar_get(open_nc_file,varid = "SMP")[3,]
  SMP <- append(SMP,tmp)
  nc_close(open_nc_file)
}

#plot(SMP)
#SMP <- SMP * 2

#SMP_dry <- SMP * 1.6

NPP <- c()
for(i in dir()){
  open_nc_file <- nc_open(i)
  tmp <- ncvar_get(open_nc_file,varid = "NPP")
  NPP <- append(NPP,tmp)
  nc_close(open_nc_file)
}


FSDS <- c()
for(i in dir()){
  open_nc_file <- nc_open(i)
  tmp <- ncvar_get(open_nc_file,varid = "FSDS")
  FSDS <- append(FSDS,tmp)
  nc_close(open_nc_file)
}


ED_biomass <- c()
for(i in dir()){
  open_nc_file <- nc_open(i)
  tmp <- ncvar_get(open_nc_file,varid = "ED_biomass")
  ED_biomass <- append(ED_biomass,tmp)
  nc_close(open_nc_file)
}
# 
ED_balive <- c()
for(i in dir()){
  open_nc_file <- nc_open(i)
  tmp <- ncvar_get(open_nc_file,varid = "ED_balive")
  ED_balive <- append(ED_balive,tmp)
  nc_close(open_nc_file)
}
# 
GPP <- c()
for(i in dir()){
  open_nc_file <- nc_open(i)
  tmp <- ncvar_get(open_nc_file,varid = "GPP")
  GPP <- append(GPP,tmp)
  nc_close(open_nc_file)
}


TLAI <- c()
for(i in dir()){
  open_nc_file <- nc_open(i)
  tmp <- ncvar_get(open_nc_file,varid = "TLAI")
  TLAI <- append(TLAI,tmp)
  nc_close(open_nc_file)
}


SABG <- c()
for(i in dir()){
  open_nc_file <- nc_open(i)
  tmp <- ncvar_get(open_nc_file,varid = "SABG")
  SABG <- append(SABG,tmp)
  nc_close(open_nc_file)
}





##################
##################
##################





#set up the model run
#setwd("C:/Users/ahanb/OneDrive/Documents/RecruitmentToyModel/RecruitmentToyModel")

run_name <- "bci_3pct_light_test_new_emerg"

#parameters
#site and scenario params
avg_precip <- 71 #precipitation in mm over two weeks (the annual average)
avg_SMP <- -60326 #
avg_l <- 61 #the average total solar radiation load (MJ per m2) at the forest floor over 6 months (annual average)
pft_names <- c("earlydi", "earlydt", "latedi", "latedt")
percent_light <- 0.03
dbh.x <- 500 #dbh in mm
N_co.x <- 800  #the number of individuals in a cohort
model_area <- 10000 #area in square meters

#parameters for allocation to reproduction
#Dmax <- dmax_vals #these are the default parameters based off of BCI FDP data
Dmax <- c(934.2857, 846.3182, 556.7179, 561.3786) #maximum diamater (mm)
names(Dmax) <- pft_names
n_PFTs <- 4
frac_repro <- c(0.1,0.1,0.1,0.1)#the fraction of NPP that gets allocated to reproduction
names(frac_repro) <- pft_names
seed_frac <- 0.5 #the fraction of reproductive carbon that gets allocated to seeds

#seed bank and emergence
decay_rate <- 0.51 #the annual decay rate of the seedbank
#beta_emerg <- c(0.074, 0.056, 0.03617053, 0.07876964)
a_emerg <- rep(0.5/365, 4) #the average daily fraction of the seedbank that moves to the seedling pool (annual average)
names(a_emerg) <- pft_names
b_emerg <- c(1.05,1.05, 1, 1) #the precipitation response parameter for emergence
names(b_emerg) <- pft_names


background_seedling_mort <- default_background_seedling_mort #see script called background_seedling_mort.R for derivation
#background_seedling_mort <- 0.2875200 0.2875200 0.2094371 0.2094371

#seedling mort-H20
names(background_seedling_mort) <- pft_names

P1H20 <- rep(c(coef(DI_dr_lm)[2], coef(DT_dr_lm)[2]),2) 
names(P1H20) <- pft_names
P2H20 <- rep(c(coef(DI_dr_lm)[1], coef(DT_dr_lm)[1]),2) 
names(P2H20) <- pft_names

#P1H20 <- c(4.97e-08, 5.07e-08, 4.97e-08, 5.07e-08)
#names(P1H20) <- pft_names
#P2H20 <- c(-3.93e-17, -2.45e-17, -3.93e-17, -2.45e-17)
#names(P2H20) <- pft_names
thresh.xx <- c(-167973.2, -350000.0, -167973.2, -350000.0) #the water moisture threshold (mm of head of water) when plants start to stress
names(thresh.xx) <- pft_names
window.x <- 18*7 #the number of days over which to calculate the moisture deficit

#seedling light mort
P1light_mort <- c(0.752, 0.752, 0.0241, 0.0241)
names(P1light_mort) <- pft_names
P2light_mort <- c(0.1368, 0.1368, 0.0404, 0.0404)
names(P2light_mort) <- pft_names
Z0_seedling <- c(35,35,40,40)
names(Z0_seedling) <- pft_names


#transition from seedling to adult recruit
#a_rec <- a_rec_default #this is the daily beta rec default from liza comitas data
a_rec <- rep(0.0002,4)
names(a_rec) <- pft_names
b_rec <- c(1.0653, 1.0653, 0.8615, 0.8615)
names(b_rec) <- pft_names
Z0 <- 165

#initial conditions
seedbank_0 = 22750 #the initial mass of carbon in the seedbank (g C)
seedpool_0 = 12000 #the initial mass of carbon in the seedling pool (g C)
litter_0 = 10000

plot_input_vars <- "Y"

#source the model subfunctions
setwd(subfunctions_path)
for(i in 1:length(grep(x = dir(), pattern = ".R"))){
source(dir()[grep(x = dir(), pattern = ".R")][i])
}


#creating a dataframe of the input variables from FATES

FATES_vars <- data.frame(hour = hour, SMP = SMP, FSDS = FSDS, ED_biomass = ED_biomass, NPP = NPP ,TLAI = TLAI, SABG = SABG, GPP = GPP)
hour <- seq(from = 0, to = length(FATES_vars$SMP) -1)
#hour <- 0:113880


FATES_vars <- FATES_vars %>% 
  mutate(date = as_datetime(hour*3600, origin = "2003-01-01")) %>% #adding the date and time
  mutate(day = floor(as.numeric(date)/86400)-floor(as.numeric(date[1])/86400)) %>% # adding the day
  mutate_at(.,.cols = vars(NPP), .funs = function(x){x*3600}) %>% #converting NPP from a flux every second to an hourly flux. 
  mutate_at(.,.cols = vars(FSDS), .funs = function(x){x*3600}) #convert FSDS from a flux every second to an hourly flux (J hour -1 m -1)

  
FATES_vars <- FATES_vars %>% 
  rbind(.,.) %>% #duplicating the data to account for multiple PFTs
  rbind(.,.) %>%
  add_column(pft = c(rep(pft_names[1], (length(.$hour)/4)), rep(pft_names[2], (length(.$hour)/4)), rep(pft_names[3], length(.$hour)/4), rep(pft_names[4], length(.$hour)/4))) %>% #adding pfts
  add_column(dbh = rep(dbh.x, length(.$hour))) %>% #adding the dbh
  add_column(N_co = rep(N_co.x, length(.$hour))) #adding the number in the cohort
  

FATES_state_vars <- FATES_vars %>%
  select(day, date, pft, dbh, N_co, SMP) %>%
  group_by(day,pft) %>%
  summarise_all(.,mean) %>% ungroup(.) %>% arrange(pft, day) %>% #converting to daily time step
  add_column(precip = rep(bci_precip_new[bci_precip_new$date >= as.Date("2003-01-01") & bci_precip_new$date <= as.Date("2015-12-29"),2], 4))

               
FATES_flux_vars <- FATES_vars %>%
  select(day, pft, NPP, FSDS) %>%
  group_by(day,pft) %>%
  summarise_all(.,sum) %>% ungroup(.) %>% arrange(pft, day) #summing hourly fluxes. FSDS = Joules per day, NPP = gC per day




FATES_vars <- cbind(FATES_state_vars, FATES_flux_vars[,-c(1,2)]) #combining the state vars and the flux vars
  #add_column(water_def = append(def_func(soil_moist = FATES_state_vars$SMP, thresh.x = thresh.xx, window = window.x, dPFT = "DI"), def_func(soil_moist = FATES_state_vars$SMP, thresh.x = thresh.xx, window = window.x, dPFT = "DT"))) %>% #adding water deficit


#generating water deficit values
water_def <- c()
for(PFT in pft_names){
  water_def <- append(water_def, def_func(soil_moist = FATES_state_vars[FATES_state_vars$pft == PFT,]$SMP, thresh.x = thresh.xx[PFT], window = window.x))
}
FATES_vars$water_def <- water_def

FATES_vars <- FATES_vars %>%
  mutate(H20_mort_rate = base::mapply(FUN = H20_mort, deficit_days = FATES_vars$water_def, pft.x = FATES_vars$pft))


FATES_vars <- FATES_vars %>%
  mutate(e_frac = base::mapply(FUN = efrac, N = (FATES_vars$N_co), co_dbh_ind = (FATES_vars$dbh), PFT = FATES_vars$pft)) %>% #adding the "effective fraction" of NPP that gets allocated to reproduction in each time step
  mutate(c_repro = e_frac * NPP * model_area/n_PFTs) %>%  #calculating the carbon allocated to reproduction in each daily timestep for the whole model area (1 hectare)
  mutate_at(.tbl = .,.cols = vars(c_repro), .funs = function(x){ifelse(x < 0, 0, x)}) %>% 
  arrange(., day,pft) %>%
  mutate(light = FSDS * percent_light / 1e6) 


output <- list()

j <- 1

for(PFT in pft_names){
    
    
    input_vars <- FATES_vars %>% filter(pft == PFT)
    
    #pfts
    PFT_record <- c()
    
    #seed bank dynamics
    seedbank <- c()
    frac_emerging <- c()
    
    #seedling pool dynamics
    seedpool <- c()
    light_mort_rate <- c()
    frac_rec.t <- c()
    
    #recruitment and litter pool
    R <- c()
    N <- c()
    litter <- c()
    
    
    #initializing
    #pfts
    lPFT[1] <- PFT.xxx 
    dPFT[1] <- dPFT.xxx
    PFT_record[1] <- PFT
    
  
    #seedbank dynamics
    seedbank[1] <- seedbank_0
    frac_emerging[1] <- 0 
    
    #seedling pool dynamics
    seedpool[1] <- seedpool_0
    light_mort_rate[1] <- 0
    frac_rec.t <- 0
    
    #recruitment and litter pool
    R[1] <- 0
    N[1] <- 0
    litter[1] <- litter_0
   
    
    for(i in 1:(nrow(input_vars)-1)){
      
      #recording PFTs
      PFT_record[i+1] <- PFT
      
      
      #allocation dynamics are captured above, outside the for loops, because they don't rely on previous time steps
      
      
      #seedbank dynamics
      seedbank[i+1] <- seedbank[i] %>%
        - (decay_rate/365 * seedbank[i]) %>%
        - emerg_func(SMP.x = (ifelse(test= i > 14, yes = mean(input_vars$SMP[(i-13):i]), no = input_vars$SMP[i])), seedbank.x = seedbank[i])$C_emerg %>%
        + (seed_frac * input_vars$c_repro[i])
      
      frac_emerging[i+1] <- emerg_func(SMP.x = (ifelse(test= i > 14, yes = mean(input_vars$SMP[(i-13):i]), no = input_vars$SMP[i])), seedbank.x = seedbank[i])$frac_emerg
      
      #seedling pool dynamics
      
      #if(i == 3653){browser()}
      
      seedpool[i+1] <- seedpool[i] %>%
        + (emerg_func(SMP.x = (ifelse(test= i > 14, yes = mean(input_vars$SMP[(i-13):i]), no = input_vars$SMP[i])), seedbank.x = seedbank[i])$C_emerg)  %>%
        - ((light_mort(light = ifelse(test = i > 90, yes = sum(input_vars$light[(i-90):i] +0.0001), no = input_vars$light[i]*90 + 0.00001), seedpool.x = seedpool[i])) * seedpool[i]) %>%
        - (input_vars$H20_mort_rate[i] * seedpool[i]) %>%
        - (seedpool[i]*background_seedling_mort[PFT]/365) %>%
        - (rec_func(l = ifelse(test = i > 183, yes = sum(input_vars$light[(i-183):i] +0.0001), no = sum(input_vars$light[(i+183):i]) + 0.0001), seedpool.x = seedpool[i])$C_rec)
      
      light_mort_rate[i+1] <- (light_mort(light = ifelse(test = i > 90, yes = sum(input_vars$light[(i-90):i] +0.0001), no = input_vars$light[i]*90 + 0.00001), seedpool.x = seedpool[i]))
      
      frac_rec.t[i+1] <- rec_func(l = ifelse(test = i > 183, yes = sum(input_vars$light[(i-183):i] +0.0001), no = sum(input_vars$light[(i+183):i]) + 0.0001), seedpool.x = seedpool[i])$frac_rec
      
      
      #recruitment and litter pool dynamics
      R[i+1] <- rec_func(l = ifelse(test = i > 183, yes = sum(input_vars$light[(i-183):i] +0.0001), no = sum(input_vars$light[(i+183):i]) + 0.0001), seedpool.x = seedpool[i])$N_rec
      
      N[i+1] <- N[i] %>%
        + (R[i+1])
      
      
      litter[i+1] <- litter[i] %>%
        + ((1-seed_frac) * input_vars$c_repro[i]) %>%
        + (decay_rate/365 * seedbank[i]) %>%
        + ((light_mort(light = ifelse(test = i > 90, yes = sum(input_vars$light[(i-90):i] +0.0001), no = input_vars$light[i]*90 + 0.00001), seedpool.x = seedpool[i])) * seedpool[i]) %>%
        + (seedpool[i] * background_seedling_mort[PFT]/365) %>%
        + (H20_mort(deficit_days = input_vars$water_def[i], pft.x = PFT)*seedpool[i]) 
      
      
    }
    
    
    output[[j]] <- data.frame(PFT_record = PFT_record,
                              seedbank = seedbank, 
                              frac_emerging = frac_emerging,
                              seedpool = seedpool,
                              light_mort_rate = light_mort_rate,
                              frac_rec.t = frac_rec.t,
                              R = R, 
                              N = N,
                              litter = litter) %>% cbind(input_vars,.) %>% select(day, date, pft, dbh, N_co, NPP, e_frac, c_repro, seedbank, precip, frac_emerging, seedpool, FSDS, light, light_mort_rate, frac_rec.t, SMP, water_def, H20_mort_rate, R, N, litter)
    
    print(paste0(j," of 4 is done!"))  
    j <- j+1
    
    
  }


full_output <- rbind(output[[1]], output[[2]], output[[3]], output[[4]])



str(full_output)






















































#create a folder for the output
dir.create(path = paste0("C:/Users/ahanb/OneDrive/Documents/rec_submodel/Submodel_Output/",run_name), showWarnings = T)

#record the params
paramsOFrun <- data.frame(param_names = c("model_area", "dbh.x", "N_co.x", "Dmax", "frac_repro", "seed_frac","decay_rate", "a_emerg", "b_emerg", "a_rec", "b_rec", "percent_light", "thresh", "window.x", "seedbank_0", "seedpool_0", "litter_0"), param_vals = c(model_area, dbh.x, N_co.x, paste0(Dmax, collapse = ","),paste0(frac_repro, collapse = ","), seed_frac, decay_rate, paste0(a_emerg, collapse = ","), paste0(b_emerg, collapse = ","), paste0(a_rec, collapse = ","), paste0(b_rec, collapse = ","), percent_light, paste0(thresh.xx, collapse = ","), window.x, seedbank_0, seedpool_0, litter_0))

#put the params file in the output folder
write.csv(paramsOFrun, file = paste0("C:/Users/ahanb/OneDrive/Documents/rec_submodel/Submodel_Output/",run_name,"/params.csv"))


setwd(paste0("C:/Users/ahanb/OneDrive/Documents/rec_submodel/Submodel_Output/",run_name))



# NPP for each PFT 

  
  NPP_g <- ggplot(data = full_output, aes(x = as.Date(date), y = NPP*10000, color = pft)) +
    labs(title = "NPP") +
    theme_classic()+
    smooth_line +
    year_axis +
    ylab(expression(paste("NPP ", "(g C ha"^"-1","day"^"-1",")")))+
    xlab(bquote('year'))+
    adams_theme +
    theme(legend.position = "none")
  
  png(paste0(getwd(),"/01_NPP.png"), height=5, width=8, units="in", res = 100)
  NPP_g
  dev.off()
  
  


#graphing the fraction of NPP going to reproduction
p1 <- ggplot(data = full_output, aes(x = as.Date(date), y = e_frac, color = pft)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(bquote('fraction of NPP going to reproduction'))+
  xlab(bquote('year'))+
  geom_text(mapping = aes(x = median(as.Date(date)), y = 0.2), data = full_output, label = paste("early",full_output$e_frac %>% head(.,n=1)), color = "black") +
  geom_text(mapping = aes(x = median(as.Date(date)), y = 0.17), data = full_output, label = paste("late",full_output$e_frac %>% tail(.,n=1)), color = "black") +
  labs(title = "The frac. of NPP allocated to repro.") +
  theme_classic()+
  adams_theme+
  scale_color_manual(values = pft.cols)
  
  #xlab("Production Type")+
  #ylab(expression(paste("Kg ", "CO" ["2(eq)"], " per Kg fresh tomato" )))
  
png(paste0(getwd(),"/02_e_frac.png"), height=5, width=8, units="in", res = 100)
p1
dev.off()



#graphing the carbon allocated to reproduction
p2 <- ggplot(data = full_output, aes(x = as.Date(date), y = c_repro, color = pft)) +
  smooth_line +
  year_axis +
  ylab(expression(paste("carbon for repro.", "(g C day"^"-1","ha"^"-1",")")))+
  xlab(bquote('year'))+
  labs(title = "C allocated to reproduction per day") +
  theme_classic() +
  adams_theme +
  theme(legend.position = "none")+
  scale_color_manual(values = pft.cols)

png(paste0(getwd(),"/03_C_for_repro.png"), height=5, width=8, units="in", res = 100)
p2
dev.off()



#graphing seedbank size
p3 <- ggplot(data = full_output, aes(x = as.Date(date), y = seedbank, color = pft)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(expression(paste("seedbank size ", " (g C ","ha"^"-1",")")))+
  xlab(bquote('year'))+
  labs(title = "Seed bank size (g C)") +
  theme_classic() +
  adams_theme +
  theme(legend.position = "none")+
  scale_color_manual(values = pft.cols)

png(paste0(getwd(),"/04_SeedBank.png"), height=5, width=8, units="in", res = 100)
p3
dev.off()


#add precip to this on a second axis
#graphing the fraction of the seedbank emerging each day
p4 <- ggplot(data = full_output, aes(x = as.Date(date), y = frac_emerging, color = pft)) +
  geom_smooth(size = 1.8, method = "loess", span = .01, se = F, color = "black")+
  year_axis +
  ylab(expression(paste("frac. of seedbank emerging"," (day)"^"-1")))+
  xlab(bquote('year'))+
  labs(title = expression(paste("Seedling Emergence"," (day)"^"-1"))) +
  theme_classic() +
  adams_theme + 
  theme(legend.position = "none")+
  scale_color_manual(values = pft.cols)

png(paste0(getwd(),"/05_frac_emerging.png"), height=5, width=8, units="in", res = 100)
p4
dev.off()


#graphing the seedling pool
p5 <- ggplot(data = full_output %>% filter(date >= as.POSIXct("2005-01-01")), aes(x = as.Date(date), y = seedpool, color = pft)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(bquote('seedling pool size (gC)'))+
  xlab(bquote('year'))+
  labs(title = 'seedling pool size (gC)') +
  scale_color_manual(values = pft.cols)+
  theme_classic() +
  adams_theme
  


png(paste0(getwd(),"/06_seedling_pool.png"), height=5, width=8, units="in", res = 100)
p5
dev.off()


#graphing the light mortality rate
p6 <- ggplot(data = full_output %>% filter(date >= as.POSIXct("2005-01-01")), aes(x = as.Date(date), y = light_mort_rate, color = pft)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(bquote('daily mort rate (% of seedling pool)'))+
  xlab(bquote('year'))+
  #scale_color_manual(values = c("darkolivegreen4", "midnightblue"))+
  labs(title = 'light-dep. seedling mortality (3% light)') +
  theme_classic() +
  adams_theme +
  scale_color_manual(values = pft.cols)


png(paste0(getwd(),"/07_light_mort_3pct.png"), height=5, width=8, units="in", res = 100)
p6
dev.off()



#graphing H20 mortality rate
p7 <- ggplot(data = full_output %>% filter(date >= as.POSIXct("2005-01-01")), aes(x = as.Date(date), y = H20_mort_rate, color = pft)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(expression(paste('H20 Mort. Rate'," (day)"^"-1"))) +
  #ylab(bquote('H20 mort rate'))+
  xlab(bquote('year'))+
  labs(title = 'H20 mort rate') +
  #scale_color_manual(values = c("darkolivegreen2", "darkolivegreen4"))+
  theme_classic() +
  adams_theme+
  scale_color_manual(values = pft.cols)


png(paste0(getwd(),"/08_H20_mort_rate.png"), height=5, width=8, units="in", res = 100)
p7
dev.off()


precip_g <- ggplot(data = full_output, aes(x = as.Date(date), y = precip, color = pft)) +
  labs(title = "02_precip.") +
  theme_classic()+
  #geom_point()+
  smooth_line +
  year_axis +
  ylab(expression(paste("daily precip (mm)")))+
  xlab(bquote('year'))+
  adams_theme +
  theme(legend.position = "none")+
  scale_color_manual(values = (rep("blue",4)))

png(paste0(getwd(),"/09_precip.png"), height=5, width=8, units="in", res = 100)
precip_g
dev.off()



SMP_MPa_g <- ggplot(data = full_output %>% filter(date >= "2005-01-01"), aes(x = as.Date(date), y = SMP/1e5)) +
  labs(title = "Soil Matric Potential (MPa)") +
  theme_classic()+
  geom_line(size = 1.8, color = "black")+
  #smooth_line +
  year_axis +
  ylab(expression(paste("SMP (MPa)")))+
  xlab(bquote('year'))+
  adams_theme +
  theme(legend.position = "none") +
  geom_hline(yintercept = thresh.xx[1]/1e5, size = 1.8, color = "darkolivegreen2")+
  geom_hline(yintercept = thresh.xx[2]/1e5, size = 1.8, color = "darkolivegreen4")#+
#geom_text(mapping = aes(x = median(as.Date(date)), y = thresh.xx[1]/1e5), data = full_output, label = "DI threshold", color = "black") +
#geom_text(mapping = aes(x = median(as.Date(date)), y = thresh.xx[2]/1e5), data = full_output, label = "DT threshold", color = "black")



png(paste0(getwd(),"/10_SMP.png"), height=5, width=8, units="in", res = 100)
SMP_MPa_g
dev.off()



water_def_g <- ggplot(data = full_output, aes(x = as.Date(date), y = water_def, color = pft)) +
  labs(title = "Water Deficit Days") +
  theme_classic()+
  geom_line(size = 1.8)+
  smooth_line +
  year_axis +
  ylab(expression(paste("Deficit Days (cum. sum of SMP deficit)")))+
  xlab(bquote('year'))+
  adams_theme +
  #theme(legend.position = "none") +
  geom_hline(yintercept = thresh.xx[1]/1e5)+
  geom_hline(yintercept = thresh.xx[2]/1e5)#+
#geom_text(mapping = aes(x = median(as.Date(date)), y = thresh.xx[1]/1e5), data = full_output, label = "DI threshold", color = "black") +
#geom_text(mapping = aes(x = median(as.Date(date)), y = thresh.xx[2]/1e5), data = full_output, label = "DT threshold", color = "black")


png(paste0(getwd(),"/11_water_def.png"), height=5, width=8, units="in", res = 100)
water_def_g
dev.off()





#graphing the fraction recruiting from the seedling pool to the adult size class
p8 <- ggplot(data = full_output, aes(x = as.Date(date), y = frac_rec.t, color = pft)) +
  smooth_line +
  year_axis +
  ylab(expression(paste('rec. rate (% of seedling pool'," day"^"-1",")")))+
  xlab(bquote('year'))+
  labs(title = 'seedling recruitment rate (%)') +
  theme_classic() +
  adams_theme +
  scale_color_manual(values = pft.cols)

png(paste0(getwd(),"/12_fraction_recruiting.png"), height=5, width=8, units="in", res = 100)
p8
dev.off()



#graphing the daily recruitment rate without the total
p9 <- full_output %>% arrange(desc(pft)) %>% ggplot( aes(x = as.Date(date), y = R*365, color = pft)) +
  smoother_line +
  year_axis +
  ylab(expression(paste('N recruits'," ha"^"-1"," year"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'annual number of recruits') +
  theme_classic() +
  adams_theme +
  scale_color_manual(values = pft.cols)

#+
  #geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), 
              #mapping = aes(x = as.Date(date), y = total_R, color = pft),
              #size = 1.8, method = "loess", span = .01, se = F) 



png(paste0(getwd(),"/13_annual_N_recruits.png"), height=5, width=8, units="in", res = 100)
p9
dev.off()





#graphing the annual recruitment rate with the total

p10 <- full_output %>% arrange(desc(pft)) %>% ggplot( aes(x = as.Date(date), y = R*365, color = pft)) +
  smoother_line +
  year_axis +
  ylab(expression(paste('N recruits'," ha"^"-1"," yr"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'annual number of recruits') +
  theme_classic() +
  adams_theme +
  geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), 
              mapping = aes(x = as.Date(date), y = total_R*365, color = pft),
              size = 1.8, method = "loess", span = .1, se = F)+
  scale_color_manual(values = append(pft.cols, "black"))
  
  

png(paste0(getwd(),"/14_daily_N_recruits_w_total.png"), height=5, width=8, units="in", res = 100)
p10
dev.off()



#creating table of the annual number of recruits per year per PFT
N_recs_per_year_pfts <- full_output %>% mutate(year = substring(text = as.character(date), first = 1, last = 4)) %>% group_by(year, pft) %>% summarise(N_rec = sum(R)) 

N_recs_per_year_pfts$year <- as.Date(paste0((as.numeric(N_recs_per_year_pfts$year)+1), "-01-01"))



##################################################
#submodel annual sum of recruitment with benchmark
####################################################
#creating data of the number of recruits per year

#graph of the annual sums
Submodel_annual_rec <- ggplot() +
  smoother_line +
  ylab(expression(paste('N recruits'," ha"^"-1"," yr"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'New Submodel') +
  #geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), mapping = aes(x = as.Date(date), y = total_R*365, color = pft), size = 1.8, method = "loess", span = .1, se = F) +
  geom_point(data = N_recs_per_year_pfts, mapping = aes(x = year, y = N_rec, color = pft), size = 8) +
  geom_segment(aes(x=as.Date("2003-01-01"),xend=as.Date("2005-01-01"),y=14,yend=14), size = 1.8, color = "green3")+
  geom_segment(aes(x=as.Date("2003-01-01"),xend=as.Date("2005-01-01"),y=28,yend=28), size = 1.8, color = "blue1")+
  geom_segment(aes(x=as.Date("2005-01-01"),xend=as.Date("2010-01-01"),y=15,yend=15), size = 1.8, color = "green3")+
  geom_segment(aes(x=as.Date("2005-01-01"),xend=as.Date("2010-01-01"),y=35,yend=35), size = 1.8, color = "blue1")+
  #scale_linetype_manual(values = "yellow2")+
  theme_classic() +
  adams_theme +
  year_axis +
  scale_color_manual(values = pft.cols)

Submodel_annual_rec


png(paste0(getwd(),"/16_Recruitment_Annual_Sums.png"), height=5, width=8, units="in", res = 100)
Submodel_annual_rec
dev.off()









