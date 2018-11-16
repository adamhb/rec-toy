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

run_name <- "bci_3pct_light"
avg_precip <- 71 #precipitation in mm over two weeks (the annual average)
avg_l <- 61 #the average total solar radiation load (MJ per m2) at the forest floor over 6 months (annual average)


#parameters
#scenario
pft_names <- c("earlydi", "earlydt", "latedi", "latedt")
percent_light <- 0.03
dbh.x <- 500 #dbh in mm
N_co.x <- 800  #the number of individuals in a cohort
model_area <- 10000/4 #area in square meters

#parameters for allocation to reproduction
Dmax <- dmax_vals #these are the default parameters based off of BCI FDP data
Dmax <- c(934.2857, 846.3182, 556.7179, 561.3786) #maximum diamater (mm)
names(Dmax) <- pft_names
frac_repro <- c(0.1,0.1,0.1, 0.1)#the fraction of NPP that gets allocated to reproduction
names(frac_repro) <- pft_names
seed_frac <- 0.5 #the fraction of reproductive carbon that gets allocated to seeds

#seed bank and emergence
seed_mass <- c(0.09272, 0.09272, 0.0954152, 0.0954152)
seed_mass <- default_seed_mass
names(seed_mass) <- pft_names
decay_rate <- 0.51 #the annual decay rate of the seedbank
beta_emerg <- beta_emerg_default
#beta_emerg <- c(0.074, 0.056, 0.03617053, 0.07876964)
names(beta_emerg) <- pft_names
a_emerg <- rep(0.5/365, 4) #the average daily fraction of the seedbank that moves to the seedling pool (annual average)
names(a_emerg) <- pft_names
b_emerg <- c(1.05,1.05, 1, 1) #the precipitation response parameter for emergence
names(b_emerg) <- pft_names

#seedling mort-H20
background_seedling_mort <- default_background_seedling_mort #see script called background_seedling_mort.R for derivation
#background_seedling_mort <- 0.2875200 0.2875200 0.2094371 0.2094371
names(background_seedling_mort) <- pft_names
P1H20 <- c(4.97e-08, 5.07e-08, 4.97e-08, 5.07e-08)
P2H20 <- c(-3.93e-17, -2.45e-17, -3.93e-17, -2.45e-17)
thresh.xx <- c(-167973.2, -350000.0, -167973.2, -350000.0) #the water moisture threshold (mm of head of water) when plants start to stress
names(thresh.xx) <- pft_names
window.x <- 18*7 #the number of days over which to calculate the moisture deficit

#light mort
P1light_mort <- c(0.752, 0.752, 0.0241, 0.0241)
names(P1light_mort) <- pft_names
P2light_mort <- c(0.1368, 0.1368, 0.0404, 0.0404)
names(P2light_mort) <- pft_names
Z0_seedling <- c(35,35,40,40)
names(Z0_seedling) <- pft_names


#transition from seedling to adult recruit
a_rec <- a_rec_default #this is the daily beta rec default from liza comitas data
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
source(dir()[grep(x = dir(), pattern = ".R")])

#creating a dataframe of the input variables from FATES
hour <- 0:113880
FATES_vars <- data.frame(hour = hour, SMP = SMP, FSDS = FSDS, ED_biomass = ED_biomass, NPP = NPP ,TLAI = TLAI, SABG = SABG, GPP = GPP)


FATES_vars <- FATES_vars %>% 
  mutate(date = as_datetime(hour*3600, origin = "2003-01-01")) %>% #adding the date and time
  mutate(day = floor(as.numeric(date)/86400)-floor(as.numeric(date[1])/86400)) %>% # adding the day
  mutate_at(.,.cols = vars(NPP), .funs = function(x){x*3600}) %>% #converting NPP from a flux every second to an hourly flux. CHECK THIS, MAKE SURE ITS SUMMING TO THE DAY.
  mutate_at(.,.cols = vars(FSDS), .funs = function(x){x*3600}) #convert FSDS from a flux every second to an hourly flux (J hour -1 m -1)

  
FATES_vars <- FATES_vars %>% 
  rbind(.,.) %>% #duplicating the data to account for two PFTs (early and late)
  add_column(pft = c(rep("early", (length(.$hour)/2)), rep("late", (length(.$hour)/2)))) %>% #adding pfts
  add_column(dbh = rep(dbh.x, length(.$hour))) %>% #adding the dbh
  add_column(N_co = rep(N_co.x, length(.$hour))) #adding the number in the cohort
  

FATES_state_vars <- FATES_vars %>%
  select(day, date, pft, dbh, N_co, SMP) %>%
  group_by(day,pft) %>%
  summarise_all(.,mean) %>% ungroup(.) %>% arrange(pft, day) %>% #converting to daily time step
  add_column(precip = rep(bci_precip_new[bci_precip_new$date >= as.Date("2003-01-01") & bci_precip_new$date <= as.Date("2015-12-29"),2], 2))

               
FATES_flux_vars <- FATES_vars %>%
  select(day, pft, NPP, FSDS) %>%
  group_by(day,pft) %>%
  summarise_all(.,sum) %>% ungroup(.) %>% arrange(pft, day) #summing hourly fluxes. FSDS = Joules per day, NPP = gC per day


FATES_vars <- cbind(FATES_state_vars, FATES_flux_vars[,-c(1,2)]) %>% #combining the state vars and the flux vars
  rbind(.,.) %>% #duplicating to account for 2 drought tolerant PFTS
  add_column(dpft = c(rep("DI", (length(.$day)/2)), rep("DT", (length(.$day)/2)))) %>% #adding drought tolerance pfts
  add_column(water_def = append(def_func(soil_moist = FATES_state_vars$SMP, thresh.x = thresh.xx, window = window.x, dPFT = "DI"), def_func(soil_moist = FATES_state_vars$SMP, thresh.x = thresh.xx, window = window.x, dPFT = "DT"))) %>% #adding water deficit
  add_column(H20_mort_rate = .$water_def) %>%
  mutate_at(.tbl = ., .cols = vars(H20_mort_rate), .funs = H20_mort, PFT = vars(dpft)) #adding H20 mort rate


FATES_vars <- FATES_vars %>%
  mutate(e_frac = base::mapply(FUN = efrac, N = (FATES_vars$N_co), co_dbh_ind = (FATES_vars$dbh), PFT = FATES_vars$pft)) %>% #adding the "effective fraction" of NPP that gets allocated to reproduction in each time step
  mutate(c_repro = e_frac * NPP * model_area) %>%  #calculating the carbon allocated to reproduction in each daily timestep for the whole model area (1 hectare)
  mutate_at(.tbl = .,.cols = vars(c_repro), .funs = function(x){ifelse(x < 0, 0, x)}) %>% 
  arrange(., day,pft,dpft) %>%
  mutate(light = FSDS * percent_light) 


output <- list()

j <- 1

for(PFT.xxx in c("early", "late")){
  for(dPFT.xxx in c("DI", "DT")){
    
    
    input_vars <- FATES_vars %>% filter(pft == PFT.xxx, dpft == dPFT.xxx)
    
    #pfts
    lPFT <- c()
    dPFT <- c()
    
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
    PFT <- 
    
  
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
      lPFT[i+1] <- PFT.xxx
      
      dPFT[i+1] <- dPFT.xxx
      
      
      #allocation dynamics are captured above, outside the for loops, because they don't rely on previous time steps
      
      
      #seedbank dynamics
      seedbank[i+1] <- seedbank[i] %>%
        - (decay_rate/365 * seedbank[i]) %>%
        - (frac_emerg_func(rain = (ifelse(test= i > 7, yes = sum(input_vars$precip[(i-6):i]), no = 7 * input_vars$precip[i]))) * seedbank[i]) %>%
        + (seed_frac * input_vars$c_repro[i])
      
      frac_emerging[i+1] <- (frac_emerg_func(rain = (ifelse(test= i > 7, yes = sum(input_vars$precip[(i-6):i]), no = 7 * input_vars$precip[i]))))
      
      #seedling pool dynamics
      
      #if(i == 3653){browser()}
      
      seedpool[i+1] <- seedpool[i] %>%
        + ((frac_emerg_func(rain = (ifelse(test= i > 7, yes = sum(input_vars$precip[(i-6):i]), no = 7 * input_vars$precip[i])))) *seedbank[i]) %>%
        - ((light_mort(light = (input_vars$light[i]+0.0001), seedlings_C = seedpool[i], PFT = PFT.xxx, N_smp = 50)) * seedpool[i]) %>%
        - (H20_mort(deficit_days = input_vars$water_def[i], PFT = dPFT.xxx)*seedpool[i]) %>%
        - (seedpool[i]*background_seedling_mort/365) %>%
        - (frac_rec(light = (input_vars$light[i]+0.0001), PFT = PFT.xxx, seedlings_C = seedpool[i]) * seedpool[i])
      
      light_mort_rate[i+1] <- light_mort(light = (input_vars$light[i]+0.0001), seedlings_C = seedpool[i], PFT = PFT.xxx, N_smp = 50)
      
      frac_rec.t[i+1] <- frac_rec(light = (input_vars$light[i]+0.0001), PFT = PFT.xxx, seedlings_C = seedpool[i])
      
      
    
      #recruitment and litter pool dynamics
      R[i+1] <- frac_rec(light = (input_vars$light[i]+0.0001), PFT = PFT.xxx, seedlings_C = seedpool[i]) * seedpool[i] / (Z0)
      
      
      N[i+1] <- N[i] %>%
        + R[i+1]
      
      
      
      litter[i+1] <- litter[i] %>%
        + ((1-seed_frac) * input_vars$c_repro[i]) %>%
        + (decay_rate/365 * seedbank[i]) %>%
        + (light_mort(light = (input_vars$light[i]+0.0001), seedlings_C = seedpool[i], PFT = PFT.xxx, N_smp = 50) * seedpool[i]) %>%
        + (seedpool[i] * background_seedling_mort/365) %>%
        + (H20_mort(deficit_days = water_def[i], PFT = dPFT.xxx) * seedpool[i])
      
      
    }
    
    
    output[[j]] <- data.frame(lPFT = lPFT,
                              dPFT = dPFT,
                              seedbank = seedbank, 
                              frac_emerging = frac_emerging,
                              seedpool = seedpool,
                              light_mort_rate = light_mort_rate,
                              frac_rec.t = frac_rec.t,
                              R = R, 
                              N = N,
                              litter = litter) %>% cbind(input_vars,.) %>% select(day, date, pft, dpft, dbh, N_co, NPP, e_frac, c_repro, seedbank, precip, frac_emerging, seedpool, FSDS, light, light_mort_rate, frac_rec.t, SMP, water_def, H20_mort_rate, R, N, litter)
    
    print(paste0(j," of 4 is done!"))  
    j <- j+1
    
    
  }
  
}


full_output <- rbind(output[[1]], output[[2]], output[[3]], output[[4]])
full_output$pfts <- paste0(full_output$pft,",",full_output$dpft)































p11 <- full_output %>% arrange(desc(pfts)) %>% ggplot( aes(x = as.Date(date), y = R*365, color = pfts)) +
  smoother_line +
  year_axis +
  ylab(expression(paste('N recruits'," yr"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'New Submodel') +
  theme_classic() +
  adams_theme +
  geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), mapping = aes(x = as.Date(date), y = total_R*365, color = pft), size = 1.8, method = "loess", span = .1, se = F) 

p11


N_recs_per_year_pfts <- full_output %>% mutate(year = substring(text = as.character(date), first = 1, last = 4)) %>% group_by(year, pft) %>% summarise(N_rec = sum(R)) 

N_recs_per_year_pfts$year <- as.Date(paste0((as.numeric(N_recs_per_year_pfts$year)+1), "-01-01"))

Submodel_annual_rec_extra_slide <- ggplot() +
  #smoother_line +
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
  scale_color_manual(values = c("green3","blue1"))

Submodel_annual_rec_extra_slide



















































#create a folder for the output
dir.create(path = paste0("C:/Users/ahanb/OneDrive/Documents/rec_submodel/Submodel_Output/",run_name), showWarnings = T)

#record the params
paramsOFrun <- data.frame(param_names = c("model_area", "dbh.x", "N_co.x", "Dmax", "frac_repro", "seed_frac","decay_rate", "percent_light", "thresh", "window.x", "seedbank_0", "seedpool_0", "litter_0"), param_vals = c(model_area, dbh.x, N_co.x, paste0(Dmax[1],",",Dmax[2]), (paste0(frac_repro[1],",",frac_repro[2])), seed_frac, decay_rate, percent_light, paste0(thresh.xx[1],",",thresh.xx[2]), window.x, seedbank_0, seedpool_0, litter_0))

#put the params file in the output folder
write.csv(paramsOFrun, file = paste0("C:/Users/ahanb/OneDrive/Documents/rec_submodel/Submodel_Output/",run_name,"/params.csv"))


setwd(paste0("C:/Users/ahanb/OneDrive/Documents/rec_submodel/Submodel_Output/",run_name))


#set theme for the plots
adams_theme <- theme(plot.title = element_text(hjust = 0.5, size = 20),
       strip.text.x = element_text(size = 18),
       legend.title = element_blank (),
       axis.title.x = element_text (size = 15), # change the axis title
       axis.title.y = element_text (size = 15),
       axis.text.x = element_text (size = 14, colour = "black"),
       axis.text.y = element_text (size = 14, colour = "black"),
       legend.text = element_text (size = 15))
year_axis <-  scale_x_date(breaks = date_breaks("2 years"), labels = date_format("%Y"))
smooth_line <- geom_smooth(size = 1.8, method = "loess", span = .01, se = F)
smoother_line <- geom_smooth(size = 1.8, method = "loess", span = .1, se = F)



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
  
  png(paste0(getwd(),"/NPP.png"), height=5, width=8, units="in", res = 100)
  NPP_g
  dev.off()
  
  
  
  precip_g <- ggplot(data = full_output, aes(x = as.Date(date), y = precip, color = pft)) +
    labs(title = "precip.") +
    theme_classic()+
    #geom_point()+
    smooth_line +
    year_axis +
    ylab(expression(paste("daily precip (mm)")))+
    xlab(bquote('year'))+
    adams_theme +
    theme(legend.position = "none")
  
  png(paste0(getwd(),"/precip.png"), height=5, width=8, units="in", res = 100)
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
    
  
  
  png(paste0(getwd(),"/SMP.png"), height=5, width=8, units="in", res = 100)
  SMP_MPa_g
  dev.off()
  
  
  
  
  
  water_def_g <- ggplot(data = full_output, aes(x = as.Date(date), y = water_def, color = dpft)) +
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
    
    
    png(paste0(getwd(),"/water_def.png"), height=5, width=8, units="in", res = 100)
  water_def_g
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
  adams_theme
  
  #xlab("Production Type")+
  #ylab(expression(paste("Kg ", "CO" ["2(eq)"], " per Kg fresh tomato" )))
  
png(paste0(getwd(),"/e_frac.png"), height=5, width=8, units="in", res = 100)
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
  theme(legend.position = "none")

png(paste0(getwd(),"/C_for_repro.png"), height=5, width=8, units="in", res = 100)
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
  theme(legend.position = "none")

png(paste0(getwd(),"/SeedBank.png"), height=5, width=8, units="in", res = 100)
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
  theme(legend.position = "none")

png(paste0(getwd(),"/frac_emerging.png"), height=5, width=8, units="in", res = 100)
p4
dev.off()


#graphing the seedling pool
p5 <- ggplot(data = full_output %>% filter(date >= as.POSIXct("2005-01-01")), aes(x = as.Date(date), y = seedpool, color = pfts)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(bquote('seedling pool size (gC)'))+
  xlab(bquote('year'))+
  labs(title = 'seedling pool size (gC)') +
  scale_color_manual(values = pft.cols)+
  theme_classic() +
  adams_theme


png(paste0(getwd(),"/seedling_pool.png"), height=5, width=8, units="in", res = 100)
p5
dev.off()


#graphing the light mortality rate
p6 <- ggplot(data = full_output %>% filter(date >= as.POSIXct("2005-01-01")), aes(x = as.Date(date), y = light_mort_rate, color = pft)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(bquote('daily mort rate (% of seedling pool)'))+
  xlab(bquote('year'))+
  scale_color_manual(values = c("darkolivegreen4", "midnightblue"))+
  labs(title = 'light-dep. seedling mortality (3% light)') +
  theme_classic() +
  adams_theme


png(paste0(getwd(),"/light_mort_3pct.png"), height=5, width=8, units="in", res = 100)
p6
dev.off()




#graphing H20 mortality rate
p7 <- ggplot(data = full_output %>% filter(date >= as.POSIXct("2005-01-01")), aes(x = as.Date(date), y = H20_mort_rate, color = dpft)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(expression(paste('H20 Mort. Rate'," (day)"^"-1"))) +
  #ylab(bquote('H20 mort rate'))+
  xlab(bquote('year'))+
  labs(title = 'H20 mort rate') +
  scale_color_manual(values = c("darkolivegreen2", "darkolivegreen4"))+
  theme_classic() +
  adams_theme


png(paste0(getwd(),"/H20_mort_rate.png"), height=5, width=8, units="in", res = 100)
p7
dev.off()


#graphing the fraction recruiting from the seedling pool to the adult size class
p8 <- ggplot(data = full_output, aes(x = as.Date(date), y = frac_rec.t, color = pft)) +
  smooth_line +
  year_axis +
  ylab(expression(paste('rec. rate (% of seedling pool'," day"^"-1",")")))+
  xlab(bquote('year'))+
  labs(title = 'seedling recruitment rate (%)') +
  theme_classic() +
  adams_theme

png(paste0(getwd(),"/fraction_recruiting.png"), height=5, width=8, units="in", res = 100)
p8
dev.off()



#graphing the daily recruitment rate without the total
p9 <- full_output %>% arrange(desc(pfts)) %>% ggplot( aes(x = as.Date(date), y = R, color = pfts)) +
  smoother_line +
  year_axis +
  ylab(expression(paste('N recruits'," day"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'daily number of recruits') +
  theme_classic() +
  adams_theme #+
  #geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), 
              #mapping = aes(x = as.Date(date), y = total_R, color = pft),
              #size = 1.8, method = "loess", span = .01, se = F) 



png(paste0(getwd(),"/daily_N_recruits.png"), height=5, width=8, units="in", res = 100)
p9
dev.off()





#graphing the daily recruitment rate with the total

p10 <- full_output %>% arrange(desc(pfts)) %>% ggplot( aes(x = as.Date(date), y = R, color = pfts)) +
  smoother_line +
  year_axis +
  ylab(expression(paste('N recruits'," day"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'daily number of recruits') +
  theme_classic() +
  adams_theme +
  geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), 
              mapping = aes(x = as.Date(date), y = total_R, color = pft),
              size = 1.8, method = "loess", span = .1, se = F) 
  
  

png(paste0(getwd(),"/daily_N_recruits_w_total.png"), height=5, width=8, units="in", res = 100)
p10
dev.off()



#graph 1 (right)
#graphing the annual recruitment rate of the submodel with the totals and all pfts
p11 <- full_output %>% arrange(desc(pfts)) %>% ggplot( aes(x = as.Date(date), y = R*365, color = pfts)) +
  smoother_line +
  year_axis +
  ylab(expression(paste('N recruits'," yr"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'New Submodel') +
  theme_classic() +
  adams_theme +
  geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), mapping = aes(x = as.Date(date), y = total_R*365, color = pft), size = 1.8, method = "loess", span = .1, se = F) 

p11

png(paste0(getwd(),"/submodel_annual_N_recruits_w_total.png"), height=5, width=8, units="in", res = 100)
p11
dev.off()


#average annual recruitment rate output for a given scenario




#testing creating the plot



#average number of recruits for FATES









