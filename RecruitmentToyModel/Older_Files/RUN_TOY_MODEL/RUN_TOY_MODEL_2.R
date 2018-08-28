#model_runs
#set wd to where you downloaded the RecruitmentToyModel Repo
#you can parameterize the model below using all the function arguments in the rec_toy_model function
#name the output files whatever you like by changing the name of the "run_name" object.
#output graphs will appear in the "graphs" subdirectory

setwd("C:/Users/ahanb/OneDrive/Documents/RecruitmentToyModel/RecruitmentToyModel")
source("./data&params.R")
source("./clean/munge.R")
source("./funcs/other_toy_model_funcs.R")
source("./funcs/frac_rec_Kobe.R")
source("./funcs/H20_mort.R")
source("./funcs/light_mort.R")
source("./funcs/frac_emerg.R")
source("./RUN_TOY_MODEL/RecruitmentToyModelFunction.R")

setwd("C:/Users/ahanb/OneDrive/Documents/RecruitmentToyModel/RecruitmentToyModel")


library(ggplot2)
library(magrittr)
library(tidyverse)


              toymodout <- rec_toy_model(
                PFTs = c("early", "late"),
                PFT_prptn = c(0.5,0.5), #The proportions of each PFT in the model run.
                years = 5, #how long you run the model for
                n_PFTs = 2, #the number of PFTs in the model
                dbh_0 = c(10,10),# the starting dbh of the adult cohorts (early and late) (cm)
                frac_NPP2GPP = 0.01,#the ratio of GPP to NPP
                frac_NPP2growth = 0.3,#the fraction of NPP that is used for growth
                precip = bci_precip_1998,#precipitation data from bci in 2013 or 1998; 1998 is an el Nino Year. Just toggle between "2013" to "1998"
                soil_moist.x = bci_sm_1998_Lutz_9, #soil moisture data from bci in 2013 or 1998; 1998 is an el Nino Year. Just toggle between "2013" to "1998"
                pct_light_seedling_layer = 0.8,#relative irradiance at the seedling layer (%top of canopy; 0 - 1) 
                seedbank_0 = 50000, #the initial mass of carbon in the seedbank (g C). 
                seedpool_0 = 22750, #the initial mass of carbon in the seedling pool (g C). I initialize this at 22,750 g per ha after a back of the envelope calculation using the seedling dynamics data.
                litter_0 = 10000, #the initial mass of carbon in the reproductive litterpool (g C)
                N_0 = 650, #the initial number of seedlings. This is less important because it get calculated from other variables in the model at t = 2. 
                R_0 = 0, #the initial number of recruits
                avg_solar_flux_wet = 347.2, #Average irradiance of the wet season (joules per meter squared per second)
                avg_solar_flux_dry = 462.96, #Average irradiance of the dry season (joules per meter squared per second)
                dry_season_length = 120)#number of days in the dry season
                
              
              
              
              run_name <- "mature_cohort_80pct_light_5_yr_el_nino"
              
              
              toy_model_output <- list()
              toy_model_output[[1]] <- rbind(toymodout[[1]],toymodout[[2]])
              toy_model_output[[2]] <- rbind(toymodout[[4]],toymodout[[6]], toymodout[[8]], toymodout[[10]])
              toy_model_output[[2]]$PFT <- as.factor(paste0(toy_model_output[[2]]$PFT.x,toy_model_output[[2]]$dPFT))
              
              
              tail(toy_model_output[[2]])
              
              
              #graphing cohort dbh
              p1 <- ggplot(data = toy_model_output[[1]], aes(x = day, y = co_dbh_ind, color = PFT.x)) +
                geom_line(size = 1.8)+
                scale_x_continuous(name = "day")+
                ylab(bquote('dbh (mm)'))
              #graphing carbon allocated to reproduction each day 
              p2 <- ggplot(data = toy_model_output[[2]], aes(x = day, y = c4_repro_day, color = PFT)) +
                geom_line(size = 1.8)+
                scale_x_continuous(name = "day") +
                ylab(bquote('carbon for reproduction (g) per day'))
              #graphing the cohort LAI over time
              p3 <- ggplot(data = toy_model_output[[1]], aes(x = day, y = co_LAI_ind, color = PFT.x)) +
                geom_line(size = 1.8)+
                scale_x_continuous(name = "day") +
                ylab(bquote('LAI per tree ' *m^2))
              #graphing the number of individuals per cohort
              p4 <- ggplot(data = toy_model_output[[1]], aes(x = day, y = N_co, color = PFT.x)) +
                geom_line(size = 1.8)+
                scale_x_continuous(name = "day") +
                ylab(bquote('N indiv. per cohort'))
              #graphing seedbank size
              p5 <- ggplot(data = toy_model_output[[2]], aes(x = day, y = seedbank, color = PFT)) +
                geom_line(size = 1.8)+
                scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1-1-1"), "%Y-%b"), breaks = seq(0, length(toy_model_output[[2]]$day), 365)) +
                ylab(bquote('seedbank size (gC)')) +
                xlab("year-month")
              #graphing seedpool size
             p6 <- ggplot(data = toy_model_output[[2]], aes(x = day, y = seedpool, color = PFT)) +
                geom_line(size = 1.8)+
                scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1-1-1"), "%Y-%b"), breaks = seq(0, length(toy_model_output[[2]]$day), 365)) +
                ylab(bquote('seedling pool (gC)'))+
                xlab("year-month") 
              #graphing the number of new recruits into the 1 cm size class
              p7 <- ggplot(data = toy_model_output[[2]], aes(x = day, y = R, color = PFT)) +
                geom_line(size = 1.8)+
                scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1-1-1"), "%Y-%b"), breaks = seq(0, length(toy_model_output[[2]]$day), 365)) +
                ylab(bquote('rec rate ('*'ind.ha' ^-1~day^-1*')'))+
                xlab("year-month")
              #graphing N  
              p8 <- ggplot(data = toy_model_output[[2]], aes(x = day, y = N, color = PFT)) +
                geom_line(size = 1.8)+
                scale_x_continuous(name = "day") +
                ylab(bquote('N ('*'ind.ha'*')'))
              #graphing fraction emerging
              p9 <- ggplot(data = toy_model_output[[2]], aes(x = day, y = frac_emerging, color = PFT)) +
                geom_line(size = 1.8)+
                scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1-1-1"), "%Y-%b"), breaks = seq(0, length(toy_model_output[[2]]$day), 365)) +
                ylab("frac emerg from seedbank per day")+
                xlab("year-month")
              #p10 mort rate from water stress
              p10 <- ggplot(data = toy_model_output[[2]], aes(x = day, y = H20_mort_rate, color = PFT)) +
                geom_line(size = 1.8)+
                scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1-1-1"), "%Y-%b"), breaks = seq(0, length(toy_model_output[[2]]$day), 365)) +
                ylab("H20 Mort Rate")+
                xlab("year-month")
              
Total_recruiting_per_year <-     (tail(toy_model_output[[2]]$N)[6]  -   head(toy_model_output[[2]]$N)[1])  / years

Total_recruiting_per_year           
p7
              
              num_plots <- 10
              
              plots <- list()
              
              plots[[1]] <- p1
              plots[[2]] <- p3
              plots[[3]] <- p4
              plots[[4]] <- p2
              
              
              plots[[5]] <- p5
              plots[[6]] <- p9
              plots[[7]] <- p6
              plots[[8]] <- p10
              
              
              plots[[9]] <- p7
              plots[[10]] <- p8
              
              
              file_date <- format(Sys.time(), "%m_%d_%Y_%H_%M_%S")
              
              
          setwd("C:/Users/ahanb/OneDrive/Documents/RecruitmentToyModel/RecruitmentToyModel/Output_for_Lara")
              
              png(filename = paste0(run_name,"allocation.png"), height=8.5, width=11, units="in", res=300)
              multiplot(plotlist = plots[1:4], cols = 2)
              dev.off()
              
              png(filename = paste0(run_name,"b_seedling_dynamics.png"), height=8.5, width=11, units="in", res=300)
              multiplot(plotlist = plots[5:8], cols = 2)
              dev.off()
              
              png(filename = paste0(run_name,"other.png"), height=8.5, width=11, units="in", res=300)
              multiplot(plotlist = plots[9:10], cols = 2)
              dev.off()