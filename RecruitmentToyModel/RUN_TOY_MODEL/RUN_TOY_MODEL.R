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

              toymodout <- rec_toy_model(years = 1, 
                            cmc_ind_0 = 2e4,#starting amount of biomass for the adult cohort (g C)
                            frac_NPP2GPP = 0.3,#the fraction of NPP/GPP
                            frac_NPP2growth = 0.5,#the fraction of NPP that is used for growth
                            precip = bci_precip_1998,#precipitation data from bci in 2013; can also put in 1998- an el Nino Year. Just change "2013" to "1998"
                            soil_moist.x = bci_sm_1998_Lutz_9,
                            pct_light_seedling_layer = 0.7,#the percent light at the seedling layer (%top of canopy) 
                            seedbank_0 = 35000, #the initial mass of carbon in the seedbank (g C)
                            seedpool_0 = 12000, #the initial mass of carbon in the seedling pool (g C)
                            litter_0 = 100000, #the initial mass of carbon in the reproductive litterpool (g C)
                            N_0 = 0, #the initial number of seedlings 
                            R_0 = 0, #the initial number of recruits
                            avg_solar_flux_wet = avg_solar_flux_wet,
                            avg_solar_flux_dry = avg_solar_flux_dry,
                            wet_season_length = wet_season_length,
                            dry_season_length = dry_season_length,
                            n_PFTs = 2,
                            PFTs = c("early", "late"))
              
              run_name <- "guest_run"
              
              
              
              
              
              toy_model_output <- list()
              toy_model_output[[1]] <- rbind(toymodout[[1]],toymodout[[2]])
              toy_model_output[[2]] <- rbind(toymodout[[4]],toymodout[[6]], toymodout[[8]], toymodout[[10]])
              toy_model_output[[2]]$PFT <- as.factor(paste0(toy_model_output[[2]]$PFT.x,toy_model_output[[2]]$dPFT))
              
              #graphing cohort dbh
              p1 <- ggplot(data = toy_model_output[[1]], aes(x = day, y = co_dbh_ind, color = PFT.x)) +
                geom_line(size = 1.8)+
                scale_x_continuous(name = "day")+
                ylab(bquote('dbh (mm)'))
              #graphing carbon allocated to reproduction each day 
              p2 <- ggplot(data = toy_model_output[[2]], aes(x = day, y = c4_repro_day, color = PFT)) +
                geom_line(size = 1.8)+
                scale_x_continuous(name = "day") +
                ylab(bquote('carbon for reproduction (g)'))
              #graphing the cohort LAI over time
              p3 <- ggplot(data = toy_model_output[[1]], aes(x = day, y = co_LAI_ind, color = PFT.x)) +
                geom_line(size = 1.8)+
                scale_x_continuous(name = "day") +
                ylab(bquote('cohort LAI ' *m^-2))
              #graphing the number of individuals per cohort
              p4 <- ggplot(data = toy_model_output[[1]], aes(x = day, y = N_co, color = PFT.x)) +
                geom_line(size = 1.8)+
                scale_x_continuous(name = "day") +
                ylab(bquote('N_cohort'))
              #graphing seedbank size
              p5 <- ggplot(data = toy_model_output[[2]], aes(x = day, y = seedbank, color = PFT)) +
                geom_line(size = 1.8)+
                scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1-1-1"), "%b"), breaks = seq(0, length(toy_model_output[[2]]$day), 240)) +
                ylab(bquote('seedbank size (gC)')) +
                xlab("month")
              #graphing seedpool size
              p6 <- ggplot(data = toy_model_output[[2]], aes(x = day, y = seedpool, color = PFT)) +
                geom_line(size = 1.8)+
                scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1-1-1"), "%b"), breaks = seq(0, length(toy_model_output[[2]]$day), 240)) +
                ylab(bquote('seedling pool (gC)'))+
                xlab("month")
              #graphing the number of new recruits into the 1 cm size class
              p7 <- ggplot(data = toy_model_output[[2]], aes(x = day, y = R, color = PFT)) +
                geom_line(size = 1.8)+
                scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1-1-1"), "%b"), breaks = seq(0, length(toy_model_output[[2]]$day), 240)) +
                ylab(bquote('rec rate ('*'ind.ha' ^-1~day^-1*')'))+
                xlab("month")
              #graphing N  
              p8 <- ggplot(data = toy_model_output[[2]], aes(x = day, y = N, color = PFT)) +
                geom_line(size = 1.8)+
                scale_x_continuous(name = "day") +
                ylab(bquote('N ('*'ind.ha'*')'))
              #graphing fraction emerging
              p9 <- ggplot(data = toy_model_output[[2]], aes(x = day, y = frac_emerging, color = PFT)) +
                geom_line(size = 1.8)+
                scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1-1-1"), "%b"), breaks = seq(0, length(toy_model_output[[2]]$day), 240)) +
                ylab("frac emerg from seedbank per day")+
                xlab("month")
              #p10 mort rate from water stress
              p10 <- ggplot(data = toy_model_output[[2]], aes(x = day, y = H20_mort_rate, color = PFT)) +
                geom_line(size = 1.8)+
                scale_x_continuous(labels = function(x) format(as.Date(x, origin = "1-1-1"), "%b"), breaks = seq(0, length(toy_model_output[[2]]$day), 240)) +
                ylab("H20 Mort Rate")+
                xlab("month")
              
              
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
              
              
              png(paste0("./graphs/",run_name,"_allocation.png"), height=8.5, width=11, units="in", res=300)
              multiplot(plotlist = plots[1:4], cols = 2)
              dev.off()
              
              png(paste0("./graphs/",run_name,"_seedling_dynamics.png"), height=8.5, width=11, units="in", res=300)
              multiplot(plotlist = plots[5:8], cols = 2)
              dev.off()
              
              png(paste0("./graphs/",run_name,"_recruitment.png"), height=8.5, width=11, units="in", res=300)
              multiplot(plotlist = plots[9:10], cols = 2)
              dev.off()