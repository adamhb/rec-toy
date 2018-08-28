#FATES current


setwd("C:/Users/ahanb/OneDrive/Documents/RecruitmentToyModel/RecruitmentToyModel/hist")
#file1 <- nc_open(dir()[1])

NPP <- c()
for(i in dir()){
  open_nc_file <- nc_open(i)
  tmp <- ncvar_get(open_nc_file,varid = "NPP")
  NPP <- append(NPP,tmp)
  nc_close(open_nc_file)
}

setwd("C:/Users/ahanb/OneDrive/Documents/RecruitmentToyModel/RecruitmentToyModel")

run_name <- "FATES_current_8_4_2018"
#dir.create(path = paste0("C:/Users/ahanb/OneDrive/Documents/RecruitmentToyModel/RecruitmentToyModel/Output_for_Lara/",run_name), showWarnings = T)



hour <- 0:113880
FATES_vars_F <- data.frame(hour = hour, NPP = NPP)


FATES_vars_F <- FATES_vars_F %>% 
  mutate(date = as_datetime(hour*3600, origin = "2003-01-01")) %>% #adding the date and time
  mutate(day = floor(as.numeric(date)/86400)-floor(as.numeric(date[1])/86400)) %>% # adding the day
  mutate_at(.,.cols = vars(NPP), .funs = function(x){x*3600}) #making NPP hourly


FATES_state_vars_F <- FATES_vars_F %>%
  select(day, date) %>%
  group_by(day) %>%
  summarise_all(.,mean) %>% ungroup(.) %>% arrange(day)


date_F <- FATES_state_vars_F$date

FATES_flux_vars_F <- FATES_vars_F %>%
  select(day, NPP) %>%
  group_by(day) %>%
  summarise_all(.,sum) %>% ungroup(.) %>% arrange(day)

FATES_flux_vars_F <- FATES_flux_vars_F %>%
mutate(c_repro_F = NPP * 0.1 * 10000) %>%  #calculating the carbon allocated to reproduction in each daily timestep for the whole model area (1 hectare)
mutate_at(.tbl = .,.cols = vars(c_repro_F), .funs = function(x){ifelse(x < 0, 0, x)}) %>% 
arrange(., day) 


FATES_current_data <- FATES_flux_vars_F

c_repro_FATES <- FATES_current_data$c_repro_F 
seedbank_F <- c()
seedbank_F[1] <- 1
R_FATES <- c()
R_FATES[1] <- 0

for(i in 1:(length(c_repro_FATES)-1)){

seedbank_F[i+1] <- seedbank_F[i] %>%
  + c_repro_FATES[i] %>%
  - (decay_rate/365 * seedbank_F[i]) %>%
  - (seedbank_F[i] * 0.0013)

R_FATES[i+1] <- (seedbank_F[i] * 0.0013) / Z0
  
}


FATES_current_data <- FATES_current_data %>%
  mutate(seedbank = seedbank_F) %>%
  mutate(R = R_FATES) %>%
  mutate(date = date_F)



#create a folder for the output
dir.create(path = paste0("C:/Users/ahanb/OneDrive/Documents/RecruitmentToyModel/RecruitmentToyModel/Output_for_Lara/",run_name), showWarnings = T)


#put the params file in the output folder
write.csv(paramsOFrun, file = paste0("C:/Users/ahanb/OneDrive/Documents/RecruitmentToyModel/RecruitmentToyModel/Output_for_Lara/",run_name,"/params.csv"))


setwd("C:/Users/ahanb/OneDrive/Documents/RecruitmentToyModel/RecruitmentToyModel")



#graphing the carbon allocated to reproduction
p2F <- ggplot(data = FATES_current_data, aes(x = as.Date(date), y = c_repro_F)) +
  smooth_line +
  year_axis +
  ylab(expression(paste("carbon for repro.", "(g C day"^"-1","ha"^"-1",")")))+
  xlab(bquote('year'))+
  labs(title = "C allocated to reproduction per day") +
  theme_classic() +
  adams_theme +
  theme(legend.position = "none")

png(paste0("./Output_for_Lara/",run_name,"/C_for_repro_current_FATES.png"), height=5, width=8, units="in", res = 100)
p2F
dev.off()



#graphing seedbank size
p3F <- ggplot(data = FATES_current_data, aes(x = as.Date(date), y = seedbank)) +
  smooth_line+
  year_axis +
  ylab(expression(paste("seedbank size ", " (g C ","ha"^"-1",")")))+
  xlab(bquote('year'))+
  labs(title = "Seed bank size (g C)") +
  theme_classic() +
  adams_theme +
  theme(legend.position = "none")

png(paste0("./Output_for_Lara/",run_name,"/SeedBank_current_FATES.png"), height=5, width=8, units="in", res = 100)
p3F
dev.off()



#graphing the number of recruits
p9F <- ggplot(data = FATES_current_data, aes(x = as.Date(date), y = R)) +
  smooth_line +
  year_axis +
  ylab(expression(paste('N recruits'," day"^"-1"))) +
  xlab(bquote('year')) +
  labs(title = 'daily number of recruits') +
  theme_classic() +
  adams_theme #+
#geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), 
#mapping = aes(x = as.Date(date), y = total_R, color = pft),
#size = 1.8, method = "loess", span = .01, se = F) 



png(paste0("./Output_for_Lara/",run_name,"/daily_N_recruits_current_FATES.png"), height=5, width=8, units="in", res = 100)
p9F
dev.off()


