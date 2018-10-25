
#emergence subroutine

#input vars: the amount of precipitation over the prior two weeks, the seedbank size
#output var: the amount of carbon leaving the seedbank during each daily timestep 

#params
#a 4 seedling emerging per m2 per year for early and 10 for late. This is based on Garwood.
#b is a tuning param tuned to phenominological observations from garwood re. timing of germination




#calculating a PFT-specific seed mass size
setwd("C:/Users/ahanb/OneDrive/Documents/data/BCI_FDP_Data")
seed_mass_data <- read.csv("seed_mass_data_Daws_et_al_2007.csv")

seed_mass_data <- merge(seed_mass_data, pfts_sept_2018, by = "Latin", all.x = T)


seed_mass <- seed_mass_data %>%
  group_by(e_vs_l, dpft) %>%
  summarise(pft_seed_mass = mean(dry_mass_mg)) %>%
  .$pft_seed_mass

#seed dry biomass
seed_mass <- c(seed_mass[1], seed_mass[2], seed_mass[3], seed_mass[3])

#converting from mg to grams of dry biomass and then to grams of carbon assuming 40% of mass of seed is carbon
seed_mass <- seed_mass / 1000 * 0.4
default_seed_mass <- seed_mass


emerg_func <- function(pft = PFT, beta_emerg.x = beta_emerg[PFT], a = a_emerg[PFT], b = b_emerg[PFT], rain = 71, avg_rain = 71, seedbank.x = 40000){
  
  a <- a * Z0_seedling[PFT] / 365 * 14 #converting the a parameter to be in units of carbon over a two week period
  
  if(transition_probs == T){
    a <- beta_emerg.x * seedbank.x / 365 * 14
  }
  
  log10_C_emerg <- log10(a) + b*log10(rain/avg_rain) #the log number of individuals emerging over the prior two week interval
  
  
  C_emerg_2wks <- 10^log10_C_emerg #the number of individual emerging over a two week internval
  C_emerg_per_day <-  C_emerg_2wks / 14
  
  return(C_emerg_per_day)
}


transition_probs <- T

14*10000 / 365 * 35 #13,000 grams per ha per day



debug(emerg_func)
emerg_func()






#the benchmark for what is emerging per day should be around 3500 g per ha per day

#this should be around 1500 per ha per two weeks on average, or around 109 seedlings per ha per day 
#beta trans is 595 times too small, or seedbank is too small



#testing the recruitment subroutine
rain_test <- rollapply(bci_precip_2013$rain_mm, 14, sum)
rain_test <- rain_test[seq(1, length(rain_test), 14)]



$carbon_g


emergence_fall_2018(rain = rain_test) #this gives the daily emergence 

#emergence benchmark: earlydi = 5 m2 per year = 50,000 ha yr = 1917 per ha per two weeks




sum(emergence_fall_2018(pft = "early", rain = rain_test, b.x = c(1.5,1))[[1]])

rain_data <- data.frame(rain = rain_test, emerg_per_2_weeks =  Rs.e) 
rain_data <- cbind(data.frame(week = 1:26), rain_data)

ggplot(data = rain_data, mapping = aes(x = week, y = emerg_per_2_weeks)) + geom_point()











plot(rain_test, Rs.e)

Rs.l <- emergence_fall_2018(pft = "late", l = l, a = 20)

Ruger_bench_data <- data.frame(l = l, early = Rs.e[[1]], late = Rs.l[[1]])
Ruger_bench_data <- melt(data = Ruger_bench_data, id.vars = "l", variable.name = "pft") 

Ruger_bench_data %>% filter(l < 100) %>% ggplot(mapping = aes(x = l, y = value, color = pft)) + geom_line() +
  scale_y_continuous(name = "N _recruits_ha_6_months")+
  scale_x_continuous(name = "light")



#params
#a late: 14 per ha per 6 months under 2 % light
#a early: 7 per ha per 6 months under 2 % light
#l is in terms of MJ per months 
#avg. l = 58 MJ per 6 months (2% light)




seed_2_seedling <- read.csv("Dataset2_BCIseed2seedling.csv")
seed_2_seedling <- seed_2_seedling[,c("species","p_rec")]
names(seed_2_seedling) <- c("sp","p_rec")


beta_emerg_default <- merge(pfts_sept_2018, seed_2_seedling, by = "sp") %>%
  group_by(e_vs_l, dpft) %>%
  summarise(p_rec_mean = mean(p_rec)) %>%
  .$p_rec_mean

beta_emerg 
b_trans_2_wks <-


  
  
  emerg_func_version_with_seed_mass <- function(pft = PFT, beta_emerg.x = beta_emerg[PFT], a = a_emerg[PFT], b = b_emerg[PFT], rain = 71, avg_rain = 71, seedbank.x = 40000){
    
    if(transition_probs == T){
      a <- beta_emerg.x * (seedbank.x / seed_mass[PFT])     
    }
    
    a <- a / 365 * 14 #converting the a parameter to be over a two week period
    
    log10_n_emerg <- log10(a) + b*log10(rain/avg_rain) #the log number of individuals emerging over the prior two week interval
    
    n_emerg_2wks <- 10^log10_n_emerg #the number of individual emerging over a two week internval
    n_emerg_per_day <-  n_emerg_2wks / 14
    
    
    C_emerg_2wks <- n_emerg_2wks * seed_mass[PFT] #the amount of carbon emerging over the prior two week internval
    C_emerg_per_day <- C_emerg_2wks / 14
    
    
    
    out <- list(n_emerg_per_day,C_emerg_per_day)
    names(out) <- c("n_emergs", "carbon_g")
    
    return(out)
  }

debug(emerg_func_version_with_seed_mass)
emerg_func_version_with_seed_mass()













































emerg_func <- function(a = a_emerg[PFT], b = b_emerg[PFT], rain = 71, avg_rain = 71, seedbank.x = 40000){

  
  log10_frac_emerg <- log10(a) + b*log10(rain/avg_rain) 
  
  frac_emerg <- 10^log10_frac_emerg 
  
  C_emerg <- frac_emerg * seedbank.x
  
  out <- list(frac_emerg, C_emerg)
  names(out) <- c("frac_emerg", "C_emerg")
  return(out)
}


emerg_func(a = 0.5/365, b = 1.05, rain = 90, avg_rain = 71, seedbank.x = 40000)$frac_emerg







