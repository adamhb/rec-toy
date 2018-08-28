

setwd("C:/Users/ahanb/OneDrive/Documents/RecruitmentToyModel/RecruitmentToyModel")
source("C:/Users/ahanb/OneDrive/Documents/RecruitmentToyModel/RecruitmentToyModel/comita_seedling_analysis/LOAD_seedling_data.R")
source("C:/Users/ahanb/OneDrive/Documents/RecruitmentToyModel/RecruitmentToyModel/comita_seedling_analysis/MUNGE_seedling_dyn_data_Johnson.R")


#Calculating transition rates from the raw seedling dynamics data.
#the number of recruits over the whole dataset.
n_recs <- seed_dyn %>%
  filter(as.numeric(d) >= 10) %>%
  select(TAGF, Q20P5, census, d, start, stop) %>% 
  mutate(days = stop - start) %>%
  group_by(census) %>%
  summarise(n_quads = length(unique(Q20P5)), days = mean(days), n_recs = length(unique(TAGF)))%>%
  mutate(rec_per_m_2_per_year = n_recs/(days/360*n_quads*25)) %>%
  summarise(total_recs = sum(n_recs))


#for the years where the census interval was actually one year, the recruitment rates per square meter is actually in the same order of magnitude as my calculations from the forest dynamics plot data: It is about double in some cases, but I think this is because the seedling dynamics plot include all woody species, not just canopy trees.



#calculating the mean recruitment rate (all woody species) per square meter per year from the reworked seedling dynamics data that Dan Johnson sent me. 
seed_dyn_c %>%
  select(census, start.x, stop.x, Q20P5, Nt_1, N_trans, t_rate, daily_t_rate, RI3, TOC_PFD, PFD) %>%
  group_by(census) %>%
  summarise(n_quads = length(unique(Q20P5)), n_recruits = sum(N_trans), census_time_yrs = mean((stop.x - start.x)/365)) %>%
  mutate(area = n_quads * 25, rec_per_m2_per_yr = n_recruits / (area * census_time_yrs)) %>%
  summarise(mean_rec = mean(rec_per_m2_per_yr))
  
#The result 0.0002745151 is two order of magnitude lower than what I calculated from the FDP bci data. There is a problem with something in my cleaned dataset here. 



## code for checking the feasibility of joining the FDP data with the seedling dynamics data.

pfts


#interannual standard errors / deviations
seed_dyn %>%
  filter(SPP %in% pfts$SPP) %>%
  select(TAGF, Q20P5, census) %>%
  group_by(Q20P5, census) %>%
  summarise(N = length(unique(TAGF))) %>%
  group_by(Q20P5) %>%
  summarise(Mean = mean(N), sd = sd(N), se = sd(N)/sqrt(length(N))) %>%
  .$Mean %>%
  na.omit() %>% summary()
  
#number of living seedlings in each census

seed_dyn %>%
  filter(SPP %in% pfts$SPP) %>%
  select(TAGF, census, mort) %>%
  filter(census == 8, mort == 0) %>%
  .$TAGF %>% na.omit() %>% length()



#number of plots in each census



#variation within a 20X20 plot


  seed_dyn %>%
    filter(SPP %in% pfts$SPP) %>%
    select(TAGF, Q20, Q20P5, census) %>%
    group_by(Q20, Q20P5, census) %>%
    summarise(N = length(unique(TAGF))) %>%
    group_by(Q20) %>%
    summarise(Mean = mean(N), sd = sd(N), se = sd(N)/sqrt(length(N))) %>%
    .$se %>%
    na.omit() %>% summary()


 #number of 5x5 plots in each 20X20 plot
  seed_dyn %>%
    filter(SPP %in% pfts$SPP) %>%
    select(TAGF, Q20, Q20P5, census) %>%
    group_by(Q20) %>%
    summarise(N5X5_per20 = length(unique(Q20P5))) %>% 
    .$N5X5_per20 %>%
    na.omit() %>% summary()

  
  

#calculating the average number of seedlings per square meter
1.857 / 25 * 10000

#742 seedlings per acre
#
742*








