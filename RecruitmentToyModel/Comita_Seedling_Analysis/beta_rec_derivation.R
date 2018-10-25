#beta rec parameter estimation from Liza Comita's data 


#determining the annual seedling mort rate
background_seedling_mort <- seed_dyn %>%
  filter(habitat == "slope") %>%
  filter(sp %in% unique(pfts_sept_2018$sp)) %>%
  mutate(census_length_days = as.numeric(stop.date - start.date)) %>%
  merge(., pfts_sept_2018, by = "sp") %>%
  filter(dpft != "0") %>%
  mutate(pft = paste0(e_vs_l,dpft)) %>%
  group_by(e_vs_l) %>%
  summarise(mort_rate = (sum(mort)/length(mort)), mean_cen_length = mean(census_length_days)) %>%
  mutate(annual_mort_rate = (mort_rate / mean_cen_length)*365) %>%
  .$annual_mort_rate


seed_dyn_original <- readRDS("bci_seeding_data_for_Adam.RDS")




#N for each interval
N_start <- seed_dyn_original %>%
  #filter(habitat == "slope") %>%
  mutate(sp = tolower(SPP)) %>%
  filter(sp %in% unique(pfts_sept_2018$sp)) %>%
  mutate(census_length_days = as.numeric(stop.date - start.date)) %>%
  merge(., pfts_sept_2018, by = "sp") %>%
  group_by(census, e_vs_l) %>%
  summarise(Nstart = length(TAGF)) %>%
  ungroup(.)

#R for each interval
rec_per_year <- seed_dyn_original %>%
  #filter(habitat == "slope") %>%
  mutate(sp = tolower(SPP)) %>%
  filter(sp %in% unique(pfts_sept_2018$sp)) %>%
  merge(., pfts_sept_2018, by = "sp") %>%
  mutate(census_length_days = as.numeric(stop - start)) %>% 
  filter(as.numeric(d) > 10) %>%
  group_by(census, e_vs_l) %>%
  summarise(R = length(census), mean_cen_length = mean(census_length_days)) %>%
  mutate(rec_rate = R / mean_cen_length * 365) %>%
  ungroup(.)


#beta_rec for early versus late
beta_rec <- cbind(N_start, rec_per_year[,c(3,4,5)]) %>%
  mutate(beta_rec = rec_rate / Nstart) %>%
  #filter(census != 13) %>%
  group_by(e_vs_l) %>%
  summarise(mean_beta_rec = median(beta_rec)) %>%
  .$mean_beta_rec


#this is the mean 6 month transition probability from seedling to 1 cm recruit 
beta_rec_default <- (c(rep(beta_rec[1], 2), rep(beta_rec[2], 2))) / 2

#if these beta rec values appear to be very off we can take the mean not the medium to get more, then we can check with the other form the data is in. The publically available version of Dan and Liza's data.
















#N for each interval
seed_dyn_original %>%
  group_by(census) %>%
  summarise(Nstart = length(TAGF))

#R for each census interval
seed_dyn_original %>%
  filter(as.numeric(d) > 10) %>%
  group_by(census) %>%
  summarise(R = length(census))
 

seed_dyn_original %>%
  filter(census == 4) %>%
  .$TAGF %>% unique(.) %>% length(.)





seed_dyn_original %>%
  filter(as.numeric(d) > 10) %>%
  group_by(census) %>%
 