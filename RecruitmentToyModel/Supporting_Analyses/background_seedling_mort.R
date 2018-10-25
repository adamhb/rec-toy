#visualizing seedling mortality rates
seed_dyn %>%
  filter(habitat == "slope") %>%
  filter(sp %in% unique(pfts_sept_2018$sp)) %>%
  mutate(census_length_days = as.numeric(stop.date - start.date)) %>%
  merge(., pfts_sept_2018, by = "sp") %>%
  filter(dpft != "0") %>%
  mutate(pft = paste0(e_vs_l,dpft)) %>%
  group_by(pft, census) %>%
  summarise(mort_rate = (sum(mort)/length(mort)), mean_cen_length = mean(census_length_days)) %>%
  mutate(annual_mort_rate = (mort_rate / mean_cen_length)*365) %>%
  ggplot(mapping = aes(x = census, y = annual_mort_rate, color = pft))+ geom_line()


#calculating average annual background mort rate

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

default_background_seedling_mort <- c(rep(background_seedling_mort[1],2),rep(background_seedling_mort[2],2))
