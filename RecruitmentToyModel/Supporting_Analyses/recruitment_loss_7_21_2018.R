library(tidyverse)
library(magrittr)
library(reshape2)

setwd("C:/Users/ahanb/OneDrive/Documents/RecruitmentToyModel/RecruitmentToyModel")
seed_dyn <- readRDS("bci_seeding_data_for_Adam.RDS")
load("bci.spptable.RData")
pfts_7_27_2018 <- read.csv("canopy_sp_pfts_7_27_2018.csv")

#cleaning data
seed_dyn$OBnum <- seq(1:length(seed_dyn$TAGF))
seed_dyn$start.date <- as.Date(seed_dyn$start, origin = as.Date("2001-1-15", format = "%Y-%m-%d"))
seed_dyn$stop.date <- as.Date(seed_dyn$stop, origin = as.Date("2001-1-15", format = "%Y-%m-%d"))
seed_dyn$sp <- tolower(seed_dyn$SPP)

R <- as.numeric(as.numeric(seed_dyn$d) >= 10)
R[is.na(R)] <- 0
seed_dyn$R <- R

#removing observations where a recruit was censused more than once
rec_dupes <- seed_dyn %>%
  select(OBnum, TAGF, census, start, stop, Q20P5, x, y, h, d, R, mort, shd, dmin, ra, ra.d) %>%
  arrange(TAGF, census) %>%
  filter(R == 1) %>%
  mutate(dupes = duplicated(TAGF)) %>%
  filter(dupes == TRUE) %>%
  .$OBnum

seed_dyn <- seed_dyn[!seed_dyn$OBnum %in% rec_dupes,]

seed_dyn %>% filter(census == 4) %>% filter(R == 1) 


#filtering to get just the canopy species
seed_dyn_c.x <- seed_dyn %>% filter(sp %in% pfts_7_27_2018$sp)
#adding pft as a variables
seed_dyn_c.x <- merge(seed_dyn_c.x, pfts_7_27_2018, by = "sp")
str(seed_dyn_c.x)

#finding the number of sampled area of 5x5 m quadrats in the filtered comita / johnson dataset
n_quads <- seed_dyn_c.x$Q20P5 %>% unique(.) %>% length(.)

#finding the multiplier to get the equivalent at 50 ha
multiplier <- 1 / ((n_quads * 25) / 500000)

#getting the recruitment rate per year
seed_dyn_c.x %>% select(pft, census, start, stop, start.date, stop.date, R) %>%
  group_by(census, pft) %>%
  summarise(R.x = sum(R), start.d = mean(start.date), stop.d = mean(stop.date), stop_day = mean(stop), start_day = mean(start)) %>%
  mutate(int_length = stop_day - start_day) %>%
  mutate(R_per_yr = R.x / (int_length/365)) %>%
  mutate(R_per_yr_50_ha = R_per_yr*multiplier)


#finding






