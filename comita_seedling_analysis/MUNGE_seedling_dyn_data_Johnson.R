library(tidyverse)
library(magrittr)
library(ggplot2)

#creating a serial number for each observation
seed_dyn$OBnum <- seq(1:length(seed_dyn$TAGF))

#creating a variable to indicate a recruit
R <- as.numeric(as.numeric(seed_dyn$d) >= 10)
R[is.na(R)] <- 0
seed_dyn$R <- R

#adding pfts 

#Checking that pft species are in the seedling dynamcis plots
pfts$SPP <- toupper(pfts$sp.x)
seed_spp <- unique(seed_dyn$SPP)
pfts$SPP %in% seed_spp

seed_dyn$pft <- rep("unk", length(seed_dyn$TAGF))
seed_dyn[seed_dyn$SPP %in% pfts[pfts$pft == "e",]$SPP,]$pft <- "e"
seed_dyn[seed_dyn$SPP %in% pfts[pfts$pft == "l",]$SPP,]$pft <- "l"
seed_dyn[seed_dyn$SPP %in% pfts[pfts$pft == "ml",]$SPP,]$pft <- "ml"
seed_dyn[seed_dyn$SPP %in% pfts[pfts$pft == "me",]$SPP,]$pft <- "me"

seed_dyn$pft <- factor(seed_dyn$pft, levels = c("e", "me", "ml", "l", "unk"))


#removing observations where a recruit was censuses more than once
rec_dupes <- seed_dyn %>%
  select(OBnum, TAGF, census, start, stop, Q20P5, x, y, h, d, R, mort, shd, dmin, ra, ra.d) %>%
  arrange(TAGF, census) %>%
  filter(R == 1) %>%
  mutate(dupes = duplicated(TAGF)) %>%
  filter(dupes == TRUE) %>%
  .$OBnum

seed_dyn <- seed_dyn[!seed_dyn$OBnum %in% rec_dupes,]

#querying the dataframe for statistical analysis
seed_dyn_c <- seed_dyn %>%
  select(TAGF, census, start, stop, Q20P5, R, shd, pft, Q20) %>%
  filter(pft == "l") %>%
  group_by(Q20P5, census) %>%
  summarise(start.x = min(start), stop.x = min(stop), shade = unique(shd), Nt_1 = length(unique(TAGF)), N_trans = sum(R))


#exploring the data
ggplot(data = seed_dyn_c, mapping = aes(x = as.character(N_trans), y = shade)) +
  geom_boxplot() 



#pick up here with exploring these relationships with subsets of the data
#doing the same as above buy on a larger spatial scale... i.e. at the 20X20 m quadrat level
seed_dyn_c <- seed_dyn %>%
  select(TAGF, census, start, stop, Q20, R, shd, pft) %>%
  filter(pft == "e") %>%
  group_by(census, Q20) %>%
  summarise(start.x = mean(start), stop.x = mean(stop), shade = mean(shd), Nt_1 = length(unique(TAGF)), N_trans = sum(R))


ggplot(data = seed_dyn_c, mapping = aes(x = as.character(N_trans), y = shade)) +
  geom_boxplot()


hist(seed_dyn_c$N_trans)


test <- seed_dyn %>%
  select(TAGF, census, start, stop, Q20, Q20P5, R, shd, pft) %>%
  filter(Q20P5 == 101144, census == 11) %>%
  .[,"TAGF"] %>%
  length(.)
  
  group_by(census, Q20) %>%
  summarise(start.x = mean(start), stop.x = mean(stop), shade = mean(shd), Nt_1 = length(unique(TAGF)), N_trans = sum(R))






















