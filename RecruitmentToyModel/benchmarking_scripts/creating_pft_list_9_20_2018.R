str(bci.full)

library(magrittr)
library(tidyverse)
library(vegan)


#function to convert Latin names to sp names
Latin2sp <- function(Latin.name){
  if(is.character(Latin.name) != T){Latin.name <- as.character(Latin.name)} 
  
  sp <- sp_code[sp_code$Latin == Latin.name,]$sp
  
  if(Latin.name %in% sp_code$Latin == F){return(Latin.name)}else{
    return(sp)}
}



#defining canopy species
canopy_species_bci <- bci.full %>% select(sp, dbh) %>% filter(dbh > 200) %>% .$sp %>% unique(.)
canopy_species_bci <- as.data.frame(canopy_species_bci)
names(canopy_species_bci) <- "sp"
#adding early vs. late (determined in other script from wsg following Powell et al., 2018 New Phyt.)

wsg.ctfs3$Latin <- paste(wsg.ctfs3$genus, wsg.ctfs3$species)
temp <- merge(wsg.ctfs3,bci.spptable, by = "Latin")[,c(1,2,5)]
names(temp)[3] <- "sp"
temp[temp$Latin == "Trema integerrima",]$sp <- "tremin"
canopy_species <- merge(as.data.frame(canopy_species_bci),temp, by = "sp") %>% na.omit(.)
write.csv(canopy_species, "canopy_sp_pfts_9_20_2018.csv")




#adding early versus late accoring to Powell 2018
pft <- rep(NA, length(canopy_species$sp))
pft[canopy_species$wsg >= 0.49] <- "late"
pft[canopy_species$wsg < 0.49] <- "early"
canopy_species$pft <- pft


#adding drought tolerant versus intolerant from Engelbrecht drought indices 
pfts_sept_2018 <- merge(d_indices, canopy_species, all.y = T)

#adding data from Harms 2001 on whats positively or negatively associated with the plateau

#importing the habitat associations from the harms 2001 paper
harms_pft <- read.csv("harms_habitat_associations.csv")
harms_pft$sp <- gsub(pattern = "\n", replacement = " ", x = harms_pft$sp)
harms_pft$stat <- as.character(harms_pft$stat)
names(harms_pft) <- c("Latin", "dpft")

#merging the values with the dpft data from Harmin
pfts_sept_2018 <- merge(pfts_sept_2018, harms_pft, by = "Latin", all.x = T)


#adding the values from the PNAS paper

# importing the moisture response data from the PNAS paper
moistr_resp <- read.table("TreeCommunityDrySeasonSpeciesResponse.txt", sep = '\t', header = T)
moistr_resp <- moistr_resp %>% select(Latin, occur, Inter, Moist, Moist.2)
moistr_resp$Latin <- lapply(X = strsplit(x = as.character(moistr_resp$Latin), split = " (", fixed = T), `[[`, 1) %>% unlist(.)


#merging this new list back with the pft data
pfts_sept_2018 <- merge(pfts_sept_2018, y = moistr_resp[,c("Latin", "Moist")], by = "Latin", all.x = T)
pfts_sept_2018$engel_dpft <- rep(0, length(pfts_sept_2018$Latin))

quantile(d_indices$d_index, probs = c(.33, .66), na.rm = T)

pfts_sept_2018 <- pfts_sept_2018 %>% mutate_at(c(2:4), funs(replace(., is.na(.), 0)))

pfts_sept_2018[pfts_sept_2018$d_index < 14.2 & pfts_sept_2018$d_index > 0,]$engel_dpft <- "dt"
pfts_sept_2018[pfts_sept_2018$d_index > 33.35,]$engel_dpft <- "di"

names(pfts_sept_2018)[c(5,6,8)] <- c("e_vs_l","harms_dt_vs_di", "engel_dt_vs_di")

pfts_sept_2018 <- pfts_sept_2018 %>% select(Latin, sp, wsg, e_vs_l, d_index, engel_dt_vs_di, harms_dt_vs_di, Moist)


#adding the PNAS categorization

quantile(pfts_sept_2018$Moist, na.rm = T)
pfts_sept_2018 <- pfts_sept_2018 %>% mutate_at(c(7:8), funs(replace(., is.na(.), 0)))

PNAS_dt_vs_di <- rep(0, length(pfts_sept_2018$Latin))

PNAS_dt_vs_di[pfts_sept_2018$Moist < -0.0001] <- "di"
PNAS_dt_vs_di[pfts_sept_2018$Moist > 0] <- "dt"


pfts_sept_2018$PNAS_dt_vs_di <- PNAS_dt_vs_di


#creating final dt_vs_di pfts
dpft <- c()
for(i in 1:length(pfts_sept_2018$Latin)){
  if(pfts_sept_2018$engel_dt_vs_di[i] != 0){dpft[i] <- pfts_sept_2018$engel_dt_vs_di[i]}else{
    if(pfts_sept_2018$harms_dt_vs_di != 0){dpft[i] <- pfts_sept_2018$harms_dt_vs_di[i]}else{
      dpft[i] <- pfts_sept_2018$PNAS_dt_vs_di[i]
    }
  }
}


pfts_sept_2018$dpft <- dpft


write.csv(pfts_sept_2018, file = "pfts_9_20_2018.csv")

