library(magrittr)
library(tidyverse)
library(reshape2)
library(ggplot2)


path_non_driver_data <- "C:/Users/ahanb/OneDrive/Documents/data/Non_Driver_Data/"
path_bench_output <- "C:/Users/ahanb/OneDrive/Documents/rec_submodel/benchmarking_output/"

#############benchmarking recruitment at BCI###################################

#first determining what are canopy species
####################getting rec rates by PFT at BCI##########
#species list of canopy trees, n = 164
canopy_species <- data.frame(sp = names(sp.Rlist_mindbh[[6]]$R))


save.image(sp.Rlist_mindbh, file = "sp.Rlist_mindbh.RData")


#doubling checking that this is a comprehensive list of canopy trees
dbh95_check <- data.frame(sp = spc[[1]], valid = spc[[2]], dbh95 = spc[[3]])
dbh95_check %>% filter(dbh95 >= 15)

#adding the species names to the canopy tree list
wsg.ctfs3$Latin <- paste(wsg.ctfs3$genus, wsg.ctfs3$species)
temp <- merge(wsg.ctfs3,bci.spptable, by = "Latin")[,c(1,2,5)]
names(temp)[3] <- "sp"
temp[temp$Latin == "Trema integerrima",]$sp <- "tremin"
canopy_species <- merge(canopy_species,temp, by = "sp") %>% na.omit(.)
write.csv(canopy_species, "canopy_sp_pfts_7_27_2018.csv")



#adding the pfts according to Tom's crtieria in his 2018 New Phyt paper
pft <- rep(NA, length(canopy_species$sp))
pft[canopy_species$wsg >= 0.49] <- "late"
pft[canopy_species$wsg < 0.49] <- "early"
canopy_species$pft <- pft


#adding basal area from 1995 - 2010
canopy_sp <- merge(data.frame(sp = row.names(sp.BAlist[[4]]$ba), ba = sp.BAlist[[4]]$ba$all), canopy_species)
names(canopy_sp)[2] <- "ba_1995" 
canopy_sp <- merge(data.frame(sp = row.names(sp.BAlist[[5]]$ba), ba = sp.BAlist[[5]]$ba$all), canopy_sp)
names(canopy_sp)[2] <- "ba_2000" 
canopy_sp <- merge(data.frame(sp = row.names(sp.BAlist[[6]]$ba), ba = sp.BAlist[[6]]$ba$all), canopy_sp)
names(canopy_sp)[2] <- "ba_2005" 
canopy_sp <- merge(data.frame(sp = row.names(sp.BAlist[[7]]$ba), ba = sp.BAlist[[7]]$ba$all), canopy_sp)
names(canopy_sp)[2] <- "ba_2010" 


#adding the species level recruitment data
canopy_sp <- merge(data.frame(sp = names(sp.Rlist_mindbh[[2]]$R), R = sp.Rlist_mindbh[[2]]$R), canopy_sp)
names(canopy_sp)[2] <- "R_85to90" 

canopy_sp <- merge(data.frame(sp = names(sp.Rlist_mindbh[[3]]$R), R = sp.Rlist_mindbh[[3]]$R), canopy_sp)
names(canopy_sp)[2] <- "R_90to95" 

canopy_sp <- merge(data.frame(sp = names(sp.Rlist_mindbh[[4]]$R), R = sp.Rlist_mindbh[[4]]$R), canopy_sp)
names(canopy_sp)[2] <- "R_95to00" 

canopy_sp <- merge(data.frame(sp = names(sp.Rlist_mindbh[[5]]$R), R = sp.Rlist_mindbh[[5]]$R), canopy_sp)
names(canopy_sp)[2] <- "R_00to05" 

canopy_sp <- merge(data.frame(sp = names(sp.Rlist_mindbh[[6]]$R), R = sp.Rlist_mindbh[[6]]$R), canopy_sp)
names(canopy_sp)[2] <- "R_05to10" 


#adding the species level abundance for one interval
canopy_sp <- merge(data.frame(sp = row.names(sp.Alist[[4]]$abund), A = sp.Alist[[4]]$abund$all), canopy_sp)
names(canopy_sp)[2] <- "A_95to00" 




#summarizing the benchmarking data at BCI
####################################################################################
#summarizing ba and recruitment by pft
#the basal area of early versus late is pretty even
summary_data <- canopy_sp %>% group_by(pft) %>%
  summarise_if(.predicate = is.numeric, .funs = funs(sum)) 
###################################################################################
######################the number of recruits per ha per year for early versus late##################
bci_rec_bench <- summary_data[c(3:7)] / (5*50)
colSums(bci_rec_bench[-1])
######################################################################
###################################################################################################

#determining the percent annual loss from the 1 cm size class by PFT (using weighted means)

#adding the annual mortality rates for each species
canopy_sp <- merge(data.frame(sp = row.names(sp.Mlist[[2]]$rate), M_rate = sp.Mlist[[2]]$rate[,1]), canopy_sp)
names(canopy_sp)[2] <- "M_85to90" 

canopy_sp <- merge(data.frame(sp = row.names(sp.Mlist[[3]]$rate), M_rate = sp.Mlist[[3]]$rate[,1]), canopy_sp)
names(canopy_sp)[2] <- "M_90to95" 

canopy_sp <- merge(data.frame(sp = row.names(sp.Mlist[[4]]$rate), M_rate = sp.Mlist[[4]]$rate[,1]), canopy_sp)
names(canopy_sp)[2] <- "M_95to00" 

canopy_sp <- merge(data.frame(sp = row.names(sp.Mlist[[5]]$rate), M_rate = sp.Mlist[[5]]$rate[,1]), canopy_sp)
names(canopy_sp)[2] <- "M_00to05" 

canopy_sp <- merge(data.frame(sp = row.names(sp.Mlist[[6]]$rate), M_rate = sp.Mlist[[6]]$rate[,1]), canopy_sp)
names(canopy_sp)[2] <- "M_05to10" 


#adding the total percent of recruitment that each species is responsible for
canopy_sp2 <- canopy_sp %>% mutate(R_total = rowSums(.[8:12])) %>%
  mutate(pct_total_R = R_total / sum(R_total)) 

#creating weighted means of the mortality rates in the 1 cm size class
canopy_sp2_l <- canopy_sp2 %>% filter(pft == "late")
canopy_sp2_l <- do.call(data.frame,lapply(canopy_sp2_l, function(x) replace(x, is.infinite(x),NA)))

mort_rates_late <- canopy_sp2_l %>%
  mutate(pct_total_R = R_total / sum(R_total)) %>%
  mutate(M_00to05_wgt = M_00to05 * pct_total_R) %>%
  mutate(M_05to10_wgt = M_05to10 * pct_total_R) %>%
  group_by(pft) %>%
  summarise(w_mean_M_00to05 = sum(M_00to05_wgt, na.rm = T), w_mean_M_05to10 = sum(M_05to10_wgt, na.rm = T))


canopy_sp2_e <- canopy_sp2 %>% filter(pft == "early")
canopy_sp2_e <- do.call(data.frame,lapply(canopy_sp2_e, function(x) replace(x, is.infinite(x),NA)))


mort_rates_early <- canopy_sp2_e %>%
  mutate(pct_total_R = R_total / sum(R_total)) %>%
  mutate(M_00to05_wgt = M_00to05 * pct_total_R) %>%
  mutate(M_05to10_wgt = M_05to10 * pct_total_R) %>%
  group_by(pft) %>%
  summarise(w_mean_M_00to05 = sum(M_00to05_wgt, na.rm = T), w_mean_M_05to10 = sum(M_05to10_wgt, na.rm = T))

mort_rates <- rbind(mort_rates_early, mort_rates_late)

#early 00-05
mort_rates <- mort_rates %>% select(1,3,2)
bci_rec_bench <- cbind(c("early","late"), bci_rec_bench)
names(bci_rec_bench)[1] <- "pft"

#adding the mortality loss back to the recruitment rates
rec_adjust <- function(R, M){
  R_adjust <- (R / (1 - M))
  return(R_adjust)
}

xxx <- reshape2::melt(data = bci_rec_bench[1:3], id.vars = "pft", variable.name = "year")
names(xxx)[3] <- "R" 
yyy <- reshape2::melt(data = mort_rates, id.vars = "pft", variable.name = "year")
names(yyy)[3] <- "M" 

rec_adjust_data <- cbind(xxx, yyy)[c(1:3,6)]

for(i in 1:4){
  rec_adjust_data$R_adj[i] <- rec_adjust(R = rec_adjust_data$R[i], M = rec_adjust_data$M[i])
}
rec_adjust_data









###################
#Trying to come up with drought pft allocations using the drought index values in the Engelbrecht 2007 (Nature paper)
#################

#importing the drought index data from Engelbrecht 2007 (Nature paper)
not_in <- d_indices[!d_indices$Latin %in% canopy_species$Latin,]
d_indices <- read.csv(paste0(path_non_driver_data,"drought_indices_Engelbrecht_2007.csv"))
not_in_sp <- merge(not_in,bci.spptable,by = "Latin")$sp #creating a species list of whats not in the canopy species


#checking how many species are in both data sets
sum(d_indices$Latin %in% canopy_species$Latin) #19 sp in common

#checking how many are in the bci species table
sum(d_indices$Latin %in% bci.spptable$Latin) #44 are in the bci FDP species table

#checking how many are in the bci FDP
bcisp <- bci.full7$sp %>% unique(.)
sum(not_in_sp %in% bcisp) #another 19 species are in the bci FDP data 

#but they have dbh95 values less than 15 cm (not bci data is in mm)
bci.full7 %>% filter(sp %in% not_in_sp) %>% group_by(sp) %>% summarise(dbh95 = quantile(x = dbh, probs = 0.95, na.rm = T, names = F))#an additional 19 species that have a drought index are not technically canopy trees according to the dbh95 criteria

#seeing what fraction of recruits are covered by the species that have a drought index
tmp <- merge(d_indices,canopy_species,by = "Latin")
d_index_species <- merge(tmp,canopy_sp2,by = "sp")
d_index_species %>% group_by(pft.x) %>% summarise(pct_total = sum(pct_total_R))#27%



#if we changed criteria to be just one species larger than 20 cm in dbh it would include another 8 species, but I'd have to rerun the recruitment benchmarks to see what that would do.
tmpx <- bci.full7 %>% filter(sp %in% not_in_sp) %>% group_by(sp) %>% summarise(maxdbh = max(x = dbh, na.rm = T)) %>% .[.$maxdbh > 200,]

















########benchmarking with Ruger data and model##########################
#using the pft assignments to create ruger data
ruger_params <- read.csv(paste0(path_non_driver_data,"ruger_params_clean.csv"))
str(ruger_params)
names(ruger_params)[1] <- "Latin"
ruger_clean <- merge(ruger_params[c(1,5,11)], canopy_sp[c(1,12,14, 17, 19)], by = "Latin")
ruger_clean$b90_95 <- as.numeric(as.character(ruger_clean$b90_95))


#######the species level mean b parameter for recruitment rates as a function of light 
b_param <- ruger_clean %>% group_by(pft) %>% summarise(b = mean(b90_95, na.rm = T)) %>% .$b
###############################################

#for the a parameter I'm using the number of recruits per 5x5 m quadrat over the 05_10 census interval
a_param <- log10(rec_adjust_data$R[1:2] / (10000/25)) 


#generating becnhmarking data using Ruger et al

l  <- 1:100
Rs.e <- 10^(a_param[1] + b_param[1] * log10(l)) * 400
Rs.l <- 10^(a_param[2] + b_param[2] * log10(l)) * 400

Ruger_bench_data <- data.frame(l = l, early = Rs.e, late = Rs.l)
Ruger_bench_data <- melt(data = Ruger_bench_data, id.vars = "l", variable.name = "pft") 

#generating benchmarking data with Ruger
Ruger_bench_data %>% ggplot(mapping = aes(x = l, y = value, color = pft)) + geom_line() +
  scale_y_continuous(name = "N _recruits_ha_yr")+
  scale_x_continuous(name = "percent light")

#showing just the range where Ruger actually had observations <15 percent light
Ruger_bench_data %>% filter(l <= 16) %>% ggplot(mapping = aes(x = l, y = value, color = pft)) + geom_line() +
  scale_y_continuous(name = "N _recruits_ha_yr")+
  scale_x_continuous(name = "percent light")

write.csv(Ruger_bench_data, paste0(path_bench_output,"Ruger_bench_data_8_28_2018.csv"))


















































