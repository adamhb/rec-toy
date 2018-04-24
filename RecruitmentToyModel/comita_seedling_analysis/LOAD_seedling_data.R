library(tidyverse)
library(magrittr)
library(reshape2)

setwd("C:/Users/ahanb/OneDrive/Documents/RecruitmentToyModel/RecruitmentToyModel")

#loading the seedling dynamics data
com_sdlg <- read.csv("comita_seedling.csv")
head(com_sdlg)
load("condit_shade.Rdata")
#post-1989 shade index is for 0.5m above the ground
seed_dyn <- readRDS("bci_seeding_data_for_Adam.RDS")
pfts <- read.csv("PFTs.csv")
load("bci.spptable.RData")
solar_bci <- read.csv("daily_solar_insolation_bci.csv")
pfts2 <- read.csv("wright_spring_2018.csv")


max(seed_dyn_c$stop.date.x)










