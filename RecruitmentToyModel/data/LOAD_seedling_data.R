library(tidyverse)
library(magrittr)
library(reshape2)

com_sdlg <- read.csv("comita_seedling.csv")
head(com_sdlg)

load("condit_shade.Rdata")

seed_dyn <- readRDS("bci_seeding_data_for_Adam.RDS")


str(seed_dyn)


#post-1989 shade index is for 0.5m above the ground
str(canopyht5x5)









