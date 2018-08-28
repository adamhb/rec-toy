#graphs for the R03 call


setwd("C:/Users/ahanb/OneDrive/Documents/Recruitment_Toy_Model_non_Git/Submodel Output/Output_for_ESA_2018")

#defining graphing objects
rec_var <- expression(paste('N recruits'," ha"^"-1"," yr"^"-1"))

pft.cols <- c("darkolivegreen2","darkolivegreen4","lightskyblue", "midnightblue")

early.bench <- 15
late.bench <- 35




#graphing the recruitment rates with just the totals and comparing FATES current to the submodel
pft_total <- full_output %>% filter(date >= as.POSIXct("2005-01-01")) %>% group_by(date) %>% summarise(total_R = sum(R)) %>% .$total_R

FATES_current_R <- FATES_current_data %>% filter(date >= as.POSIXct("2005-01-01"))
FATES_current_just_R <- FATES_current_R %>% .$R

graph1_data <- data.frame(date = FATES_current_R$date, submodel = pft_total, FATES_current = FATES_current_just_R)

graph1_data_melt <- reshape2::melt(graph1_data, id.vars = "date", variable.name = "run")
graph1_data_melt$bci.data <- rep("bci data",length(graph1_data_melt$date))
names(graph1_data_melt)[4] <- "bci data"



fates_vs_submodel <- ggplot(data = graph1_data_melt, aes(x = as.Date(date), y = value*365, color = run)) +
  #geom_line(size = 1.8)+
  geom_smooth(size = 1.8, method = "loess", span = .01, se = F)+
  year_axis +
  ylab(expression(paste('N recruits'," ha"^"-1"," yr"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'Current FATES vs. New Submodel') +
  scale_color_manual(values = c("black", "red")) +
  #geom_segment(aes(x=as.Date("2003-01-01"),xend=as.Date("2005-01-01"),y=92,yend=92), size = 1.8, color = "yellow")+
  geom_segment(aes(x=as.Date("2005-01-01"),xend=as.Date("2010-01-01"),y=early.bench+late.bench,yend=early.bench+late.bench, linetype = "bci data"), size = 2.5, color = "darkorchid2")+
  #scale_linetype_manual(values = "yellow2")+
  scale_linetype_manual("bci data",values=c("bci data"=1))+
  theme_classic() +
  adams_theme #+
#geom_smooth(data = graph1_data_melt, 
#mapping = aes(x = as.Date(date), y = FATES_current*365),
#size = 1.8, method = "loess", span = .1, se = F)

#%>% filter(date > as.POSIXct("2005-01-01 11:30:00 UTC")
setwd("C:/Users/ahanb/OneDrive/Documents/Recruitment_Toy_Model_non_Git/Submodel Output/Output_for_ESA_2018")
png(paste0("./Current_FATES_vs_NewSubModel.png"), height=5, width=8, units="in", res = 100)
fates_vs_submodel
dev.off()
















#graphing the annual recruitment rate (as a instantaneous daily rate) of the submodel with the totals and all pfts
Rec_rate_submodel_annualized_daily <- full_output %>% filter(date >= as.POSIXct("2005-01-01")) %>% arrange(desc(pfts)) %>% ggplot( aes(x = as.Date(date), y = R*365, color = pfts)) +
  geom_smooth(size = 1.8, method = "loess", span = .05, se = F)+
  #smoother_line +
  year_axis +
  ylab(expression(paste('N recruits'," ha"^"-1"," yr"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'New Submodel') +
  geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), mapping = aes(x = as.Date(date), y = total_R*365, color = pft), size = 1.8, method = "loess", span = .1, se = F) +
  #geom_point(data = N_recs_per_year, mapping = aes(x = year, y = N_rec, color = "year_total"), size = 8) +
  geom_segment(aes(x=as.Date("2005-01-01"),xend=as.Date("2010-01-01"),y=early.bench+late.bench,yend=early.bench+late.bench, linetype = "bci data"), size = 2.5, color = "darkorchid2")+
  #scale_linetype_manual(values = "yellow")+
  scale_color_manual(values = c(pft.cols,"black")) +
  #scale_linetype_manual(values= c("solid", "solid", "solid", "solid", "dotted"))
  #legend(lty = c(rep("solid", 4),"dotted")) +
  theme(legend.title=element_blank()) +
  theme_classic() +
  adams_theme


setwd("C:/Users/ahanb/OneDrive/Documents/Recruitment_Toy_Model_non_Git/Submodel Output/Output_for_ESA_2018")
png(paste0("./NewSubModel_Recruitment_Annualized_Daily_Rate.png"), height=5, width=8, units="in", res = 100)
Rec_rate_submodel_annualized_daily
dev.off()











##################################################
#submodel annual sum of recruitment with benchmark
####################################################
#creating data of the number of recruits per year

N_recs_per_year_pfts <- full_output %>% mutate(year = substring(text = as.character(date), first = 1, last = 4)) %>% group_by(year, pft) %>% summarise(N_rec = sum(R)) 

N_recs_per_year_pfts$year <- as.Date(paste0((as.numeric(N_recs_per_year_pfts$year)+1), "-01-01"))


#getting the global average of model output by PFT

N_recs_per_year_pfts %>% filter(year >= as.Date("2005-01-01")) %>% group_by(pft) %>% summarise(rec_mean = mean(N_rec))



Submodel_annual_rec <- ggplot() +
  geom_point(data = N_recs_per_year_pfts %>% filter(year >= as.Date("2005-01-01")), mapping = aes(x = year, y = N_rec, color = pft), size = 8) +
  geom_segment(aes(x=as.Date("2005-01-01"),xend=as.Date("2010-01-01"),y=early.bench,yend=early.bench), size = 1.8, color = pft.cols[2])+
  geom_segment(aes(x=as.Date("2005-01-01"),xend=as.Date("2010-01-01"),y=late.bench,yend=late.bench,linetype = "bci data"), size = 1.8, color = pft.cols[4])+
  #smoother_line +
  ylab(expression(paste('N recruits'," ha"^"-1"," yr"^"-1")))+
  xlab(bquote('year'))+
  year_axis +
  labs(title = 'New Submodel') +
  #geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), mapping = aes(x = as.Date(date), y = total_R*365, color = pft), size = 1.8, method = "loess", span = .1, se = F) +
  scale_color_manual(values = c(pft.cols[2], pft.cols[4]))+
  theme_classic() +
  adams_theme 
  
  #scale_color_manual(values = c("green3","blue1"))

Submodel_annual_rec
#scale_linetype_manual("segment legend",values=c("segment legend"=2))


setwd("C:/Users/ahanb/OneDrive/Documents/Recruitment_Toy_Model_non_Git/Submodel Output/Output_for_ESA_2018")
png(paste0("./Recruitment_Annual_Sums.png"), height=5, width=8, units="in", res = 100)
Submodel_annual_rec
dev.off()





#############################################
#seasonal cycle#############################


#making an average seasonal cycle for the fraction emerging
seasonal_cycle_data <- full_output %>% filter(date >= as.POSIXct("2005-01-01")) %>% select(date, frac_emerging) %>% 
  mutate(day = lubridate::yday(date)) %>%
  group_by(day) %>%
  summarise(frac_emerg_d = mean(frac_emerging), sd = sd(frac_emerging), n = length(frac_emerging)/4) %>%
  mutate(se = sd / sqrt(n)) %>%
  mutate(se_up = frac_emerg_d + se, se_down = frac_emerg_d - se)



#adding precip. at bci as a second axis
raingra <- bci_precip_new %>% filter(date >= as.Date("2005-01-01")) %>% select(date,rain) %>% 
  mutate(day = lubridate::yday(date)) %>%
  group_by(day) %>%
  summarise(rainm = mean(rain))


seasonal_cycle_data$rain <- raingra$rainm

seasonal_cycle_data$FATES_current <- rep(0.0013, length(seasonal_cycle_data$rain))
names(seasonal_cycle_data)[2] <- "New_Submodel"

seasonal_cycle_data <- reshape2::melt(data = seasonal_cycle_data, measure.vars = c("New_Submodel", "FATES_current"), variable_name = "type")

names(seasonal_cycle_data)[8] <- "Model"



  seaonal_cycle_emergence <- ggplot(data = seasonal_cycle_data, mapping = aes(x = day, y = New_Submodel)) +
  geom_linerange(mapping = aes(x = day, ymax = se_up, ymin = se_down)) +
  geom_line(mapping = aes(x = day, y = FATES_current), color = "red", size = 1.8) +
  scale_y_continuous(sec.axis = sec_axis(~. *300, name = "daily precip. (mm)")) +
  geom_smooth(mapping = aes(x = day, y = rain/300), color = "blue", size = 1.8, method = "loess", span = .1, se = F) +
  #year_axis +
  ylab(expression(paste("frac. of seedbank emerging "," (day)"^"-1")))+
  xlab(bquote('day of the year'))+
  labs(title = expression(paste("Seasonality of Emergence"))) +
  theme_classic() +
  adams_theme + 
  theme(legend.position = "none", axis.text.y.right = element_text(color = "blue"), axis.title.y.right = element_text(color = "blue"))

  setwd("C:/Users/ahanb/OneDrive/Documents/Recruitment_Toy_Model_non_Git/Submodel Output/Output_for_ESA_2018")
  png(paste0("./Seasonality_of_Emergence.png"), height=5, width=8, units="in", res = 100)
  seaonal_cycle_emergence
  dev.off()


































#creating slides to show recruitment rates under varying cohort size, light, and drought conditions.
#creating the data for the graphs
#run_comparisons <- list()
#run_comparisons[[run_name]] <- full_output %>% mutate(year = substring(text = as.character(date), first = 1, last = 4)) %>% group_by(pft,dpft,year) %>% summarise(N_rec = sum(R)) %>% summarise(avg_annual_n_rec = mean(N_rec)) %>% mutate(age = "mature", light = "3pct", moisture = "dry")

run_comps <- rbind(run_comparisons[[1]], run_comparisons[[2]], run_comparisons[[3]], run_comparisons[[4]], run_comparisons[[5]], run_comparisons[[6]], run_comparisons[[7]], run_comparisons[[8]])

run_comps$light <- paste0(run_comps$light,"_","light")
run_comps$light <- gsub(x = run_comps$light, pattern = "15 pct_light", replacement = "15pct_light")

#run_comps$moisture <- plyr::mapvalues(x = run_comps$moisture, from = "reg", to = "amb.")

#creating slides to show recruitment rates under varying cohort size, light, and drought conditions.

#mature cohort
Mature_Cohort <- ggplot(run_comps %>% mutate(PFT = as.factor(paste0(pft,dpft))) %>% filter(age == "mature"), aes(x = moisture, y = avg_annual_n_rec, fill = PFT)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ light)+
  scale_fill_manual(values = pft.cols)+ 
  labs(title = "Mature Cohort (50cm)")+
  theme (plot.title = element_text (hjust = 0.5, size = 20),
         legend.title = element_blank (),
         strip.text.x = element_text(size = 15),
         axis.title.x = element_text (size = 20), # change the axis title
         axis.title.y = element_text (size = 20),
         axis.text.x = element_text (size = 15, colour = "black"),
         axis.text.y = element_text (size = 15, colour = "black"),
         legend.text = element_text (size = 15))+
  xlab("")+
  ylab(rec_var)


setwd("C:/Users/ahanb/OneDrive/Documents/Recruitment_Toy_Model_non_Git/Submodel Output/Output_for_ESA_2018")
png(paste0("Mature_Cohort_Bar_Graph.png"), height=5, width=8, units="in", res = 100)
Mature_Cohort
dev.off()
#young cohort
Young_Cohort <- ggplot(run_comps %>% mutate(PFT = as.factor(paste0(pft,dpft))) %>% filter(age == "young"), aes(x = moisture, y = avg_annual_n_rec, fill = PFT)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ light)+
  scale_fill_manual(values = pft.cols)+ 
  labs(title = "Young Cohort (3 cm)")+
  theme (plot.title = element_text (hjust = 0.5, size = 20),
         legend.title = element_blank (),
         strip.text.x = element_text(size = 15),
         axis.title.x = element_text (size = 20), # change the axis title
         axis.title.y = element_text (size = 20),
         axis.text.x = element_text (size = 15, colour = "black"),
         axis.text.y = element_text (size = 15, colour = "black"),
         legend.text = element_text (size = 15))+
  xlab("")+
  ylab(rec_var)


setwd("C:/Users/ahanb/OneDrive/Documents/Recruitment_Toy_Model_non_Git/Submodel Output/Output_for_ESA_2018")
png(paste0("Young_Cohort_Bar_Graph.png"), height=5, width=8, units="in", res = 100)
Young_Cohort
dev.off()





#comparison to Ruger
#generating output at various light levels to compare to Ruger 

#light_output

SubModel_light_variation <- rbind(light_output[[1]], light_output[[2]], light_output[[3]], light_output[[4]], light_output[[5]])
SubModel_light_variation$light <- as.numeric(SubModel_light_variation$light) *100

Ruger_bench_data <- read.csv("ruger_bench_data.csv")

Benchmarking_to_Ruger <- Ruger_bench_data %>% filter(l <= 16) %>% ggplot(mapping = aes(x = l, y = value, color = pft)) + geom_line(size = 1.8) +
  ylab(expression(paste('N recruits'," ha"^"-1"," yr"^"-1")))+
  xlab(bquote('Percent Light'))+
  scale_color_manual(values = c(pft.cols[2],pft.cols[4])) +
  geom_point(data = SubModel_light_variation, mapping = aes(x = light, y = avg_annual_n_rec, color = pft), size = 8) +
  #scale_linetype_manual(values= c("solid", "solid", "solid", "solid", "dotted"))
  #legend(lty = c(rep("solid", 4),"dotted")) +
  theme(legend.title=element_blank()) +
  theme_classic() +
  adams_theme

setwd("C:/Users/ahanb/OneDrive/Documents/Recruitment_Toy_Model_non_Git/Submodel Output/Output_for_ESA_2018")
png(paste0("Ruger_Benchmark.png"), height=5, width=8, units="in", res = 100)
Benchmarking_to_Ruger
dev.off()
























#reproductive status

repro_size_demo <- data.frame(size = 0:500, Dmax_500mm = prob_repro(size_mm = 0:500, Dmax = 500), Dmax_200mm = prob_repro(size_mm = 0:500, Dmax = 200), FATES_current = c(rep(0,150),rep(1,351)))

repro_size_demo_melt <- melt(repro_size_demo, id.vars = "size", variable.name = "type")

repro_status_graph <- ggplot(data = repro_size_demo_melt, mapping = aes(x = size, y = value, color = type))+
  geom_line(size = 1.8)+
  ylab(expression(paste('Probability Reproductive')))+
  xlab(bquote('dbh (mm)'))+
  labs(title = 'New Submodel Vs. FATES Current') +
  theme_classic() +
  adams_theme +
  scale_color_manual(values = c("black","gray","red"))



png(paste0("Repr_status.png"), height=5, width=8, units="in", res = 100)
repro_status_graph
dev.off()


#seedling emergence

emerg_data <- data.frame(precip = rain, New_Submodel = frac_emerg_func(rain), FATES_current = rep(0.0013, length(rain)))

emerg_data <- melt(data = emerg_data, id.vars = "precip", variable.name = "type")

emerg_model <- ggplot(data = emerg_data, mapping = aes(x = precip, y = value, color = type)) + geom_line(size = 1.8)+
  ylab(expression(paste('fraction of the seedbank emerging per day')))+
  xlab(bquote('prior week precipitation (mm)'))+
  labs(title = 'Precip-dependent Seedling Emergence') +
  theme_classic()+
  adams_theme + 
  scale_color_manual(values = c("black","red"))


png(paste0("emergence_graph.png"), height=5, width=8, units="in", res = 100)
emerg_model
dev.off()




#light-based mortality
#early

light.x <- seq(from = 1e4, to = 15e6, length = 1000)
lightmort_e <- c()
for(i in 1:length(light.x)){
  lightmort_e[i] <- light_mort(light = light.x[i], PFT = "early")
}

plot(light.x, lightmort_e, main = "early")



#late
lightmort_l <- c()
for(i in 1:length(light.x)){
  lightmort_l[i] <- light_mort(light = light.x[i], PFT = "late")
}
plot(light.x, lightmort_l, main = "late PFT")

light_mort_data <- data.frame(light = light.x, early = lightmort_e, late = lightmort_l)
light_mort_data <- melt(light_mort_data, id.vars = "light", variable.name = "pft")


light_dep_mort <- ggplot(data = light_mort_data, mapping = aes(x = light, y = value, color = pft))+
  geom_line(size = 1.8)+
  ylab(expression(paste('Fraction of Seedbank Dying Per Day')))+
  xlab(bquote('seedling layer light (Joules per day)'))+
  labs(title = 'Light-dependent Seedling Mortality') +
  theme_classic() +
  adams_theme +
  scale_color_manual(values = c("darkolivegreen2", "royalblue"))

png(paste0("light_dep_mort.png"), height=5, width=8, units="in", res = 100)
light_dep_mort
dev.off()




####################3
######THIS GRAPH IS SCRATCH#########
#graphing light-dep mort over time (3% light)- one option
light_dep_mort_30pct <- ggplot(data = full_output %>% filter(date >= as.POSIXct("2005-01-01")), aes(x = as.Date(date), y = light_mort_rate, color = pft)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(bquote('mortality rate (% of seedling pool)'))+
  xlab(bquote('year'))+
  labs(title = 'light-dependent seedling mortality') +
  #scale_color_manual(values = c("darkolivegreen2", "royalskyblue"))
  theme_classic() +
  adams_theme +
  scale_color_manual(values = c("darkolivegreen2", "midnightblue"))
png(paste0("light_dep_mort_30pct.png"), height=5, width=8, units="in", res = 100)
light_dep_mort_30pct
dev.off()










#option I'm using
#graphing the light mortality rate
light_mort_graph_3pct <- ggplot(data = full_output %>% filter(date >= as.POSIXct("2005-01-01")), aes(x = as.Date(date), y = light_mort_rate, color = pft)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(bquote('daily mort rate (% of seedling pool)'))+
  xlab(bquote('year'))+
  scale_color_manual(values = c("darkolivegreen4", "midnightblue"))+
  labs(title = 'light-dep. seedling mortality (3% light)') +
  theme_classic() +
  adams_theme
setwd("C:/Users/ahanb/OneDrive/Documents/Recruitment_Toy_Model_non_Git/Submodel Output/Output_for_ESA_2018")
png(paste0("./light_mort_3pct.png"), height=5, width=8, units="in", res = 100)
light_mort_graph_3pct
dev.off()


#SAME AS ABOVE BUT FOR 30% LIGHT


light_mort_graph_30pct <- ggplot(data = full_output %>% filter(date >= as.POSIXct("2005-01-01")), aes(x = as.Date(date), y = light_mort_rate, color = pft)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(bquote('daily mort rate (% of seedling pool)'))+
  xlab(bquote('year'))+
  scale_color_manual(values = c("darkolivegreen4", "midnightblue"))+
  labs(title = 'light-dep. seedling mortality (30% light)') +
  theme_classic() +
  adams_theme
setwd("C:/Users/ahanb/OneDrive/Documents/Recruitment_Toy_Model_non_Git/Submodel Output/Output_for_ESA_2018")
png(paste0("./light_mort_30pct.png"), height=5, width=8, units="in", res = 100)
light_mort_graph_30pct
dev.off()


















#frac rec graphs model relationship graphs
#late
frac_rec_l <- c()
for(i in 1:length(light.x)){
  frac_rec_l[i] <- frac_rec(light = light.x[i], PFT = "late",N_smp = 400)
}
#model prediction for number of recruits per year for early pfts
plot(light.x/1.5e7, (frac_rec_l*30000/65)*365, main = "late PFT")

#early
frac_rec_e <- c()
for(i in 1:length(light.x)){
  frac_rec_e[i] <- frac_rec(light = light.x[i], PFT = "early",N_smp = 400)
}
#model prediction for number of recruits per year for early pfts
plot(light.x, ((frac_rec_e*30000)/65)*365, main = "early PFT")

#combining both PFTs together
frac_rec_vs_light <- data.frame(light = light.x, early = frac_rec_e, late =  frac_rec_l)
frac_rec_vs_light <- melt(frac_rec_vs_light, id.vars = "light", variable.name = "pft")

light_dep_trans <- ggplot(data = frac_rec_vs_light, mapping = aes(x = light, y = value, color = pft))+
  geom_line(size = 1.8)+
  ylab(expression(paste("Seedling trans. rate (day)"^-1)))+
  xlab(expression(paste('Seedling layer light J (day)'^-1)))+
  labs(title = 'Light-dependent Seedling Transition') +
  theme_classic() +
  adams_theme +
  scale_x_continuous(labels = scales::scientific, limits = c(0,2e6))+
  scale_color_manual(values = c("springgreen2", "royalblue"))

png(paste0("light_dep_trans.png"), height=5, width=8, units="in", res = 100)
light_dep_trans
dev.off()






#graphing the fraction of the seedbank emerging each day (comparing FATES to the new submodel)

data_4_emerg_graph <- full_output %>% select(date, frac_emerging)
names(data_4_emerg_graph) <- c("date", "New_Submodel")
data_4_emerg_graph$FATES_current <- rep(0.0013, length(data_4_emerg_graph$New_Submodel)) 

data_4_emerg_graph <- melt(data_4_emerg_graph, id.vars = "date", variable.name = "Model")

Seedling_Emergence_FATES_vs_New <- ggplot(data = data_4_emerg_graph, aes(x = as.Date(date), y = value, color = Model)) +
  geom_smooth(size = 1.8, method = "loess", span = .01, se = F)+
  year_axis +
  ylab(expression(paste("frac. of seedbank emerging"," (day)"^"-1")))+
  xlab(bquote('year'))+
  labs(title = expression(paste("Seedling Emergence"," (day)"^"-1"))) +
  theme_classic() +
  adams_theme + 
  scale_color_manual(values = c("black","red"))


png(paste0("Seedling_Emergence_FATES_vs_New.png"), height=5, width=8, units="in", res = 100)
Seedling_Emergence_FATES_vs_New
dev.off()

#graphing the fractino of the seedbank transitioning each day




seedling_trans_rate_3pct <- ggplot(data = full_output, aes(x = as.Date(date), y = frac_rec.t, color = pft)) +
  smooth_line +
  year_axis +
  ylab(expression(paste('trans. rate (% of seedling pool'," day"^"-1",")")))+
  xlab(bquote('year'))+
  labs(title = 'Seedling Transition Rate') +
  theme_classic() +
  adams_theme +
  scale_color_manual(values = c("springgreen2", "royalblue"))
  

png(paste0("seedling_trans_rate_3pct.png"), height=5, width=8, units="in", res = 100)
seedling_trans_rate_3pct
dev.off()


seedling_trans_rate_30pct <- ggplot(data = full_output, aes(x = as.Date(date), y = frac_rec.t, color = pft)) +
  smooth_line +
  year_axis +
  ylab(expression(paste('trans. rate (% of seedling pool'," day"^"-1",")")))+
  xlab(bquote('year'))+
  labs(title = 'Seedling Transition Rate') +
  theme_classic() +
  adams_theme +
  scale_color_manual(values = c("springgreen2", "royalblue"))


png(paste0("seedling_trans_rate_30pct.png"), height=5, width=8, units="in", res = 100)
seedling_trans_rate_30pct
dev.off()







