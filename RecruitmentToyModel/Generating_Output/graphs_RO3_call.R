#graphs for the R03 call


setwd("C:/Users/ahanb/OneDrive/Documents/RecruitmentToyModel/RecruitmentToyModel/graphs/For_RO3_call_7_13_2018")

rec_var <- expression(paste('N recruits'," ha"^"-1"," yr"^"-1"))


#Slide 5, Left
#graphing the recruitment rates with just the totals and comparing FATES current to the submodel
#need to add benchmarks here.
pft_total <- full_output %>% group_by(date) %>% summarise(total_R = sum(R)) %>% .$total_R

FATES_current_R <- FATES_current_data$R

graph1_data <- data.frame(date = full_output$date, submodel = pft_total, FATES_current = FATES_current_R)
graph1_data_melt <- melt(graph1_data, id.vars = "date", variable.name = "run")

fates_vs_submodel <- ggplot(data = graph1_data_melt, aes(x = as.Date(date), y = value*365, color = run)) +
  #geom_line(size = 1.8)+
  geom_smooth(size = 1.8, method = "loess", span = .01, se = F)+
  year_axis +
  ylab(expression(paste('N recruits'," ha"^"-1"," yr"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'Current FATES vs. New Submodel') +
  scale_color_manual(values = c("black", "red")) +
  geom_segment(aes(x=as.Date("2003-01-01"),xend=as.Date("2005-01-01"),y=92,yend=92), size = 1.8, color = "yellow")+
  geom_segment(aes(x=as.Date("2005-01-01"),xend=as.Date("2010-01-01"),y=190,yend=190), size = 1.8, color = "yellow2")+
  scale_linetype_manual(values = "yellow2")+
  theme_classic() +
  adams_theme #+
#geom_smooth(data = graph1_data_melt, 
#mapping = aes(x = as.Date(date), y = FATES_current*365),
#size = 1.8, method = "loess", span = .1, se = F)

#%>% filter(date > as.POSIXct("2005-01-01 11:30:00 UTC")

png(paste0("./Current_FATES_vs_NewSubModel.png"), height=5, width=8, units="in", res = 100)
fates_vs_submodel
dev.off()











#Slide 5, Right
#graphing the annual recruitment rate (as a instantaneous daily rate) of the submodel with the totals and all pfts
Rec_rate_submodel_s5R <- full_output %>% arrange(desc(pfts)) %>% ggplot( aes(x = as.Date(date), y = R*365, color = pfts)) +
  geom_smooth(size = 1.8, method = "loess", span = .05, se = F)+
  #smoother_line +
  year_axis +
  ylab(expression(paste('N recruits'," ha"^"-1"," yr"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'New Submodel') +
  geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), mapping = aes(x = as.Date(date), y = total_R*365, color = pft), size = 1.8, method = "loess", span = .1, se = F) +
  #geom_point(data = N_recs_per_year, mapping = aes(x = year, y = N_rec, color = "year_total"), size = 8) +
  geom_segment(aes(x=as.Date("2003-01-01"),xend=as.Date("2005-01-01"),y=92,yend=92), size = 1.8, color = "yellow")+
  geom_segment(aes(x=as.Date("2005-01-01"),xend=as.Date("2010-01-01"),y=190,yend=190), size = 1.8, color = "yellow")+
  scale_linetype_manual(values = "yellow")+
  scale_color_manual(values = c("springgreen","springgreen4","lightskyblue", "royalblue", "black", "yellow")) +
  theme(legend.title=element_blank()) +
  theme_classic() +
  adams_theme
  




#comparing FATES to toy model (slide 6)
#slide 6
#creating the data for the graphs
run_comparisons <- list()
run_comparisons[[run_name]] <- full_output %>% mutate(year = substring(text = as.character(date), first = 1, last = 4)) %>% group_by(pft,dpft,year) %>% summarise(N_rec = sum(R)) %>% summarise(avg_annual_n_rec = mean(N_rec)) %>% mutate(age = "young", light = "low", moisture = "dry")

run_comps <- rbind(run_comparisons[[1]], run_comparisons[[2]], run_comparisons[[3]], run_comparisons[[4]], run_comparisons[[5]], run_comparisons[[6]], run_comparisons[[7]], run_comparisons[[8]])


run_comps$light <- paste0(run_comps$light,"_","light")


run_comps$moisture <- plyr::mapvalues(x = run_comps$moisture, from = "reg", to = "amb.")


#mature cohort
Mature_Cohort <- ggplot(run_comps %>% mutate(PFT = as.factor(paste0(pft,dpft))) %>% filter(age == "mature"), aes(x = moisture, y = avg_annual_n_rec, fill = PFT)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ light)+
  scale_fill_manual(values = c("springgreen","springgreen4","lightskyblue", "midnightblue"))+ 
  labs(title = "Mature Cohort (50cm)")+
  theme (plot.title = element_text (hjust = 0.5, size = 20),
         legend.title = element_blank (),
         axis.title.x = element_text (size = 20), # change the axis title
         axis.title.y = element_text (size = 20),
         axis.text.x = element_text (size = 15, colour = "black"),
         axis.text.y = element_text (size = 15, colour = "black"),
         legend.text = element_text (size = 15))+
  xlab("")+
  ylab(rec_var)

png(paste0("Mature_Cohort.png"), height=5, width=8, units="in", res = 100)
Mature_Cohort
dev.off()
#young cohort
Young_Cohort <- ggplot(run_comps %>% mutate(PFT = as.factor(paste0(pft,dpft))) %>% filter(age == "young"), aes(x = moisture, y = avg_annual_n_rec, fill = PFT)) + 
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ light)+
  scale_fill_manual(values = c("springgreen","springgreen4","lightskyblue", "midnightblue"))+ 
  labs(title = "Young Cohort (3 cm)")+
  theme (plot.title = element_text (hjust = 0.5, size = 20),
         legend.title = element_blank (),
         axis.title.x = element_text (size = 20), # change the axis title
         axis.title.y = element_text (size = 20),
         axis.text.x = element_text (size = 15, colour = "black"),
         axis.text.y = element_text (size = 15, colour = "black"),
         legend.text = element_text (size = 15))+
  xlab("")+
  ylab(rec_var)
  


png(paste0("Young_Cohort.png"), height=5, width=8, units="in", res = 100)
Young_Cohort
dev.off()





#for extra slides
#annual recruitment rate of submodel with benchmark
#creating data of the number of recruits per year
N_recs_per_year <- full_output %>% mutate(year = substring(text = as.character(date), first = 1, last = 4)) %>% group_by(year) %>% summarise(N_rec = sum(R)) 

N_recs_per_year$year <- as.Date(paste0((as.numeric(N_recs_per_year$year)+1), "-01-01"))

Submodel_annual_rec_extra_slide <- ggplot() +
  #smoother_line +
  ylab(expression(paste('N recruits'," ha"^"-1"," yr"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'New Submodel') +
  #geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), mapping = aes(x = as.Date(date), y = total_R*365, color = pft), size = 1.8, method = "loess", span = .1, se = F) +
  geom_point(data = N_recs_per_year, mapping = aes(x = year, y = N_rec), size = 8) +
  geom_segment(aes(x=as.Date("2003-01-01"),xend=as.Date("2005-01-01"),y=41,yend=41), size = 1.8, color = "yellow")+
  geom_segment(aes(x=as.Date("2005-01-01"),xend=as.Date("2010-01-01"),y=50,yend=50), size = 1.8, color = "yellow2")+
  scale_linetype_manual(values = "yellow2")+
  theme_classic() +
  adams_theme +
  year_axis

Submodel_annual_rec_extra_slide
  #scale_linetype_manual("segment legend",values=c("segment legend"=2))



#for extra slides,m BY pft
#annual recruitment rate of submodel with benchmark
#creating data of the number of recruits per year
N_recs_per_year_pfts <- full_output %>% mutate(year = substring(text = as.character(date), first = 1, last = 4)) %>% group_by(year, pft) %>% summarise(N_rec = sum(R)) 

N_recs_per_year_pfts$year <- as.Date(paste0((as.numeric(N_recs_per_year_pfts$year)+1), "-01-01"))

Submodel_annual_rec_extra_slide <- ggplot() +
  #smoother_line +
  ylab(expression(paste('N recruits'," ha"^"-1"," yr"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'New Submodel') +
  #geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), mapping = aes(x = as.Date(date), y = total_R*365, color = pft), size = 1.8, method = "loess", span = .1, se = F) +
  geom_point(data = N_recs_per_year_pfts, mapping = aes(x = year, y = N_rec, color = pft), size = 8) +
  geom_segment(aes(x=as.Date("2003-01-01"),xend=as.Date("2005-01-01"),y=14,yend=14), size = 1.8, color = "green3")+
  geom_segment(aes(x=as.Date("2003-01-01"),xend=as.Date("2005-01-01"),y=28,yend=28), size = 1.8, color = "blue1")+
  geom_segment(aes(x=as.Date("2005-01-01"),xend=as.Date("2010-01-01"),y=15,yend=15), size = 1.8, color = "green3")+
  
  geom_segment(aes(x=as.Date("2005-01-01"),xend=as.Date("2010-01-01"),y=35,yend=35), size = 1.8, color = "blue1")+
  #scale_linetype_manual(values = "yellow2")+
  theme_classic() +
  adams_theme +
  year_axis +
  scale_color_manual(values = c("green3","blue1"))

Submodel_annual_rec_extra_slide
#scale_linetype_manual("segment legend",values=c("segment legend"=2))



















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
  scale_color_manual(values = c("springgreen2", "royalblue"))

png(paste0("light_dep_mort.png"), height=5, width=8, units="in", res = 100)
light_dep_mort
dev.off()





#graphing light-dep mort over time (3% light)
light_dep_mort_30pct <- ggplot(data = full_output, aes(x = as.Date(date), y = light_mort_rate, color = pft)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(bquote('mortality rate (% of seedling pool)'))+
  xlab(bquote('year'))+
  labs(title = 'light-dependent seedling mortality') +
  theme_classic() +
  adams_theme +
  scale_color_manual(values = c("springgreen2", "royalblue"))


png(paste0("light_dep_mort_30pct.png"), height=5, width=8, units="in", res = 100)
light_dep_mort_30pct
dev.off()


#add the 30% light one





















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







