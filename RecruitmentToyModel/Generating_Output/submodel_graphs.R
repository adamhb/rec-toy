#submodel graphs

###graph for H20 mort###
H20_mort_graph_data <- data.frame(x = rep(1:200,2), y = c(H20_mort(deficit_days = 1:200, PFT = "DI"), H20_mort(deficit_days = 1:200, PFT = "DT")), PFT = c(rep("DI", 200), rep("DT", 200)))

ggplot(data = H20_mort_graph_data, aes(x = x, y = y, color = PFT)) + 
  geom_point() + 
  xlab(expression(paste("water deficit days")))+
  ylab(expression("daily mort. rate"))+
  labs(title = "")+
  theme(
    axis.text.y.right = element_text (color = "black"),
    axis.title.y.right = element_text (size = 15, colour = "black"),
    plot.title = element_text (hjust = 0.5, size = 25),
    axis.title.x = element_text (size = 20), 
    axis.title.y = element_text (size = 15, colour = "green4"),
    axis.text.x = element_text (size = 15, colour = "black"),
    axis.text.y = element_text (size = 15, colour = "green4"))#+
  #scale_y_continuous(sec.axis = sec_axis(~./7, name = "DT daily mort. rate"))+
  #geom_line(data = predicted_DT, aes(y = y*7, x = x)) +
  #geom_point(data = ts_wk_DT, aes(x = deficit_days, y = mort*7), size = 3)












####GRAPHING THE RELATIONSHIP BETWEEN PERCENT LIGHT (PERCENT OF TOP OF CANOPY) AND DAILY RECRUITMENT RATES FOR THE frac_rec_Kobe function#####

#for the plotting the relationship between light and daily recruitment rate for both PFTs
light_range <- 1:100
rec_range <- c()
for(i in 1:100){
  rec_range[i] <- frac_rec(light = light_range[i], t_window = 2, N_smp = 100, PFT = "early")
}

rec_range_late <- c()
for(i in 1:100) {
  rec_range_late[i] <- frac_rec(light = light_range[i], t_window = 2, N_smp = 100, PFT = "late")
}

light_vs_rec <- data.frame(light = light_range, early = rec_range, late = rec_range_late)

melt_lvrec <- melt(data = light_vs_rec, measure.vars = c("early", "late"), value.name = "value")

names(melt_lvrec)[2] <- "PFT"

ggplot(data = melt_lvrec, aes(x = light, y = value, color = PFT)) + geom_point(size=3) +
  ylab("daily recruitment rate") + 
  xlab("percent light") +
  labs(title = "Light and Recruitment") +
  scale_x_continuous(sec.axis = sec_axis(~./100 * 1115, name = expression(paste("mean photon flux density (", mu, "mol ",m^-2,s^-1,")")))) + 
  theme (plot.title = element_text (hjust = 0.5, size = 20),
         #strip.text.x = element_text(size = 18),
         #legend.title = element_blank (),
         axis.title.x = element_text (size = 15), # change the axis title
         axis.title.y = element_text (size = 15),
         axis.text.x = element_text (size = 15, colour = "black"),
         axis.text.y = element_text (size = 15, colour = "black"),
         legend.text = element_text (size = 15),
         legend.title = element_text (size = 15))










#########THE RELATIONSHIP BETWEEN LIGHT AND MORTALITY############### 
#this is done for early and late PFTs plus another optional relationship based off of Pourouma: 


#creating the data for the Pourouma relationship (i.e. higher light starts to kill the seedlings)
l_range <- 1:100
morts_Pour <- c()
for(i in 1:100){
  morts_Pour[i] <- light_mort_Pourouma(light = l_range[i])
}

#creating a plot of the relationship between light and mortality for the early and late PFTs
l_range <- 1:100
morts_e <- c()
for(i in 1:100){
  morts_e[i] <- light_mort(light = l_range[i])
}

morts_l <- c()
for(i in 1:100){
  morts_l[i] <- light_mort(light = l_range[i], PFT = "late")
}

morts_df <- data.frame(l = l_range, early = morts_e, late = morts_l, late_2nd_optn. = morts_Pour)

morts_df <- melt(morts_df, measure.vars = c("early", "late", "late_2nd_optn."), variable.name = "PFT")

ggplot(data = morts_df, aes(x = l, y = value, color = PFT)) + geom_point() +
  ylab("mortality rate (daily)") + 
  scale_y_continuous(sec.axis = sec_axis(~.*365, name = "mortality rate (annual)"))+
  xlab("percent light") +
  labs(title = "Light and Mortality") +
  theme (plot.title = element_text (hjust = 0.5, size = 20),
         #strip.text.x = element_text(size = 18),
         #legend.title = element_blank (),
         axis.title.x = element_text (size = 15), # change the axis title
         axis.title.y = element_text (size = 15),
         axis.text.x = element_text (size = 15, colour = "black"),
         axis.text.y = element_text (size = 15, colour = "black"),
         legend.text = element_text (size = 15),
         legend.title = element_text (size = 15))







  
  