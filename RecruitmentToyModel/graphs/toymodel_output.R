





 





#########sratch code chunks################


bquote('Assimilation ('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')')

ylab(bquote('dbh ('*ha^-1~yr^-1*')')) +
  
  geom_hline(yintercept = NPP_to_recruits(), linetype="dotted", size = 3)+ #the number of recruits that the current FATES model predicts in 1 ha given total NPP of 8300 kg / ha 
  #annotate("text", 4, NPP_to_recruits()+7, label = "FATES (all PFTs)", size = 4, angle = 0) +
  theme(legend.position = c(0.15, 0.78),
        axis.title.x = element_text (size = 20), 
        axis.title.y = element_text (size = 20),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))+
  geom_point(x = 2, y = 106, size = 3)+ #decadal mean observed number of recs per ha per year on bci
  #annotate("text", 5.2, 170, label = "<-- decadal   mean   BCI", size = 5, angle = 45)#+
  geom_line(aes(light, total, color = "total"), total_R, size = 1.8)
?png



##############end sratch code#########################

toy_model_output <- toy_model(cmc_ind_0 = 1e7, 
                              pct_light_seedling_layer = 0.80, 
                              seedbank_0 = 2000, seedpool_0 = 10000, 
                              years = 5, 
                              frac_NPP2GPP = 0.5, 
                              frac_NPP2growth = 0.15)



#graphing cohort dbh
p1 <- ggplot(data = toy_model_output[[2]], aes(x = t_step/48, y = co_dbh_ind)) +
  geom_line(size = 1.8)+
  scale_x_continuous(name = "day")+
  ylab(bquote('dbh (mm)'))+
  theme(axis.title.x = element_text (size = 20), 
      axis.title.y = element_text (size = 20),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15))
#graphing carbon allocated to reproduction each month 
p2 <- ggplot(data = toy_model_output[[1]], aes(x = seq(1:length(toy_model_output[[1]]$month)), y = c4_repro_mo)) +
  geom_line(size = 1.8)+
  scale_x_continuous(name = "month") +
  ylab(bquote('carbon for reproduction (g)')) +
  theme(axis.title.x = element_text (size = 20), 
      axis.title.y = element_text (size = 20),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15))
#graphing the cohort LAI over time
p3 <- ggplot(data = toy_model_output[[2]], aes(x = t_step/48, y = co_LAI_ind)) +
  geom_line(size = 1.8)+
  scale_x_continuous(name = "day") +
  ylab(bquote('cohort LAI ' *m^-2))+
  theme(axis.title.x = element_text (size = 20), 
        axis.title.y = element_text (size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
#graphing the number of individuals per cohort
p4 <- ggplot(data = toy_model_output[[2]], aes(x = t_step/48, y = N_co)) +
  geom_line(size = 1.8)+
  scale_x_continuous(name = "day") +
  ylab(bquote('N_cohort'))+
  theme(axis.title.x = element_text (size = 20), 
        axis.title.y = element_text (size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
#graphing seedbank size
p5 <- ggplot(data = toy_model_output[[3]], aes(x = month, y = seedbank)) +
  geom_line(size = 1.8)+
  scale_x_continuous(name = "month") +
  ylab(bquote('seedbank size (gC)'))+
  theme(axis.title.x = element_text (size = 20), 
        axis.title.y = element_text (size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
#graphing seedpool size
p6 <- ggplot(data = toy_model_output[[3]], aes(x = month, y = seedpool)) +
  geom_line(size = 1.8)+
  scale_x_continuous(name = "month") +
  ylab(bquote('seedpool size (gC)'))+
  theme(axis.title.x = element_text (size = 20), 
        axis.title.y = element_text (size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
#graphing the number of new recruits into the 1 cm size class
p7 <- ggplot(data = toy_model_output[[3]], aes(x = month, y = R)) +
  geom_line(size = 1.8)+
  scale_x_continuous(name = "month") +
  ylab(bquote('rec rate ('*'ind.ha' ^-1~month^-1*')'))+
  theme(axis.title.x = element_text (size = 20), 
        axis.title.y = element_text (size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
#graphing N  
p8 <- ggplot(data = toy_model_output[[3]], aes(x = month, y = N)) +
  geom_line(size = 1.8)+
  scale_x_continuous(name = "month") +
  ylab(bquote('N ('*'ind.ha'*')'))+
  theme(axis.title.x = element_text (size = 20), 
        axis.title.y = element_text (size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))



num_plots <- 8

plots <- list()

plots[[1]] <- p1
plots[[2]] <- p3
plots[[3]] <- p4
plots[[4]] <- p2


plots[[5]] <- p5
plots[[6]] <- p6
plots[[7]] <- p7
plots[[8]] <- p8


#plots[[9]] <- p9


png("p1_run5_gap.png", height=8.5, width=11, units="in", res=300)
multiplot(plotlist = plots[1:4], cols = 2)
dev.off()

png("p2_run5_gap.png", height=8.5, width=11, units="in", res=300)
multiplot(plotlist = plots[5:8], cols = 2)
dev.off()



run4_66cm_2pctlight_15yrs <- toy_model_output






