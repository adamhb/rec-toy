library(tidyverse)
library(magrittr)


load(bci.full)
load(spc)
load(sp_lhs_code)


#pfts_sept_2018 <- read.csv("pfts_9_20_2018.csv")



names()


PFT_data <- pfts_sept_2018[,c(2,4)]

data <- bci.full %>%
  select(bid,quadrat,date,sp,dbh,status) %>%
  filter(sp %in% PFT_data$sp) %>%
  merge(., PFT_data, by = "sp")

  
  quads <- unique(data$quadrat)
  
  recruitment_by_PFT <- data.frame()
  
  interval <- numeric(6 * length(quads) * length(PFT_data$sp))
  quadrat <- numeric(6 * length(quads) * length(PFT_data$sp))
  e_vs_l <- numeric(6 * length(quads) * length(PFT_data$sp))
  R_quad <- numeric(6 * length(quads) * length(PFT_data$sp))
  R_rate_quad <- numeric(6 * length(quads) * length(PFT_data$sp))
  
  rownumber <- 1
  mindbh <- 1


  
  
  for(q in quads){
    print(which(q==quads)/length(quads)*100)
    
    for(b in c(1,2,3,4,5,6)){
      
      for(pft in c("early","late")){
        
      
        group1 <- data %>% filter(e_vs_l==pft, quadrat==q, bid==b) 
        group2 <- data %>% filter(e_vs_l==pft, quadrat==q, bid==b+1)
        
        if(nrow(group1)==0 || nrow(group2)==0)
          next
        
        inc <- !is.na(group1$status) & !is.na(group2$status) & 
          group1$status != "M" & group2$status != "M" 
        
        group1 <- group1[inc, ]
        group2 <- group2[inc, ]
        
        time_f <- (group2$date - group1$date)/365.25
        
        survivor <- alive1 <- alive2 <- rep(FALSE, length(group1$status))
        alive1[group1$status == "A"] <- TRUE
        survivor[group1$status == "A" & group2$status == "A"] <- TRUE
        alive2[group2$status == "A"] <- TRUE
        S.inc <- survivor & group1$dbh >= mindbh
        N2.inc <- (alive2 & group2$dbh >= mindbh) | S.inc
        S <- length(group2$dbh[S.inc])
        N2 <- length(group2$dbh[N2.inc])
        timeint <- mean(time_f[N2.inc], na.rm = TRUE)
        
        interval[rownumber] <- b
        quadrat[rownumber] <- q
        e_vs_l[rownumber] <- pft
        R_quad[rownumber] <- N2 - S
        R_rate_quad[rownumber] <- ((N2-S)/timeint)/400
        
        rownumber <- rownumber + 1
      }
    }
  }
  
output_data <- data.frame(quad = quadrat, int = interval, pft = e_vs_l, R = R_quad, rec_rate = R_rate_quad)

#cleaning up the quadrat that doesn't appear to be real
output_data <- output_data[!output_data$quad == "",]  


#checking distribution of recruitment rates
output_data %>% filter(int == 2, pft == "early") %>% .$rec_rate %>% hist(., na.rm = T)
 
#checking distribution of recruitment rates 
output_data %>% filter(int == 2, pft == "late") %>% .$rec_rate %>% hist(., na.rm = T)


#adding quadrats to the condit data

canopyht5x5$quadX <- formatC(floor(canopyht5x5$x/20), width = 2, flag = 0)
canopyht5x5$quadY <- formatC(floor(canopyht5x5$y/20), width = 2, flag = 0)
canopyht5x5$quad <- paste0(canopyht5x5$quadX, canopyht5x5$quadY)

#averaging the shd indices to the level of the 20 m quadrat

#interval 2
shade_per_8590 <- canopyht5x5 %>% 
  group_by(quad) %>% 
  summarise(shd = mean(shade8589)) %>%
  mutate(int = 2) 

#shade_per_8590$RI <- exp(predict(l_shd_m_3.x, newdata = shade_per_8590))
summary(shade_per_8590)


#interval 3
canopyht5x5$shade_int3 <- rowMeans(canopyht5x5[,c(12,13, 14, 15)])

shade_per_int3 <- canopyht5x5 %>% 
  group_by(quad) %>% 
  summarise(shd = mean(shade_int3)) %>%
  mutate(int = 3)


#interval 4
canopyht5x5$shade_int4 <- rowMeans(canopyht5x5[,c(16,17)])

shade_per_int4 <- canopyht5x5 %>% 
  group_by(quad) %>% 
  summarise(shd = mean(shade_int4)) %>%
  mutate(int = 4)



#interval 6
shade_per_int6 <- canopyht5x5 %>% 
  group_by(quad) %>% 
  summarise(shd = mean(shade0509)) %>%
  mutate(int = 6)


#interval 5
canopyht5x5$shade_int5 <- rowMeans(canopyht5x5[,c(18,19)])
shade_per_int5 <- canopyht5x5 %>% 
  group_by(quad) %>% 
  summarise(shd = mean(shade_int5)) %>%
  mutate(int = 5)


shade_data <- rbind(shade_per_8590, shade_per_int3, shade_per_int4, shade_per_int5, shade_per_int6)


#adding the relative irradiance values for the first 3 intervals

#creating a linear conversion between wirth's quantiles and the index (pre 2003)
#shd_quantiles.x <- quantile(data_int_2$shade8589, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = FALSE, names = TRUE, type = 7)
shd_quantiles.int2_4 <- quantile(shade_data[shade_data$int < 5,]$shd, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = FALSE, names = TRUE, type = 7)

shd_quantiles2.x <- shd_quantiles^2
shd_quantiles3.x <- shd_quantiles^3
lnRI_quantiles.x <- c(-1.5, -3.1, -3.8, -4.1, -4.5, -5.2 , -6.1)

plot(shd_quantiles.int2_4, lnRI_quantiles.x)

l_shd_data.int2_4 <- data.frame(shd = shd_quantiles.int2_4, lnRI = lnRI_quantiles.x)


#looking at the relationship between the quantiles of shade and RI
l_shd_data.int2_4 %>% ggplot(mapping = aes(x = shd, y = lnRI)) + geom_point()
#creating a linear relationship 
l_shd_m_3.int2_4 <- lm(data = l_shd_data.int2_4, lnRI ~ shd)
summary(l_shd_m_3.x)

shade_data$RI <- rep(0, length(shade_data$quad))
shade_data[shade_data$int < 5,]$RI <- exp(predict(l_shd_m_3.int2_4, newdata = shade_data[shade_data$int < 5,]))






#adding the relative irradiance values for the last two intervals

shd_quantiles.int5_6 <- quantile(shade_data[shade_data$int > 4,]$shd, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1), na.rm = FALSE, names = TRUE, type = 7)
lnRI_quantiles.x <- c(-1.5, -3.1, -3.8, -4.1, -4.5, -5.2 , -6.1)

plot(shd_quantiles.int5_6, lnRI_quantiles.x)


l_shd_data.int5_6 <- data.frame(shd = shd_quantiles.int5_6, lnRI = lnRI_quantiles.x)

#looking at the relationship between the quantiles of shade and RI
l_shd_data.int5_6 %>% ggplot(mapping = aes(x = shd, y = lnRI)) + geom_point()
#creating a linear relationship 
l_shd_m_3.int5_6 <- lm(data = l_shd_data.int5_6, lnRI ~ shd)
summary(l_shd_m_3.int5_6)

#shade_data$RI <- rep(0, length(shade_data$quad))
shade_data[shade_data$int > 4,]$RI <- exp(predict(l_shd_m_3.int5_6, newdata = shade_data[shade_data$int > 4,]))


output_data2 <- merge(output_data, shade_data, by = c("quad", "int"))




#relationship between RI and recruitment
#RI predicts recruitment rates
output_data2 %>% filter(pft == "late") %>% ggplot(mapping = aes(x = RI, y = R))+
  geom_point()+
  scale_x_log10()#+
  #scale_y_log10()


#doing this across years might cause issues but there is a nice relationship within the 2nd interval
output_data2 %>% filter(pft == "early", int == 2) %>% ggplot(mapping = aes(x = RI, y = R))+
  geom_point()+
  scale_x_log10()
  #scale_y_log10()





#getting the solar load over each census interval in each quadrat

quad <- c()
int <- c()
start <- c()
stop <- c()

quads <- unique(data$quadrat)
rownumber <- 1
for(q in 1:length(quads)){
  for(i in c(2,3,4,5,6)){
    
    quad[rownumber] <- quads[q]
    int[rownumber] <- i
    
    start[rownumber] <- bci.full %>%
      select(bid,quadrat,date,sp,dbh,status) %>%
      filter(bid == i, quadrat == quads[q]) %>%
      summarise(mean_start = mean(date)) %>%
      .$mean_start
   
    stop[rownumber] <- bci.full %>%
      select(bid,quadrat,date,sp,dbh,status) %>%
      filter(bid == i+1, quadrat == quads[q]) %>%
      summarise(mean_stop = mean(date)) %>%
      .$mean_stop
    
  
    rownumber <- rownumber + 1
    print(rownumber/(length(quads)*5))
  }
}



quad_start_stop <- data.frame(quad = quad, int = int, start = start, stop = stop)

output_data_3 <- merge(output_data2, quad_start_stop, by = c("quad", "int"))

output_data_3$start_date <- as.Date(output_data_3$start, origin = as.Date("1960-1-1"))
output_data_3$stop_date <- as.Date(output_data_3$stop, origin = as.Date("1960-1-1"))




#first adding solar data for 1985
solar_bci$MJm2 %>% mean(.)

solar_bci_1985 <- data.frame(ndays = rep("null", 365), MJm2 = rep(15.87, 365), dates = seq.Date(from = as.Date("1985-01-01"), to = as.Date("1985-12-31"), by = 1)) 

solar_bci_fall_2018 <- rbind(solar_bci_1985, solar_bci)
                               
        

#creating function to do this
solar_calc_Joules <- function(data, start.date, stop.date){
  
  light_data <- data %>%
    filter(dates >= start.date & dates <= stop.date)
  
  light_val <- sum(light_data$MJm2)
  
  return(light_val)
}


#generating a vector of total solar load for each quadrat interval

five_yr_light_vals <- c()
for(i in 1:length(output_data_3$quad)){

strdate <- output_data_3$start_date[i]
stpdate <- output_data_3$stop_date[i]

five_yr_light_vals[i] <- solar_calc_Joules(data = solar_bci_fall_2018, start.date = strdate, stop.date = stpdate)

print(i/length(output_data_3$quad))
}


#solar_calc_Joules(data = solar_bci_fall_2018, start.date = "1985-10-05", stop.date = "1990-10-15")


#calculating the average number of Joules per 6 months over each quadrat interval 
output_data_4 <- cbind(output_data_3, five_yr_light_vals)

output_data_4 <- output_data_4 %>%
  mutate(light_MJ_per_6_mo_floor = five_yr_light_vals/10 * RI) %>%
  mutate(rec_rate_6_mo = rec_rate / 2)

output_data_4 %>% 
  filter(int == 2) %>%
  filter(light_MJ_per_6_mo_floor < 101) %>%
  filter(pft == "early") %>%
  ggplot(mapping = aes(x = light_MJ_per_6_mo_floor, y = rec_rate_6_mo * 10000)) + 
  geom_point()


#the above relationship looks good for census interval 2, but not the rest. Should check that the shade indices look ok for the other intervals 

light_mod_early <- output_data_4 %>% 
  filter(int == 2) %>%
  #filter(pft == "early") %>%
  lm(data = ., formula = rec_rate_6_mo ~ light_MJ_per_6_mo_floor + pft) 
summary(light_mod_early)



light_mod_fall_2018 <- output_data_4 %>% 
  filter(int == 2) %>%
  #filter(pft == "late") %>%
  lm(data = ., formula = rec_rate_6_mo ~ light_MJ_per_6_mo_floor + pft) 
summary(light_mod_late)


light_response_params_early <- coefficients(light_mod_early)
light_response_params_late <- coefficients(light_mod_late)


light_model_data <- 

early_predictionspredict(object = light_mod_early, newdata = output_data_4)


predict(object = light_mod_fall_2018, newdata = data.frame(light_MJ_per_6_mo_floor = 50, pft = "early")) * 500000



?abline















solar_calc_Joules

output_data_3 %>% filter()



head(quad_start_stop)
#adding the total number of Joules over the 2nd interval period





dates <- as.Date(solar_bci$ndays, origin = strptime(as.character("1980-1-1"), format = "%Y-%m-%d"), format = "%Y-%m-%d")
solar_bci$dates <- dates


names(output_data2)


















#scratch










#exploring the relationship between RI and recruitment

#there is a significant relationship between rec and shade
#shade X rec rate (per meter squared per year)
plot(data_int_2$shade8589, data_int_2$rec_rate)
#log (shade) X log (rec rate per m2 per year)
plot(log10(data_int_2$shade8589), log10(data_int_2$rec_rate))
plot(data_int_2$shade8589, data_int_2$R)
plot(data_int_2$RI, data_int_2$rec_rate)


shade_data$RI <- summary(exp(predict(l_shd_m_3.x, newdata = shade_data[shade_data$int == 2,])))



names(shade_per_int3)



merge(output_data,shade_per_int5, by = c("quad","int"))

output  
  
  
  


#looking at just the shade index for the 85-90 year of data
data_int_2 <- merge(output_data[output_data$int  == 2,], canopyht5x5[,c("quad", "shade8589")], by = "quad")



#adding relative irradiance to shade data


#using the above model to calculate RI in the data
data_int_2$RI <- exp(predict(l_shd_m_3.x, newdata = data.frame(x = 1:length(data_int_2$shade8589), shd = data_int_2[,"shade8589"])))


summary(data_int_2$RI)                                                      
                                                               
                                                              # shd2 = (data_int_2[,"shade8589"])^2, shd3 = (data_int_2[,"shade8589"])^3)))










#doing the above test case for all census intervals and using solar radiation data







#a poission regression with shade predicting 85-89 recruitment is highly significant
pois_mod <- glm(data = data_int_2, formula = R ~ shade8589, family = "poisson")
summary(pois_mod)

output_data %>% filter(pft == "early")







#end result
light_response_param_early
light_response_param_late













  
  
  

  
  

