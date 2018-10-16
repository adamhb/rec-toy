
#recruitment subroutine function

recruitment_fall_2018 <- function(pft = "early", a = 18, l, Z0 = 165, avg_l = 61){
  
  ifelse(pft == "early", yes = b <- b_param[1], no = b <- b_param[2])
  
  log10_n_rec <- log10(a) + b*log10(l/avg_l)
  
  n_rec <- 10^log10_n_rec
  
  C_rec <- n_rec * Z0
  
  out <- list(n_rec,C_rec)
  names(out) <- c("n_recs", "carbon_g")
  
  return(out)
}


#testing the recruitment subroutine


l <- seq(from = 6, to = 700, by = 1)

Rs.e <- recruitment_fall_2018(pft = "early", l = l, a = 7)
Rs.l <- recruitment_fall_2018(pft = "late", l = l, a = 20)

Ruger_bench_data <- data.frame(l = l, early = Rs.e[[1]], late = Rs.l[[1]])
Ruger_bench_data <- melt(data = Ruger_bench_data, id.vars = "l", variable.name = "pft") 

Ruger_bench_data %>% filter(l < 100) %>% ggplot(mapping = aes(x = l, y = value, color = pft)) + geom_line() +
  scale_y_continuous(name = "N _recruits_ha_6_months")+
  scale_x_continuous(name = "light")



#params
#a late: 14 per ha per 6 months under 2 % light
#a early: 7 per ha per 6 months under 2 % light
#l is in terms of MJ per months 
#avg. l = 58 MJ per 6 months (2% light)







