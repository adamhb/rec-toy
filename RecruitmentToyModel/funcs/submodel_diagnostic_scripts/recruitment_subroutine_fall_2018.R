
#recruitment subroutine function
#inputs: l = light in MJ at the forest floor over the prior 6 months
##average light at the forest floor over any 6 month period


#output
#provides the number of recruits per day

rec_func <- function(a_rec.x = a_rec[PFT], b_rec.x = b_rec[PFT], l, Z0 = 165, avg_l = 61){
  
  log10_n_rec <- log10(a_rec.x) + b_rec.x*log10(l/avg_l)
  
  n_rec <- (10^log10_n_rec) / (365/2)
  
  C_rec <- n_rec * Z0 / (365/2)
  
  out <- list(n_rec,C_rec)
  
  names(out) <- c("n_recs", "carbon_g")
  
  return(out)
}


rec_func(l = 81)$n_recs * 365


#testing the recruitment subroutine

l <- seq(from = 6, to = 700, by = 1)

Rs.e <- recruitment_fall_2018(pft = "early", l = l, a = 7)

Rs.l <- recruitment_fall_2018(pft = "late", l = l, a = 16)

Ruger_bench_data <- data.frame(l = l, early = Rs.e[[1]], late = Rs.l[[1]])
Ruger_bench_data <- melt(data = Ruger_bench_data, id.vars = "l", variable.name = "pft") 

Ruger_bench_data %>% filter(l < 100) %>% ggplot(mapping = aes(x = l, y = value, color = pft)) + geom_line() +
  scale_y_continuous(name = "N _recruits_ha_6_months")+
  scale_x_continuous(name = "light")

#params
#a late: 16 per ha per 6 months under 2 % light
#a early: 7 per ha per 6 months under 2 % light
#l is in terms of MJ per months 
#avg. l = 58 MJ per 6 months (2% light)












#scratch
#recruitment subroutine based on growth rates





#4 mm per year
4/365
#0.0011 cm per day

BD_delta_e




start <- rnbinom(n = 50000, mu = round(as.numeric(.5*1e5)), size = 1)/1e5
start <- start[start < BD_1_cm_e]

hist(start, breaks = 200)

stop <- start + 0.0010958
N_rec_e <- sum(stop >= BD_1_cm_e)
N_rec_e











seed_dyn$d[1:2000] %>% na.omit(.) %>% as.numeric(.) %>% filter(. != 0) %>% hist(.)
seed_dyn$h[1:2000] %>% na.omit(.) %>% as.numeric(.) %>% hist(.)



seed_dyn %>% filter(h < 1300)
names(seed_dyn)





