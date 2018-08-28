#the original growth parameter values for Kobe 1999
#note that the standard deviations of each parameter were calculated from the 95% bootstraped support limits in Table 3 of Kobe 1999. The 95% support limits were assumed to represent 2 standard deviations away from the parameter estimate. 

p1e <- 0.7618
p1e_sd <- (0.9315 - p1e)/2

p1e_se <- p1e_sd/p1e

p1l <- 0.2211
p1l_sd <- (0.3483 - p1l) / 2

p1l_se <- p1l_sd / p1l


p2e <- 0.0039
p2e_sd <- 0.0051 - p2e

p2e_se <- p2e_sd/p2e

p2l <- 0.0145
p2l_sd <- 0.0278 - p2l

p2l_se <- p2l_sd/p2l