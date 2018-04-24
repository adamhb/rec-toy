#statistical analyses

#A linear relationship between shade and the daily transition rate.
mod1 <- lm(daily_t_rate ~ shade, data = seed_dyn_c)
summary(mod1)

#doing the same as above but with relative irradiance (with varying conversion techniques) as the predictor variable
mod2 <- lm(log(daily_t_rate + 1) ~ log(RI3), data = seed_dyn_c)
summary(mod2)

plot(x = log(seed_dyn_c$RI3), y = log(seed_dyn_c$daily_t_rate))

#other potential relationships
mod3 <- lm(daily_t_rate ~ RI, data = seed_dyn_c)
summary(mod3)

mod4 <- lm(daily_t_rate ~ RI_quad, data = seed_dyn_c)
summary(mod4)

#plotting the RI3 variables as a predictor for daily transition rate
ggplot(data = seed_dyn_c, mapping = aes(x = RI3, y = daily_t_rate)) +
  geom_point()

# A logistic regression between shade and the probability of a recruit in any census interval. Very strong relationship. 
logistic_mod_shd <- glm(R ~ shd,family=binomial(link='logit'),data=seed_dyn)
summary(logistic_mod)

#A logistic regression between RI3 and the probability of a recruit in any census interval.
logistic_mod <- glm(R ~ RI3,family=binomial(link='logit'),data=seed_dyn)
summary(logistic_mod)

logistic_mod_pois <- glm(R ~ RI3,family=poisson(link='log'),data=seed_dyn)
summary(logistic_mod_pois)








#doing the above with PFTs

mod3_all <- lm(data = seed_dyn_c, log(daily_t_rate + 1) ~ log(PFD))
summary(mod3_all)

mod3_e <- lm(data = seed_dyn_c_e, log(daily_t_rate + 1) ~ log(PFD))
summary(mod3_e)

mod3_l <- lm(data = seed_dyn_c_l, log(daily_t_rate + 1) ~ log(PFD))
summary(mod3_l)


mod3_e2 <- lm(data = subset(seed_dyn_c_e, SPP > "M"), log(daily_t_rate + 1) ~ log(PFD))
summary(mod3_e2)

mod3_l2 <- lm(data = seed_dyn_c_l, log(daily_t_rate + 1) ~ log(PFD))
summary(mod3_l)




































#poissons with lots of variables




























