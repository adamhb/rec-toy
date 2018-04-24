########## EMERGENCE ##############

#CREATING THE EMERGENCE FUNCTION

#range of emergence into seedling pool is 0-2.8 individuals per square meter per month, source(Metz et al. 2008, Fig 3)
#assume each seedling is 2.5 g C when it enters the seedling pool
#this means that there is a range of 0-7 g of carbon per square meter per month entering the seedling pool.
#The average seedbank size in the toy model for a mature cohort is 11 g C per square meter
#Therefore, we can estimate the fraction of carbon leaving the seedbank each month as 0 - 0.63
#I will force the model to emerge at least 0.1 of the seedbank each month
#thus the range is 0.1-0.63 percent of the seedbank each month

#the range of monthly precipitation seen across these sites is 32 to 426 mm
rain <- seq(32,426,by = 1)

rain <- rain/4 #converting to weekly rainfall (mm)

frac_emerg_data <- seq(0.1,0.63, length = length(rain)) #spectrum of possible monthly emergence rates according to Metz's 2008  paper.
frac_emerg_data <- frac_emerg_data/30.4 #fraction emerging each day

plot(rain, frac_emerg_data, xlab = "prior week precip (mm)", ylab = "fraction of seedbank emerging per day")

frac_emerg_lm <- lm(frac_emerg_data~rain)

#EMERGENCE FUNCTION#
#input: the sum of the prior week's precipitation
frac_emerg_func <- function(rain){
  frac_emerg <- ((coefficients(frac_emerg_lm)[2])/20)*rain + coefficients(frac_emerg_lm)[1]
  return(frac_emerg)
}
frac_emerg_func(60)
#END EMERGENCE FUNCTION














