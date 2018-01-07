#recruitment toy model functions
#this excludes the major submodels like light vs. recruitment and light vs. mortality etc.

#SEEDLING BASAL DIAMETER TO BIOMASS (g C)
#using the allometric equations in Cole, T. G., & Ewel, J. J. (1995). Allometric equations for four valuable tropical tree species. Ecology, 229, 351–360. https://doi.org/10.1016/j.foreco.2006.04.017
#because they include equations specifically for individuals that are too small to have a dbh.
#using Cordia alliodora for all PFTs (sample for equations included height ranges at small as 10 cm tall.)

seedling_BD2biomass <- function(BD){
  C <- c()
  for(i in 1:4){
    C[i] <- cole_table$a[i]*(BD^(cole_table$b[i]))
  }
  return(sum(C)*1000)
}





