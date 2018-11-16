emerg_func_old <- function(a = a_emerg[PFT], b = b_emerg[PFT], precip = 71, avg_precip.x = avg_precip, seedbank.x){
  
  log10_frac_emerg <- log10(a) + b*log10(precip/avg_precip.x) 
  
  frac_emerg <- 10^log10_frac_emerg 
  
  C_emerg <- frac_emerg * seedbank.x
  
  out <- list(frac_emerg, C_emerg)
  names(out) <- c("frac_emerg", "C_emerg")
  return(out)
}
emerg_func(seedbank.x = 10000)






emerg_func <- function(a = a_emerg[PFT], b = b_emerg[PFT], SMP.x = avg_SMP, avg_SMP.x = avg_SMP, seedbank.x){
  
  log10_frac_emerg <- log10(a) + b*log10(abs(avg_SMP.x)/abs(SMP.x)) 
  
  frac_emerg <- 10^log10_frac_emerg 
  
  C_emerg <- frac_emerg * seedbank.x
  
  out <- list(frac_emerg, C_emerg)
  names(out) <- c("frac_emerg", "C_emerg")
  return(out)
}


