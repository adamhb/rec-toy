emerg_func <- function(a = a_emerg[PFT], b = b_emerg[PFT], precip = 71, avg_precip.x = avg_precip, seedbank.x){
  
  log10_frac_emerg <- log10(a) + b*log10(precip/avg_precip.x) 
  
  frac_emerg <- 10^log10_frac_emerg 
  
  C_emerg <- frac_emerg * seedbank.x
  
  out <- list(frac_emerg, C_emerg)
  names(out) <- c("frac_emerg", "C_emerg")
  return(out)
}



