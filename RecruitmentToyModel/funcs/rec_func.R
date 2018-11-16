#recruitment subroutine function
#inputs: l = light in MJ per square meter at the forest floor over the prior 6 months
##average light at the forest floor over any 6 month period


#output
#provides the number of recruits per day

rec_func <- function(a_rec.x = a_rec[PFT], b_rec.x = b_rec[PFT], l, avg_l.x = avg_l, seedpool.x){
  
  log10_frac_rec <- log10(a_rec.x) + b_rec.x*log10(l/avg_l) 
  
  frac_rec <- (10^log10_frac_rec) #the fraction of the seedling pool recruiting per day
  
  C_rec <- frac_rec * seedpool.x
  
  N_rec <- C_rec / Z0
  
  out <- list(frac_rec,C_rec, N_rec)
  
  names(out) <- c("frac_rec", "C_rec", "N_rec")
  return(out)
}









