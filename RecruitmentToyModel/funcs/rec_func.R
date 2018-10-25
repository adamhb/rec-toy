#recruitment subroutine function
#inputs: l = light in MJ at the forest floor over the prior 6 months
##average light at the forest floor over any 6 month period


#output
#provides the number of recruits per day

rec_func <- function(a_rec.x = a_rec[PFT], b_rec.x = b_rec[PFT], beta_rec.x = beta_rec[PFT], l, Z0 = 165, avg_l = 61, seedpool.x = seedpool[i]){
  
  
  if(transition_probs == T){
    a_rec.x <- beta_rec.x * (seedpool.x / Z0_seedling[PFT])
  }
  
  log10_n_rec <- log10(a_rec.x) + b_rec.x*log10(l/avg_l) #the amount of recruits per 6 months
  
  n_rec <- (10^log10_n_rec) / (365/2) #the number recruiting per day
  
  C_rec <- n_rec * Z0 #the amount of carbon getting pulled from the seedling pool per day
  
  out <- list(n_rec,C_rec)
  
  names(out) <- c("n_recs", "carbon_g")
  
  return(out)
}

