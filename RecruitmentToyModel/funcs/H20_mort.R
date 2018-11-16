H20_mort <- function(deficit_days, pft.x){
  PFT <- pft.x
  mort_rate <- deficit_days * P1H20[PFT] + P2H20[PFT]
  return(mort_rate/(window.x))
}


H20_mort(deficit_days = 1e7, pft.x = "earlydi")
