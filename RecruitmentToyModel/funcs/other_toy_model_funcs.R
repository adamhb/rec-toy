#recruitment toy model functions
#this excludes the major submodels like light vs. recruitment and light vs. mortality etc.

#SEEDLING BASAL DIAMETER TO BIOMASS (g C)
#using the allometric equations in Cole, T. G., & Ewel, J. J. (1995). Allometric equations for four valuable tropical tree species. Ecology, 229, 351–360. https://doi.org/10.1016/j.foreco.2006.04.017
#because they include equations specifically for individuals that are too small to have a dbh.
#using Cordia alliodora for all PFTs (sample for equations included height ranges at small as 10 cm tall.)






#the probability that an individual is of reproductive status as a function of dbh (mm)
prob_repro <- function(k = 0.0125, L = 1, size_mm, Dmax){
  y <- L / (1 + exp(-k*(size_mm - 0.5*Dmax)))
  return(y)
}


Chave.AGB=function(dbh,density=0.62,htparam=c(41.7,.057,.748),heightmodel=predht.asym,forest='moist'
) { 
  if(is.null(htparam)) 
  { 
    if(forest=="moist") param=c(-1.499,2.148,0.207,-0.0281) 
    else if(forest=="dry") param=c(-.667,1.784,0.207,-.0281) 
    else if(forest=="wet") param=c(-1.239,1.98,0.207,-0.0281) 
    
    AGB=agb.dbhmodel(dbh,density,param) 
  } 
  else 
  { 
    if(forest=="moist") param=c(.0501,1) 
    else if(forest=="dry") param=c(.112,.91) 
    else if(forest=="wet") param=c(.077,.94) 
    
    ht=heightmodel(dbh,htparam) 
    AGB=agb.model(dbh,density,ht,param) 
  } 
  
  return(AGB) 
} 


predht.asym=function(dbh,param) 
{ 
  if(is.null(dim(param))) 
  { 
    ymax=param[1] 
    a=param[2] 
    b=param[3] 
  } 
  else 
  { 
    ymax=param[,1] 
    a=param[,2] 
    b=param[,3] 
  } 
  
  return(ymax*(1-exp(-a*dbh^b))) 
} 


agb.model=function(dbh,density,height,param) 
  return(param[1]*(density*dbh^2*height)^param[2]) 

dbh2LAI <- function(dbh = 10){
  max_canopy_area <- 20 * 20
  slope <- (max_canopy_area - 1) / (2000 - 10) 
  LAI <- slope * dbh 
  return(LAI)
}  #converting the dbh to LAI in (m2)


########CONVERTING TOTAL BIOMASS TO DBH###########
#Using Chave function to creat the function that converts total biomass (g of C) to dbh (mm)

y <- seq(from = 1, to = 300, by = 0.25)
x <- Chave.AGB(dbh = y) * 1.2 * 0.4

plot(x,y)
plot(log(x),log(y))

log_growth_model <- lm(log(y)~log(x))


C2dbh <- function(carbon){
  carbon_kg <- carbon / 1000
  log_dbh <- log(carbon_kg)*coefficients(log_growth_model)[2] + coefficients(log_growth_model)[1]
  dbh <- exp(log_dbh) * 10
  return(dbh)
}


efrac <- function(N, co_dbh_ind, PFT){
  N_repro <- prob_repro(size_mm = co_dbh_ind, Dmax = Dmax[PFT]) * N
  fraction_reproductive <- N_repro / N
  e_frac <- fraction_reproductive * frac_repro[PFT] #frac repro for now is just a fixed percent of NPP (10%), need better data to get better numbers for this
  return(e_frac)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



def_func <- function(soil_moist, thresh.x = thresh.xx[PFT], window){
  thresh <- thresh.x
  def <- (abs(thresh) - abs(soil_moist))*-1
  no_def <- def < 0 
  def[no_def] <- 0
  deficit_days <- c()
  for(i in 1:length(def)){
    deficit_days[i] <- ifelse(i < window, sum(def[1:i]), sum(def[(i-window):i]))
  }
  return(deficit_days)
}





#set theme for the plots
adams_theme <- theme(plot.title = element_text(hjust = 0.5, size = 20),
                     strip.text.x = element_text(size = 18),
                     legend.title = element_blank (),
                     axis.title.x = element_text (size = 15), # change the axis title
                     axis.title.y = element_text (size = 15),
                     axis.text.x = element_text (size = 14, colour = "black"),
                     axis.text.y = element_text (size = 14, colour = "black"),
                     legend.text = element_text (size = 15))
year_axis <-  scale_x_date(breaks = date_breaks("2 years"), labels = date_format("%Y"))
smooth_line <- geom_smooth(size = 1.8, method = "loess", span = .01, se = F)
smoother_line <- geom_smooth(size = 1.8, method = "loess", span = .1, se = F)

