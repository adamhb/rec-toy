
install.packages('ncdf4')
install.packages('ncdf.tools')

library(ncdf4)
library(ncdf.tools)


file1 <- nc_open(dir()[1])
file2 <- nc_open(dir()[2])

file3 <- nc_open(dir()[150])

SMP_file3 <- ncvar_get(file3, "SMP")
class(SMP_file3)

dim(SMP_file3)

SMP_file3[3,]/1e5


file4 <- nc_open(dir()[100])
SMP_file4 <- ncvar_get(file4, "SMP")
SMP_file4[3,]/1e5

length(dir())




for(i in vars){
print(length(ncvar_get(file2, varid = i)))
}

str(ncvar_get(file1,varid = "SMP"))


print(file1)
  
aggregateNcdf(fileName = c(dir()[1], dir()[2]), path.out = getwd(), period = 960)
transNcdfMerge(file.names = dir()[1:2], function(x) return(x))

print(file2)


vars <- c("ED_balive", "ED_biomass", "FSDS", "GPP", "H2OSOI", "NEP", "NPP", "SABG", "SMP", "SOILWATER_10CM", "TLAI")
files <- dir()


NPP <- c()
for(i in dir()){
  tmp <- ncvar_get(nc_open(i), varid = "NPP")
  NPP <- append(NPP, tmp)
}


FSDS <- c()
for(i in dir()){
  tmp <- ncvar_get(nc_open(i), varid = "FSDS")
  FSDS <- append(FSDS, tmp)
}

SMP <- c()
for(i in dir()[2]){
  tmp <- ncvar_get(nc_open(i), varid = "SMP")
  SMP <- append(SMP, tmp)
}


SABG <- c()
for(i in dir()){
  tmp <- ncvar_get(nc_open(i), varid = "SABG")
  SABG <- append(SABG, tmp)
}


H2OSOI <- c()
for(i in dir()){
  tmp <- ncvar_get(nc_open(i), varid = "H2OSOI")
  H2OSOI <- append(H2OSOI, tmp)
}


ED_biomass <- c()
for(i in dir()){
  tmp <- ncvar_get(nc_open(i), varid = "ED_biomass")
  ED_biomass <- append(ED_biomass, tmp)
}








FATES_data <- list()

for(v in vars){
  
var <- c()
for(i in dir()){
  tmp <- ncvar_get(nc_open(i), varid = v)
  var <- append(var, tmp)
}
browser()
FATES_data[[vars[v]]] <- var

}




FATES_data <- data.frame(t_step = 1:113881)
for(v in 1:length(vars)){
  var <- c()
for(f in 1:length(files)){
  tmp_data <- ncvar_get(nc_open(files[f]), varid = vars[v])
  var <- append(var,tmp_data)
}
cbind(FATES_data, var)
}




ncvar_get(nc = file1, varid = "DZSOI")


