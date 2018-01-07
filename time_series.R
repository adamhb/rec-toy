#2) code to create the necessary time series for the model
month_day <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
daily_user_defined_water_def <- c()
for(i in 1:(length(user_defined_water_def))){
  temp <- rep(user_defined_water_def[i], month_day[i])
  daily_user_defined_water_def <- append(daily_user_defined_water_def, temp)
}

month_series <- rep(c(rep(1, 31*48), rep(2, 28*48), rep(3, 31*48), rep(4, 30*48), rep(5, 31*48), rep(6, 30*48), rep(7, 31*48), rep(8, 31*48), rep(9, 30*48), rep(10, 31*48), rep(11, 30*48), rep(12, 31*48)), years)