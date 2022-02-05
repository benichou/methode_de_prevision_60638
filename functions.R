source('exploratory_vars.R')
library('forecast')

#la temperature : med_t = (tmin + tmax)/2
#la demand : total_dem (timeSeries object)

#other important timeSeries objects to use for plots
cdd_ts <- timeSeries( data$CDD , data$DATE)
hdd_ts <- timeSeries( data$HDD , data$DATE)
cp_ts <- timeSeries(data$CP , data$DATE)

#Satterplot de deux series chrono en fonction d'une start date
# et d'une end date
scatter_period <- function(tsx , tsy, start, end ,
                             xlab , ylab , main) {
  
  #les deux series chronos doivent etre des timeSeries object pour
  #que ca fonctionne. Utilise med_t et total dem
  ts1_wind = window(tsx , start = start , end = end)
  ts2_wind = window(tsy , start =start , end = end)
  
  df <- data.frame(ts1_wind$TS.1 , ts2_wind$TS.1)
  
  plot(x = df$ts1_wind , y = df$ts2_wind , main = main,
       xlab = xlab ,
       ylab = ylab ,
       pch = 19,
       frame = FALSE)
}

#Boxplot d'une serie chrono par groupe pour une periode donnee
boxplot_group_time <- function(ts , grp , start , end , ylab,
                               xlab , main , data ) {
  
  filter <- which(data$DATE >= start & data$DATE <= end)
  
  if (grp == 'weekday' || grp == 'month') {
    group <- factor(data[filter , grp] , 
                    levels = unique(data[filter , grp]))
  } else {
    group <- data[filter , grp]
  }
  
  boxplot( data[filter ,ts] ~ group,
           xlab = xlab ,
           ylab = ylab ,
           main = main)
}