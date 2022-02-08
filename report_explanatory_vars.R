source('functions.R')

library('timeSeries')

#Timeseries objects needed
#la temperature : med_t = (tmin + tmax)/2
#la demand : total_dem (timeSeries object)

#timeSeries objects to use for plots

cdd_ts <- timeSeries( data$CDD , data$DATE)
hdd_ts <- timeSeries( data$HDD , data$DATE)
cp_ts <- timeSeries(data$CP , data$DATE)
total_dem_ts <- timeSeries(data$SOMME , data$DATE)
north_ts <- timeSeries(data$NORTH , data$DATE)
nc_ts <- timeSeries(data$NCENT , data$DATE)
east_ts <- timeSeries(data$EAST , data$DATE)
humidity <- timeSeries(data$RELATIVE_HUM_PERCENT , data$DATE)
precipitation <- timeSeries(data$PRCP , data$DATE)
snowfall <-timeSeries(data$SNOW , data$DATE)
snowdepth <- timeSeries(data$SNWD , data$DATE)
wind_d2 <- timeSeries(data$WDF2 , data$DATE)
wind_s2 <- timeSeries(data$WSF2 , data$DATE)
wind_chill <- timeSeries(data$CP , data$DATE)
tef <- timeSeries(data$tef , data$DATE)

pdf("Explanatory_variables.pdf")

# plotting every year seperately
plot_period(total_dem_ts , start = '2012-01-01' ,
            end = '2021-12-31' ,
            ylab='Total energy demand',
            main='Total electricity demand for N , NC and E')

# years vector
years <- seq(as.Date('2012-01-01') , as.Date('2022-01-01') , "years")
years[11] <- '2021-12-30'

plot_year(total_dem_ts , years_seq = years , 
          'Daily Electricity Demand', main ="North ,North Central and
          East Daily's Electricity demand for the year")

plot_year(north_ts , years_seq = years , 
          'Daily Electricity Demand' ,
          main ="North Daily's Electricity demand for the year")

plot_year(nc_ts , years_seq = years , 
          'Daily Electricity Demand' ,
          main ="North Central 
          Daily's Electricity demand for the year")

plot_year(east_ts , years_seq = years , 
          'Daily Electricity Demand' ,
          main ="East Daily's Electricity Demand for the Year")

#Looking at the distribution of the
# total demand by month for all the years
year_b <- as.character(seq(2012 , 2021 , 1))

for (year in year_b) {
  boxplot_group_time('SOMME', 
                     'month' , 
                     start = paste(year ,'-01-01', sep=''),
                     end = paste(year, '-12-31',sep=''), 
                     ylab='Enregy Demand for the three regions' ,
                     xlab = paste('Year =',year) , 
                     main='Total Enegry demand for N , NC , E',
                     data = data)
}

for (year in year_b) {
  boxplot_group_time('NORTH', 
                     'month' , 
                     start = paste(year ,'-01-01', sep=''),
                     end = paste(year, '-12-31',sep=''), 
                     ylab='Enregy Demand for NORTH' ,
                     xlab = paste('Year =',year) , 
                     main='Total Enegry demand for NORTH',
                     data = data)
}

for (year in year_b) {
  boxplot_group_time('NCENT', 
                     'month' , 
                     start = paste(year ,'-01-01', sep=''),
                     end = paste(year, '-12-31',sep=''), 
                     ylab='Enregy Demand for NCENT' ,
                     xlab = paste('Year =',year) , 
                     main='Total Enegry demand for NCENT',
                     data = data)
}

for (year in year_b) {
  boxplot_group_time('EAST', 
                     'month' , 
                     start = paste(year ,'-01-01', sep=''),
                     end = paste(year, '-12-31',sep=''), 
                     ylab='Enregy Demand for EAST' ,
                     xlab = paste('Year =',year) , 
                     main='Total Enegry demand for EAST',
                     data = data)
}


#looking at the demand by day of the week for different periods

boxplot_group_time('SOMME',
                   'weekday' , 
                    start = '2012-01-01',
                    end = '2021-12-31', 
                    ylab='Total energy deman' ,
                    xlab = 'Day' , 
                    main= 'Energy demand by weekday',
                    data = data )

#enegry demand by week day for different months
months_data <- unique(data$month)

for (month in months_data) {
  boxplot_group_time('SOMME' ,
                     'weekday',
                     start = '2012-01-01' ,
                     end = '2021-12-31' ,
                     ylab = 'Total energy demand' ,
                     xlab = 'Day' ,
                     main = paste('Energy demand by weekday for the 
                                  month of' , month),
                     data = data ,
                     add_filter = (data$month == month))
}

# Looking at holiday vs non holiday
boxplot_group_time('SOMME',
                   'Holiday' , 
                   start = '2012-01-01',
                   end = '2021-12-31', 
                   ylab='Total energy demand' ,
                   xlab = 'Holiday' , 
                   main= 'Distribution of the demand',
                   data = data )


# Looking at weekday vs weekend
boxplot_group_time('SOMME',
                   'weekend' , 
                   start = '2012-01-01',
                   end = '2021-12-31', 
                   ylab='Total energy demand' ,
                   xlab = 'Weekend' , 
                   main= 'Distribution of the demand',
                   data = data )

boxplot_group_time('SOMME',
                   'type_of_day' , 
                   start = '2012-01-01',
                   end = '2021-12-31', 
                   ylab='Total energy demand' ,
                   xlab = 'Type of day' , 
                   main= 'Distribution of the demand',
                   data = data )

#Looking at the temperature by month

for (year in year_b) {
  boxplot_group_time('med_t', 
                     'month' , 
                     start = paste(year ,'-01-01', sep=''),
                     end = paste(year, '-12-31',sep=''), 
                     ylab='(Max temp + Min temp)/ 2' ,
                     xlab = paste('Year =',year) , 
                     main='Estimated daily temperature',
                     data = data)
}

#scatter plot temperature
scatter_period(tsx = temp_used , tsy= total_dem_ts , 
               start = '2012-01-01' ,  end = '2021-12-31' ,
               xlab = 'Estimated temperature in celsius' ,
               ylab = 'Total daily demand',
               main = 'Total daily demand against median temperature
               2012-2021')

#CDD and HDD
for (i in seq(20 , 14 , -1)) {
  find_tref(data$SOMME , data$med_t , i , type ='HDD')
}

for (i in seq(14 , 23, 1)) {
  find_tref(data$SOMME , data$med_t , i , type ='CDD')
}

#Humidity
scatter_period(humidity , total_dem_ts , start ='2012-01-01' ,
               end='2021-12-31' , xlab='Humidity' , 
               ylab='Daily demand' , main='Demand against Humidity')

#Demand vs humidity in different seasons
summer <- c('June' , 'July' , 'August' , 'September')
filter <- which(data$month %in% summer)
demand_summer <- data[filter , 'SOMME']
humidity_summer <- data[filter , 'RELATIVE_HUM_PERCENT']
plot(x =humidity_summer , y = demand_summer , 
     main = "Demand against Humidity during summer months",
     xlab = 'Humidity' ,
     ylab = 'Daily demand' ,
     pch = 19,
     frame = FALSE)

#Demand vs percipitation -To remove
scatter_period(precipitation , total_dem_ts , start ='2012-01-01' ,
               end='2021-12-31' , xlab='Precipitation' , 
               ylab='Daily demand',
               main='Demand against Precipitation')

#Demand vs snowfall -To remove
scatter_period(snowfall , total_dem_ts , start='2012-01-01' ,
               end = '2021-12-31' ,
               xlab = 'Snowfall in mm' ,
               ylab = 'Daily Electricity Demand',
               main = 'Demand against snowfall')

#Demand vs snowdepth -To remove
scatter_period(snowdepth , total_dem_ts , start='2012-01-01' ,
               end = '2021-12-31' ,
               xlab = 'Snowdepth in mm' ,
               ylab = 'Daily Electricity Demand',
               main = 'Demand against snowdepth')

#Demand vs wind direction -To remove
scatter_period(wind_d2 , total_dem_ts , start='2012-01-01' ,
               end = '2021-12-31' ,
               xlab = 'Fastest 2 min wind direction' ,
               ylab = 'Daily Electricity Demand',
               main = 'Electricity demand against wind direction')

#Demand vs wind speed -To remove
scatter_period(wind_s2 , total_dem_ts , start='2012-01-01' ,
               end = '2021-12-31' ,
               xlab = 'Fastest 2 min wind speed' ,
               ylab = 'Daily Electricity Demand',
               main = 'Electricity demand against wind speed')

#Demand distribution by Fog
boxplot_group_time('SOMME',
                   'WT01' , 
                   start = '2012-01-01',
                   end = '2021-12-31', 
                   ylab='Total energy demand' ,
                   xlab = 'Fog' , 
                   main= 'Distribution of the demand',
                   data = data )

#Demand distribution by heavy fog
boxplot_group_time('SOMME',
                   'WT02' , 
                   start = '2012-01-01',
                   end = '2021-12-31', 
                   ylab='Total energy demand' ,
                   xlab = 'Heavy fog' , 
                   main= 'Distribution of the demand',
                   data = data )

#Deman distribution by thunder
boxplot_group_time('SOMME',
                   'WT03' , 
                   start = '2012-01-01',
                   end = '2021-12-31', 
                   ylab='Total energy demand' ,
                   xlab = 'Heavy fog' , 
                   main= 'Distribution of the demand',
                   data = data )



#Demand distribution by smoke or haze
boxplot_group_time('SOMME',
                   'WT08' , 
                   start = '2012-01-01',
                   end = '2021-12-31', 
                   ylab='Total energy demand' ,
                   xlab = 'Smoke or haze' , 
                   main= 'Distribution of the demand',
                   data = data )

#Demand  vs wind chill
scatter_period(wind_chill , total_dem_ts , start='2012-01-01' ,
               end = '2021-12-31' ,
               xlab = 'Wind chill' ,
               ylab = 'Daily Electricity Demand',
               main = 'Electricity demand against wind chill')

#Demand vs effective temp
scatter_period(tef , total_dem_ts , start='2012-01-01' ,
               end = '2021-12-31' ,
               xlab = 'Effective temperature' ,
               ylab = 'Daily Electricity Demand',
               main = 'Electricity demand against effective temp')

# WT04 , WT05, WT06 , WT07 , WT09 , WT11 , WT13 , 
# WT14 , WT16, WT18 not enough observations or already captured
useless_vars <- c('WT04' , 'WT05' , 'WT06' , 'WT07' , 'WT09' ,
                  'WT11' , 'WT13' , 'WT14' , 'WT16' , 'WT18' ,
                  'TMIN' , 'TOBS' , 'TAVG' , 'WDF2' , 'WDF5' ,
                  'WSF2' , 'WSF5')


dev.off(dev.cur())