source('functions.R')

library('timeSeries')

#Timeseries objects needed
#la temperature : med_t = (tmin + tmax)/2
#la demand : total_dem (timeSeries object)

#timeSeries objects to use for plots

# temperature related
cdd_ts <- timeSeries( data$CDD , data$DATE)
hdd_ts <- timeSeries( data$HDD , data$DATE)
cp_ts <- timeSeries(data$CP , data$DATE)

# electricity demand
total_dem_ts <- timeSeries(data$SOMME , data$DATE)
north_ts <- timeSeries(data$NORTH , data$DATE)
nc_ts <- timeSeries(data$NCENT , data$DATE)
east_ts <- timeSeries(data$EAST , data$DATE)





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


dev.off(dev.cur())