source('./data_transformation.R')

library('timeSeries')

total_dem <- timeSeries(data$SOMME, data$DATE)

tmax <- timeSeries(data$TMAX, data$DATE)

tmin <- timeSeries(data$TMIN, data$DATE)

tavg <- timeSeries(data$TAVG, data$DATE)

plot(tmax , ylab='max')
plot(tmin , ylab='min')
plot(tavg , ylab='avg')

plot(total_dem)



#function to plot a single time series over a given period of time
plot_period <- function(time_series , start , end , ylab) {
  sub_series <- window(time_series , start = start , end = end)
  plot(sub_series , ylab = ylab)
}


plot_period(tmax , start = '2015-01-01' , end = '2016-01-01' , 
            ylab='test')

years <- seq(as.Date('2012-01-01') , as.Date('2022-01-01') , "years")
years[11] <- '2021-12-30'

#year over year exploration
plot_year <- function(series , years_seq , label) {
  
  for ( i in seq(1,length(years_seq)-1)) {
    plot_period(series , years_seq[i] , years_seq[i+1] , ylab=label )
  }
}



#function to find the best tref
find_tref <- function(demand , temp , tref , type) {
  
  if (type == 'HDD'){
    dd = c()
    for (i in seq(1 , length(temp))) {
      dd[i] = max(tref - temp[i] , 0 )
    }
  } else {
    dd = c()
    for (i in seq(1 , length(temp))) {
      dd[i] = max(temp[i] - tref , 0)
    }
  }
  plot(dd , demand , main = paste('Demand against' , type) ,
       xlab = type ,
       ylab = "Daily electicity demand" ,
       pch = 19,
       frame = FALSE)
}

# Throught observation , the optimal tref should be  
# for HDD 
# t_min : [9.5 : 12] , t_max : [ 17 , 12] , med_t : [ 16 : 18]
# for CDD 
# tmin : [12 , 13] , t_max : [24 , 26] , med_t : [18.5 , 21]
# I found the best tref to be med_t for both CDD and HDD with 
# tref : HDD -> 16 and tref : CDD -> 21  


tmin_range <- seq(9.5 ,13 , 0.25)
tmax_range <- seq(17 ,26  , 0.25)
med_t <- (tmax + tmin)/2
med_t_range <- seq( 16,21 , 0.25)
tavg_range <- seq( 14 , 23 , 0.25)

for (j in c('HDD' , 'CDD')) {
  for (i in tmin_range){
    find_tref(data$SOMME , tmin , i , type=j)
    print(i)
  }
  
  print('DONE WITH TMIN')

  for (i in tmax_range){
    find_tref(data$SOMME , tmax , i , type=j)
    print(i)
  }
  print('DONE WITH TMAX')
  for (i in med_t_range){
    find_tref(data$SOMME , med_t , i , type=j)
    print(i)
  }
  print('DONE WITH med')
  
  for (i in tavg_range){
    find_tref(data$SOMME , tavg , i , type=j)
    print(i)
  }
  print('DONE WITH avg')
}

CDD <- c()
for (i in seq(1 , length(med_t))) {
  CDD[i] <- max(med_t[i] - 21 , 0 )
}

HDD <- c()
for (i in seq(1 , length(med_t))) {
  HDD[i] <- max(16 - med_t[i] , 0)
}

#results
plot(CDD , data$SOMME)
plot(HDD , data$SOMME)

data['CDD'] <- CDD
data['HDD'] <- HDD

#weekdays vs weekends
data['weekday'] <- weekdays(as.Date(data$DATE))

#binary for weekend 
weekend <- c()

for (i in seq(1, length(data$weekday))) {
  if (data$weekday[i] %in% c('Saturday' , 'Sunday')) {
    weekend[i] = 1
  } else {
    weekend[i] = 0
  }
}

data['weekend'] <- weekend


#months
data['month'] <- months(as.Date(data$DATE))

#effet de refroidissement
cp <- c()
temp_used <- med_t

for (i in seq(1 , length(data$DATE))) {
  if (temp_used[i] < 18.3) {
    cp[i] = round(((data$AWND[i])**(1/2)) * (18.3 - temp_used[i]) ,
                  digits = 4 )
                   
  } else {
    cp[i] = 0
  }
}

data['CP'] <- cp


#temperature effective we will use the mid_t to 
#stay consistent
tef <- c()
tef[1] <- med_t[1]

for (i in seq(2 , length(med_t))) {
  tef[i] <- (0.5 * med_t[i]) + (0.5 * med_t[i-1])
}

data['tef'] <- tef

#holidays
holidays <- read.csv('./data/holidays.csv' )
holidays_character <- c()

for (i in seq(1 , 180)) {
  holidays_character[i] <- as.character(holidays[i , ])
}

#needs fixing
is_holiday <- c()
for (i in seq(1 , length(data$DATE))) {
  if(data$DATE[i] %in% holidays_character) {
    is_holiday[i] <- 1
  } else {
    is_holiday[i] <- 0
  }
}

data['Holiday'] <- is_holiday

#saving master data_frame
save(data , file='master_df.Rdata')


