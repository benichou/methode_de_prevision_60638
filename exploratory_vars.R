#
# Program: exploratory_vars.R
#
# Purpose: Exploratory analysis of the explanatory variables
# searching for variables that have the most linear relationship
# with target 
# 
# 
# Written by: Team G, January 30 2022
#
# Updated: Feb 10th 2022
#          
#         
#          
#         
#          
# ------------------------------------------------------



source('./data_transformation.R')

# library('timeSeries')


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
  
  plot(dd , demand , main = paste( paste('Demand against', 'HDD'),
                                   paste('tref =',
                                    as.character(tref))),
       xlab = type ,
       ylab = "Daily electicity demand" ,
       pch = 19,
       frame = FALSE)
}

# creating timeseries for weather
tmax <- timeSeries(data$TMAX , data$DATE)
tmin <- timeSeries(data$TMIN , data$DATE)
med_t <- (tmax + tmin)/2
tavg <- timeSeries(data$TAVG , data$DATE)
tobs <- timeSeries(data$TOBS , data$DATE)

# first run of optimization ! 
# tmin_range <- seq(round(min(tmin)) ,round(max(tmin)) , 0.25)
# tmax_range <- seq(round(min(tmax)) ,round(max(tmax)) , 0.25)
# med_t_range <- seq(round(min(med_t)) ,round(max(med_t)) , 0.25)
# tavg_range <- seq(round(min(tavg)) ,round(max(tavg)) , 0.25)
# tobs_range <- seq(round(min(tobs)) ,round(max(tobs)) , 0.25)


# second run of optimization ! 
# Throught observation , the optimal tref should be  
# for HDD 
# t_min : [9.5 : 12] , t_max : [ 17 , 12] , med_t : [ 16 : 18]
# for CDD 
# tmin : [12 , 13] , t_max : [24 , 26] , med_t : [18.5 , 21]
# tobs : [10 , 25]

tmin_range <- seq(9.5 ,13 , 0.25)
tmax_range <- seq(17 ,26  , 0.25)
med_t <- (tmax + tmin)/2
med_t_range <- seq( 16,21 , 0.25)
tavg_range <- seq( 14 , 23 , 0.25)
tobs_range <- seq(10 , 25 , 0.25)

data['med_t'] <- (data$TMAX + data$TMIN)/2

#Uncomment the below section if you want to look at all the graphs 
#used to find the best tref

#--------------------------------
# for (j in c('HDD' , 'CDD')) {
#   for (i in tmin_range){
#     find_tref(data$SOMME , tmin , i , type=j)
#     print(i)
#   }
# 
#   print('DONE WITH TMIN')
# 
#   for (i in tmax_range){
#     find_tref(data$SOMME , tmax , i , type=j)
#     print(i)
#   }
#   print('DONE WITH TMAX')
#   for (i in med_t_range){
#     find_tref(data$SOMME , med_t , i , type=j)
#     print(i)
#   }
#   print('DONE WITH med')
# 
#   for (i in tavg_range){
#     find_tref(data$SOMME , tavg , i , type=j)
#     print(i)
#   }
#   print('DONE WITH avg')
# 
#   for (i in tobs_range){
#     find_tref(data$SOMME , tobs , i , type=j)
#     print(i)
#   }
#   print('DONE WITH obs')
# }
#-------------------------------------------------------

# I found the best tref to be med_t for both CDD and HDD with 
# tref : HDD -> 16 and tref : CDD -> 21  

CDD <- c()
for (i in seq(1 , length(med_t))) {
  CDD[i] <- max(med_t[i] - 21 , 0 )
}

HDD <- c()
for (i in seq(1 , length(med_t))) {
  HDD[i] <- max(16 - med_t[i] , 0)
}


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

#interaction_weekend_holiday
data['inter_holi_weekend'] <- data['Holiday'] * data['weekend']

#type of day 1 : weekday , 2 : weekend , 3 : holiday ,
# 4 : holiday and weekend
type_of_day <- c()
for (i in seq(1 , length(data$DATE))) {
  if (data[i , 'inter_holi_weekend'] == 1) {
    type_of_day[i] = 4
  } else if (data[i , 'inter_holi_weekend'] == 0 & 
             data[i , 'Holiday'] == 1) {
    type_of_day[i] = 3
  } else if (data[i , 'inter_holi_weekend'] == 0 &
             data[i , 'weekend'] == 1) {
    type_of_day[i] = 2
  } else {
    type_of_day[i] = 1
  }
}

data['type_of_day'] <- type_of_day


#Season variable for ploting
winter <- c('January' , 'February' , 'March')
spring <- c('April' , 'May' , 'June')
summer <- c('July' , 'August' , 'September')
fall <- c('October', 'November' , 'December')


season <- c()
for (i in seq(1 , length(data$DATE))) {
  if (data[i , 'month'] %in% winter) {
    season[i] = 'winter'
  } else if (data[i ,'month'] %in% spring) {
    season[i] = 'spring'
  } else if (data[i , 'month'] %in% summer) {
    season[i] = 'summer'
  } else {
    season[i] = 'fall'
  }
}

data['season'] <- season

#saving master data_frame
save(data , file='master_df.Rdata')

#clearing useless variables 
rm(CDD , cp , HDD , holidays_character , i , is_holiday , 
   med_t_range , tavg_range , tef , tmin_range ,tmax_range , 
   tobs_range ,
   weekend , tavg , tmax , tmin , tobs , med_t)
