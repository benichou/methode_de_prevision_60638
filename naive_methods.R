#
# Program: naive_methods.R
#
# Purpose: evaluation of the naive methods in R
#
# Written by: Team G, January 30 2021
#
# Updated: NA
#          
#         
#          
#         
#          
# ------------------------------------------------------


# upload necessary packages
library(forecast)
library(timeSeries)

# launch the data_transformation.R module
source("./data_transformation.R")

# Naive forecast draft

#Creating timeseries
sdate = 1
data_ts = ts(data$SOMME, start=sdate, frequency=365.25)

#Naive no change
ffcast = 2
naive_next_day = naive(data_ts, h=1)

forecast_next_day <- window(naive_next_day$fitted, start=ffcast) 
observed <- window(naive_next_day$x, start=ffcast)

#<<<<<<< HEAD
#naif, naive seasonal 7 jours , moyenne mobile, random walk.
#=======
print(accuracy(forecast_next_day, observed)[,1:5])

#Seasonal Naive, 7 jours
data_ts_s <- ts(data$SOMME , start = sdate , frequency =365)


ffcast_s = 8
naive_s = snaive(data_ts_s, h=1)

forecast_s <- window(naive_s$fitted, start=ffcast_s)
observed_s <- window(naive_s$x , start=ffcast_s)

print(accuracy(forecast_s, observed_s)[,1:5])

#Moyenne mobile 3 jours
ffcast_mm = 4
naive_3days <- zoo::rollmean(data_ts, 3, align="right")
naive_mobile <- naive(naive_3days, h=1)

forecast_mobile3 = window(naive_mobile$fitted, start=ffcast_mm)
observed_mm <- window(data_ts , start = ffcast_mm)

print(accuracy(forecast_mobile3, observed_mm)[,1:5])

#>>>>>>> dev
