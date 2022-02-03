# upload necessary packages
library(forecast)
library(timeSeries)

# launch the data_transformation.R module
setwd("C:/Users/joeyo/methode_de_prevision_60638")
source("./data_transformation.R")

# Naive forecast draft

#Creating timeseries
sdate = c(2012,1)
data_ts = ts(data$SOMME, start=sdate, frequency=365.25)

#Naive no change
ffcast = c(2012,1)
naive_next_day = naive(data_ts, h=1)

forecast_next_day <- window(naive_next_day$fitted, start=ffcast) 
observed <- window(naive_next_day$x, start=ffcast)

print(accuracy(forecast_next_day, observed)[,1:5])

#Seasonal Naive, 7 jours
ffcast_s = c(2012,8)
naive_s = snaive(data_ts, h=1)

forecast_s <- window(naive_seasonal$fitted, start=ffcast_s)

print(accuracy(forecast_s, observed)[,1:5])

#Moyenne mobile 3 jours
naive_3days <- zoo::rollmean(data_ts, 3, align="right")
naive_mobile = naive(naive_3days, h=1)

forecast_mobile3 = window(naive_mobile$fitted, start=c(2012,4))

print(accuracy(forecast_mobile3, observed)[,1:5])

