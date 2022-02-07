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
data_training = data[732:1827,] #2014-2016
data_validation = data[1828:2557,]  #2017-2018
data_test = data[2558:3652,] #2019-2021

#Creating timeseries
sdate = c(2017,1)
data_val_ts = ts(data_validation$SOMME, start=sdate, frequency=365)

#Random Walk
set.seed(123)
ffcast = c(2017,2)

random_walk = rwf(data_val_ts, drift=TRUE, h=1)
forecast_rw = window(random_walk$fitted, start=ffcast)
observed_val <- window(random_walk$x, start=ffcast)

print(accuracy(forecast_rw, observed_val)[,1:5])


#Naive no change
naive_next_day = naive(data_val_ts, h=1)
forecast_next_day <- window(naive_next_day$fitted, start=ffcast) 

print(accuracy(forecast_next_day, observed_val)[,1:5])

#Seasonal Naive, 7 jours
val_ts_s <- ts(data_validation$SOMME , start = sdate, frequency = 7)

ffcast_s = c(2017,8)
naive_s = snaive(val_ts_s, h=1)

forecast_s <- window(naive_s$fitted, start=ffcast_s)
observed_val_s <- window(naive_s$x , start=ffcast_s)

print(accuracy(forecast_s, observed_val_s)[,1:5])

#Moyenne mobile 3 jours
ffcast_mm = c(2017,4)
naive_3days <- zoo::rollmean(data_val_ts, 3, align="right")
naive_mobile <- naive(naive_3days, h=1)

forecast_mobile3 = window(naive_mobile$fitted, start=ffcast_mm)
observed_val_mm <- window(data_val_ts , start = ffcast_mm)

print(accuracy(forecast_mobile3, observed_val_mm)[,1:5])

#ylim=c(min(naive_next_day$residuals, random_walk$residuals, naive_s$residuals, naive_mobile$residuals),max(naive_next_day$residuals, random_walk$residuals, naive_s$residuals, naive_mobile$residuals))

plot(naive_next_day$residuals[1:58], lwd=2, type="o", ylim=c(-250000,200000), ylab= "Résidus (MW)", xlab="Jour", main = "Résidus des méthodes VS observations")
#lines(random_walk$residuals[1:58], type="o", lwd=2, col="green")
lines(naive_s$residuals[1:58], type="o",lwd=2, col="red")
lines(naive_mobile$residuals[1:58], type="o",lwd=2, col="yellow")
abline(0,0)
legend(x=35, y=200000, legend=c("Naïve no change", "Naïve seasonal 7j", "Moyenne mobile 3j"), col=c("black", "red", "yellow"), lty=1, bg="light blue")
    # plot(week.N, axes=F,
    #      lty=1, type="l", ylim=c(min(week.N,week.T),max(week.N,week.T)),
    #      ylab="Hourly demand (in MW) for Jan 3-9, 2010", xlab="")
    # lines(week.T, lty=3, lwd=1.8)   
     
     