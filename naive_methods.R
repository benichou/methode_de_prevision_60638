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

#Creation de la variable "season"
winter = c("January", "February", "December")
spring = c("March", "April", "May")
summer = c("June", "July", "August")
fall = c("Septemebr", "October", "November")

data = data.frame(data, season = 
        ifelse(data$month %in% winter, "Winter",
             ifelse(data$month %in% spring,"Spring",
                  ifelse(data$month %in% summer, "Summer",
                        ifelse(data$month %in% fall,"Fall", "NA")))))


# Naive forecast draft
data_training = data[1:2192,] #2012-2017
data_validation = data[2193:2922,]  #2018-2019
data_test = data[2923:3652,] #2020-2021

#Creating timeseries
sdate = c(2017,1)
data_val_ts = ts(data_validation$SOMME, start=sdate, frequency=365)

# #Random Walk - trop similaire a Naive no change
# set.seed(123)
# 
# 
# random_walk = rwf(data_val_ts, drift=TRUE, h=1)
# forecast_rw = window(random_walk$fitted, start=ffcast)
# 
# print(accuracy(forecast_rw, observed_val)[,1:5])


#Naive no change
ffcast = c(2017,2)
naive_next_day = naive(data_val_ts, h=1)
forecast_next_day <- window(naive_next_day$fitted, start=ffcast) 
observed_val <- window(naive_next_day$x, start=ffcast)

print(accuracy(forecast_next_day, observed_val)[,1:5])

#Seasonal Naive, 7 jours
val_ts_s <- ts(data_validation$SOMME , start = sdate, frequency = 7)

ffcast_s = c(2017,8)
naive_s = snaive(val_ts_s, h=1)

forecast_s <- window(naive_s$fitted, start=ffcast_s)
observed_val_s <- window(naive_s$x , start=ffcast_s)

print(accuracy(forecast_s, observed_val_s)[,1:5])

#Moyenne mobile 3 jours
ffcast_mm3 = c(2017,4)
naive_3days <- zoo::rollmean(data_val_ts, 3, align="right")
naive_mobile3 <- naive(naive_3days, h=1)

forecast_mobile3 = window(naive_mobile3$fitted, start=ffcast_mm3)
observed_val_mm3 <- window(data_val_ts , start = ffcast_mm3)

print(accuracy(forecast_mobile3, observed_val_mm)[,1:5])

#Moyenne mobile 7 jours
ffcast_mm7 = c(2017,8)
naive_7days <- zoo::rollmean(data_val_ts, 7, align="right")
naive_mobile7 <- naive(naive_7days, h=1)

forecast_mobile7 = window(naive_mobile7$fitted, start=ffcast_mm7)
observed_val_mm7 <- window(data_val_ts , start = ffcast_mm7)

print(accuracy(forecast_mobile7, observed_val_mm)[,1:5])

#Inclusion des différents forecast dans le dataset
next_day_res = c(naive_next_day$residuals)
s_res = c(naive_s$residuals)
mm3_res = c(naive_mobile3$residuals)
mm7_res = c(naive_mobile7$residuals)

nrow(next_day_res) = nrow(data_validation)

data_validation = cbind(data_validation, next_day_res, s_res,
                        mm3_res, mm7_res)

options(scipen=10000)
plot(naive_next_day$residuals[1:730], lwd=2, type="o", 
ylim=c(-250000,200000), ylab= "Résidus (MW/h)", col="blue",
xlab="Jour", main = "Résidus des méthodes")
#lines(random_walk$residuals[1:58], type="o", lwd=2, col="green")
lines(naive_s$residuals[1:730], type="o",lwd=2, col="red")
lines(naive_mobile3$residuals[1:730], type="o",lwd=2, col="yellow")
lines(naive_mobile7$residuals[1:730], type="o",lwd=2, col="green")
abline(0,0)
legend(x="bottomleft", legend=c("Naïve no change","Naïve seasonal 7j",
      "Moyenne mobile 3j", "Moyenne mobile 7j"), 
      col=c("blue", "red", "yellow", "green"), 
      lty=1, bg="light blue", cex=0.8)

     