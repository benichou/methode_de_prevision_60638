#
# Program: sarima.R
#
# Purpose: exploring SARIMA models for one day ahead forecasting
# Written by: Team G, April 2nd 2022
#
# Updated: April 8th 2022
#          
#         
#          
#         
#          
# ------------------------------------------------------

library(astsa)

# our target variable, yt, is a time series built in the 
# timeseries_x.R script

# our train is yt_train from timeseries_x.R script

# our validation is yt_validation from timeseries_x.R script

plot(yt_train,type="l",
     ylab="Texas Daily Demand in Region North-Central(in MW)")
print("We observe a trend and seasonality in the time series")
print("We can discard ARIMA models then")
print("The time series is not stationary")

# acf on the target variable does not show stationarity
acf2(yt_train)

# is there a linear relationship between Yt and Yt-1
lag1.plot(yt_train)
print("There is a strong linear dependence between Yt
    and Yt-1 since the correlation is at 0.9")


# looks like it's stationary after first differencing
plot(diff(yt_train,1),type="l",
     ylab="lag 1 difference Daily Texas Daily Demand 
     in Region North-Central(in MW)")

# Yt vs Yt-1 --> no major dependence since corr at 
## 0.09
lag1.plot(diff(yt_train,1)[-1])

