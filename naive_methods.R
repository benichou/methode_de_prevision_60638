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
sdate = c(2018,1)
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
ffcast = c(2018,2)
naive_next_day = naive(data_val_ts, h=1)
forecast_next_day <- window(naive_next_day$fitted, start=ffcast) 
observed_val <- window(naive_next_day$x, start=ffcast)

print(accuracy(forecast_next_day, observed_val)[,1:5])

#Seasonal Naive 7 jours
val_ts_s <- ts(data_validation$SOMME , start = sdate, frequency = 7)

ffcast_s = c(2018,8)
naive_s = snaive(val_ts_s, h=1)

forecast_s <- window(naive_s$fitted, start=ffcast_s)
observed_val_s <- window(naive_s$x , start=ffcast_s)

print(accuracy(forecast_s, observed_val_s)[,1:5])

#Moyenne mobile 3 jours
data_val_ts_3 = ts(data_validation$SOMME, start=c(2018,4), 
                   frequency=365)
ffcast_mm3 = c(2018,4)
naive_3days <- zoo::rollmean(data_val_ts, 3, align="right")
naive_mobile3 <- naive(naive_3days, h=1)

forecast_mobile3 = window(naive_mobile3$fitted, start=ffcast_mm3)
observed_val_mm3 <- window(data_val_ts , start = ffcast_mm3)

print(accuracy(forecast_mobile3, observed_val_mm3)[,1:5])

#Moyenne mobile 7 jours
ffcast_mm7 = c(2018,8)
naive_7days <- zoo::rollmean(data_val_ts, 7, align="right")
naive_mobile7 <- naive(naive_7days, h=1, start=ffcast_mm7)

forecast_mobile7 = window(naive_mobile7$fitted, start=ffcast_mm7)
observed_val_mm7 <- window(data_val_ts)

print(accuracy(forecast_mobile7, observed_val_mm7)[,1:5])



#Inclusion des différents forecast dans le dataset
next_day_res = c(naive_next_day$residuals)
s_res = c(naive_s$residuals)
mm3_res = c(naive_mobile3$residuals)
mm7_res = c(naive_mobile7$residuals)

#Rendre les vecteurs pour avoir des longueures égales
mm3_res=c(rep(NA,2),mm3_res)
mm7_res = c(rep(NA,6),mm7_res)

#Ajouter au dataframe
data_validation = cbind(data_validation, next_day_res, s_res,
                        mm3_res, mm7_res)

                        
pdf("./visual_output/Residus.pdf")
                        
options(scipen=10000)
plot(naive_next_day$residuals[1:730], lwd=1.5, type="o", 
ylim=c(-250000,200000), ylab= "Résidus (MW/h)", col="blue",
xlab="Jour", 
main = "Résidus des méthodes sur l'échantillon validation 2018-2019")
lines(naive_s$residuals[1:730], type="o",lwd=1.5, col="red")
lines(naive_mobile3$residuals[1:730], type="o",lwd=1.5, col="yellow")
lines(naive_mobile7$residuals[1:730], type="o",lwd=1.5, col="green")
abline(0,0)
legend(x="bottomleft", legend=c("Naïve no change","Naïve seasonal 7j",
      "Moyenne mobile 3j", "Moyenne mobile 7j"), 
      col=c("blue", "red", "yellow", "green"), 
      lty=1, bg="light blue", cex=0.8)

#Prévisions des mois de janvier et février 2018
plot(observed_val[which(data_validation$DATE=="2018-01-01"):
                 which(data_validation$DATE=="2018-02-28")], 
     type="o", lwd=2, xlab="Jour", 
     ylab = "Prévisions et observations (MW/h)",
     main="Prévisions VS observations de janvier et février 2018")
lines(naive_next_day$fitted[1:59],type="o" , col="blue", lwd=2)
lines(naive_s$fitted[1:59],type="o" ,col="red", lwd=2)
lines(naive_mobile3$fitted[1:59],type="o" ,col="yellow", lwd=2)
lines(naive_mobile7$fitted[1:59],type="o" ,col="green", lwd=2)
legend(x="topright", legend=c("Naïve no change","Naïve seasonal 7j",
                            "Moyenne mobile 3j", "Moyenne mobile 7j"), 
       col=c("blue", "red", "yellow", "green"), 
       lty=1, bg="light blue", cex=0.8)

#Prévisions des mois de juillet et août 2018
plot(observed_val
     [which(data_validation$DATE=="2018-07-01"):
     which(data_validation$DATE=="2018-08-31")], type="o", 
     lwd=2, xlab="Jour", 
     ylab = "Prévisions et observations (MW/h)",
     main="Prévisions VS observations de juillet et août 2018")
lines(naive_next_day$fitted[182:243],type="o" , col="blue", lwd=2)
lines(naive_s$fitted[182:243],type="o" ,col="red", lwd=2)
lines(naive_mobile3$fitted[182:243],type="o" ,col="yellow", lwd=2)
lines(naive_mobile7$fitted[182:243],type="o" ,col="green", lwd=2)
legend(x="topright", legend=c("Naïve no change","Naïve seasonal 7j",
                            "Moyenne mobile 3j", "Moyenne mobile 7j"), 
       col=c("blue", "red", "yellow", "green"), 
       lty=1, bg="light blue", cex=0.8)

dev.off(dev.cur())
     