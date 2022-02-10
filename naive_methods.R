#
# Program: naive_methods.R 
#
# Purpose: evaluation of the naive methods in R + pdf reporting
#
# Written by: Team G, January 30 2021
#
# Updated: Feb 10th 2022
#          
#         
#          
#         
#          
# ------------------------------------------------------


# upload necessary packages
library(forecast)
library(timeSeries)
library(zoo)

# launch the exploratory_vars to get the full dataset
# source("./exploratory_vars.R")


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
ffcast_mm3 = c(2018,4)
naive_3days <- rollmean(data_val_ts, 3, align="right")
naive_mobile3 <- naive(naive_3days, h=1)

forecast_mobile3 = window(naive_mobile3$fitted, start=ffcast_mm3)
observed_val_mm3 <- window(data_val_ts, start = ffcast_mm3)

print(accuracy(forecast_mobile3, observed_val_mm3)[,1:5])

#Moyenne mobile 7 jours
ffcast_mm7 = c(2018,8)
naive_7days <- rollmean(data_val_ts, 7, align="right")
naive_mobile7 <- naive(naive_7days, h=1, start=ffcast_mm7)

forecast_mobile7 = window(naive_mobile7$fitted, start=ffcast_mm7)
observed_val_mm7 <- window(data_val_ts, start = ffcast_mm7)

print(accuracy(forecast_mobile7, observed_val_mm7)[,1:5])


#Resultats
print(accuracy(forecast_next_day, observed_val)[,1:5])
print(accuracy(forecast_s, observed_val_s)[,1:5])
print(accuracy(forecast_mobile3, observed_val_mm3)[,1:5])
print(accuracy(forecast_mobile7, observed_val_mm7)[,1:5])

#Inclusion des differents residus dans le dataset
next_day_res = c(naive_next_day$residuals)
s_res = c(naive_s$residuals)
mm3_res = c(naive_mobile3$residuals)
mm7_res = c(naive_mobile7$residuals)

#Rendre les vecteurs pour avoir des longueures egales
mm3_res=c(rep(NA,2),mm3_res)
mm7_res = c(rep(NA,6),mm7_res)

fc_nextday = c(NA, forecast_next_day)
fc_s = c(rep(NA,7), forecast_s)
fc_mm3 = c(rep(NA,3), forecast_mobile3)
fc_mm7 =  c(rep(NA,7), forecast_mobile7)


#Ajouter au dataframe les residus et les forecast
data_validation = cbind(data_validation, next_day_res, s_res,
                        mm3_res, mm7_res)

data_validation = cbind(data_validation, fc_nextday, 
                        fc_s, fc_mm3, fc_mm7)

#Diebold-Mariano entre naive next day et moyenne mobile 3j
#print(dm.test((forecast_next_day-observed_val), 
#(forecast_mobile3-observed_val_mm3)))

#Nouveaux dataframes par saison
df_winter_val = data_validation[which(data_validation$
      season=="winter"),]
df_winter_val = subset(df_winter_val, select=c("SOMME", "month",
                      "next_day_res", "s_res", "mm3_res", "mm7_res",
                      "fc_nextday", "fc_s", "fc_mm3", "fc_mm7"))

df_spring_val = data_validation[which(data_validation$
                                        season=="spring"),]
df_spring_val = subset(df_spring_val, select=c("SOMME", "month",
                      "next_day_res", "s_res", "mm3_res", "mm7_res",
                      "fc_nextday", "fc_s", "fc_mm3", "fc_mm7"))

df_summer_val = data_validation[which(data_validation$
                                        season=="summer"),]
df_summer_val = subset(df_summer_val, select=c("SOMME", "month",
                      "next_day_res", "s_res", "mm3_res", "mm7_res",
                      "fc_nextday", "fc_s", "fc_mm3", "fc_mm7"))


df_fall_val = data_validation[which(data_validation$
                                      season=="fall"),]
df_fall_val = subset(df_fall_val, select=c("SOMME", "month",
                     "next_day_res", "s_res", "mm3_res", "mm7_res",
                     "fc_nextday", "fc_s", "fc_mm3", "fc_mm7"))


#Resultats des residus par saison
residuals_col <- c('next_day_res','s_res', 'mm3_res' , 'mm7_res')
#winter

winter_mapes <- c()
spring_mapes <- c()
summer_mapes <- c()
fall_mapes <- c()
for (i in seq(1, length(residuals_col))) {
  winter_mapes[i] <- mean(abs(df_winter_val[,residuals_col[i]]/
                                df_winter_val$SOMME) , na.rm = TRUE)
  spring_mapes[i] <- mean(abs(df_spring_val[,residuals_col[i]]/
                                df_spring_val$SOMME))
  summer_mapes[i] <- mean(abs(df_summer_val[,residuals_col[i]]/
                                df_summer_val$SOMME))
  fall_mapes[i] <- mean(abs(df_fall_val[,residuals_col[i]]/
                                df_fall_val$SOMME))
}




pdf("./visual_output/Residus.pdf")

###Graphiques


#Analyse graphique des residus sur la validation
options(scipen=10000)
plot(naive_next_day$residuals, lwd=1.5, type="o", 
ylim=c(-250000,200000), ylab= "Residus (MW/h)", col="blue",
xlab="Jour", 
main = "Residus du naive no change sur 
l'echantillon validation 2018-2019")
lines(naive_s$residuals[1:730], type="o",lwd=1.5, col="red")
lines(naive_mobile3$residuals[1:730], type="o",lwd=1.5, col="yellow")
lines(naive_mobile7$residuals[1:730], type="o",lwd=1.5, col="green")
abline(0,0)
legend(x="bottomleft", legend=c("Naive no change",
"Naive seasonal 7j",
"Moyenne mobile 3j", "Moyenne mobile 7j"), 
col=c("blue", "red", "yellow", "green"), 
lty=1, bg="light blue", cex=0.8)

#Previsions des mois de janvier et fevrier 2018
plot.start = which(data_validation$DATE=="2018-01-01")
plot.end = which(data_validation$DATE=="2018-02-28")

plot(observed_val[plot.start:plot.end], 
     type="o", lwd=2, xlab="Jour", 
     ylab = "Previsions et observations (MW/h)",
     main="Previsions VS observations de janvier et fevrier 2018")
lines(naive_next_day$fitted[plot.start:plot.end],
      type="o" , col="blue", lwd=2)
lines(naive_s$fitted[plot.start:plot.end],
      type="o" ,col="red", lwd=2)
lines(naive_mobile3$fitted[plot.start:plot.end],
      type="o" ,col="yellow", lwd=2)
lines(naive_mobile7$fitted[plot.start:plot.end],
      type="o" ,col="green", lwd=2)
legend(x="topright", legend=c("Observations", "Naive no change",
       "Naive seasonal 7j","Moyenne mobile 3j", "Moyenne mobile 7j"), 
       col=c("blue", "red", "yellow", "green"), 
       lty=1, bg="light blue", cex=0.8)

#Previsions des mois de juillet et aoC;t 2018
plot.start2 = which(data_validation$DATE=="2018-07-01")
plot.end2 = which(data_validation$DATE=="2018-08-31")

plot(observed_val[plot.start2:plot.end2], type="o", 
     lwd=2, xlab="Jour", 
     ylab = "Previsions et observations (MW/h)",
     main="Previsions VS observations de juillet et aoC;t 2018")
lines(naive_next_day$fitted[plot.start2:plot.end2],
      type="o" , col="blue", lwd=2)
lines(naive_s$fitted[plot.start2:plot.end2],
      type="o" ,col="red", lwd=2)
lines(naive_mobile3$fitted[plot.start2:plot.end2],
      type="o" ,col="yellow", lwd=2)
lines(naive_mobile7$fitted[plot.end2],
      type="o" ,col="green", lwd=2)
legend(x="topright", legend=c("Naive no change","Naive seasonal 7j",
                            "Moyenne mobile 3j", "Moyenne mobile 7j"), 
       col=c("blue", "red", "yellow", "green"), 
       lty=1, bg="light blue", cex=0.8)

#Previsions des mois de juillet et aoC;t 2018
plot.start2 = which(data_validation$DATE=="2018-07-01")
plot.end2 = which(data_validation$DATE=="2018-08-31")

plot(observed_val[plot.start2:plot.end2], type="l", 
     lwd=2, xlab="Jour", 
     ylab = "Previsions et observations (MW/h)",
     main="Previsions VS observations de juillet et aoC;t 2018")
lines(window(forecast_next_day, start=ffcast)[plot.start2:plot.end2],
      type="l" , col="blue", lwd=2)
lines(window(forecast_s, start=ffcast)[plot.start2:plot.end2],
      type="l",col="red", lwd=2)
lines(window(forecast_mobile3, start=ffcast)[plot.start2:plot.end2],
      type="l" ,col="yellow", lwd=2)
lines(window(forecast_mobile7, start=ffcast)[plot.start2:plot.end2],
      type="l" ,col="green", lwd=2)
legend(x="topright", 
       legend=c("Naive no change","Naive seasonal 7j",
                              "Moyenne mobile 3j", 
                "Moyenne mobile 7j"), 
       col=c("blue", "red", "yellow", "green"), 
       lty=1, bg="light blue", cex=0.8)

print("The entire pipeline ran successfully")
dev.off(dev.cur())

# plot(observedS, ylab="Monthly demand (TWh)")
# lines(forecastS, col="red")
# lines(window(forecast, start=ffcastS), col="blue")
# lines(window(forecast3, start=ffcastS), col="cyan")
     