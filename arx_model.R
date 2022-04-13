source("timeseries_x.R")

library(dynlm)
library(astsa)

#Used later
beg_train <- 1 #2012/01/01
end_train <- 2192 #2017/12/31
end_valid <- 2922 #2019/12/31
end_test <- 3652 #2021/12/30
rp <- 365

yt_test = final_data$SOMME[(end_valid+1):end_test]

#Function taken from arx.R on ZoneCours for plots analysis

plot.diag <- function(o) { 
  
  par(mfrow=c(2,2))
  plot(o$fitted, o$residuals)
  for (k in 2:dim(o$x)[2]) { 
    plot(o$x[,k], o$residuals, xlab=dimnames(o$x)[2][[1]][k]) }
  qqnorm(o$residuals); abline(a=0, b=1, col="blue")
  acf(o$residuals)
  
}

#Creating model
yt <- timeSeries(final_data$SOMME , final_data$DATE)
yt_train <- window(yt , start = '2012-01-01' , end = '2017-12-31')
yt_valid <- window(yt , start = '2018-01-01' , end = '2019-12-31')

lag1 = final_data$SOMME[1:(length(final_data$SOMME)-1)]
lag1 = c(NA, lag1)
lag2 = final_data$SOMME[1:(length(final_data$SOMME)-2)]
lag2 = c(rep(NA,2), lag2)
lag3 = final_data$SOMME[1:(length(final_data$SOMME)-3)]
lag3 = c(rep(NA,3), lag3)
lag4 = final_data$SOMME[1:(length(final_data$SOMME)-4)]
lag4 = c(rep(NA,4), lag4)
lag5 = final_data$SOMME[1:(length(final_data$SOMME)-5)]
lag5 = c(rep(NA,5), lag5)
lag6 = final_data$SOMME[1:(length(final_data$SOMME)-6)]
lag6 = c(rep(NA,6), lag6)
lag7 = final_data$SOMME[1:(length(final_data$SOMME)-7)]
lag7 = c(rep(NA,7), lag7)

time = 1:length(yt)


#HDD and CDD lags 1 and 2
lag_CDD1 = final_data$CDD[1:(length(final_data$CDD)-1)]
lag_CDD1 = c(NA, lag_CDD1)
lag_CDD2 = final_data$CDD[1:(length(final_data$CDD)-2)]
lag_CDD2 = c(NA, NA, lag_CDD2)
lag_HDD1 = final_data$HDD[1:(length(final_data$HDD)-1)]
lag_HDD1 = c(NA, lag_HDD1)
lag_HDD2 = final_data$HDD[1:(length(final_data$HDD)-2)]
lag_HDD2 = c(NA, NA, lag_HDD2)


#Dataframe with all required covariates
final_data$weekday = as.factor(final_data$weekday)
covariates_df = final_data
covariates_df = covariates_df[-c(1, 3:4, 6, 8, 15)]
covariates_df$time= time
covariates_df$lag1 = lag1
covariates_df$lag2 = lag2
covariates_df$lag3 = lag3
covariates_df$lag4 = lag4
covariates_df$lag5 = lag5
covariates_df$lag6 = lag6
covariates_df$lag7 = lag7
covariates_df$month = as.factor(data$month)
covariates_df$lag_CDD1 = lag_CDD1
covariates_df$lag_CDD2 = lag_CDD2
covariates_df$lag_HDD1 = lag_HDD1
covariates_df$lag_HDD2 = lag_HDD2
covariates_df$season = as.factor(data$season)
#covariates_df$weekend = as.factor(data$weekend)


#Model with 7 lags
arx_7 = lm(SOMME ~ .- month, data=covariates_df, na.action = na.omit, 
           x=T, subset = seq(beg_train+7, end_train))

#Graphical analysis
par(mfrow=c(2,2))
plot(arx_7$residuals, type="l")
qqnorm(arx_7$residuals)
qqline(arx_7$residuals)
acf(arx_7$residuals)

#Predictions on validation subset no retrain
pred_arx_7 <- predict(arx_7 , 
                    newdata = covariates_df[(end_train+1):end_valid,],
                    interval = 'prediction',
                    level = c(0.95))

#Moving window model
arx_7_mov = lm(SOMME ~ . -lag_HDD1 - lag_HDD2 - month, data=covariates_df, na.action = na.omit, 
           x=T, subset = seq(beg_train+rp, end_train+rp))

#Predictions on 2nd part validation after retrain with moving window
pred_arx_7_mov <- predict(arx_7_mov , 
                      newdata = covariates_df
                      [(end_train+1+rp):end_valid,],
                      interval = 'prediction',
                      level = c(0.95))

#Expanding window model
arx_7_exp = lm(SOMME ~ .-lag_HDD1 - lag_HDD2 - month, data=covariates_df, na.action = na.omit, 
               x=T, subset = seq(beg_train+7, end_train+rp))

#Predictions on 2nd part validation after retrain with exp. window
pred_arx_7_exp <- predict(arx_7_exp , 
                      newdata = covariates_df
                      [(end_train+1+rp):end_valid,],
                      interval = 'prediction',
                      level = c(0.95))

#Generating predictions vectors for exp.and moving windows
pred_arx_7_mov = rbind(pred_arx_7[1:rp,], pred_arx_7_mov)
pred_arx_7_exp = rbind(pred_arx_7[1:rp,], pred_arx_7_exp)


### Performance on validation ###
accuracy(pred_arx_7[,1], covariates_df$SOMME[2193:2922])
accuracy(pred_arx_7_mov[,1], covariates_df$SOMME[2193:2922])
accuracy(pred_arx_7_exp[,1], covariates_df$SOMME[2193:2922])

### Coverage rates for all 3 models ###
#No retrain
cv_noretrain <- c()
for (i in seq(1, length(pred_arx_7[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_arx_7[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_arx_7[i , 3])){
    cv_noretrain[i] = 1
  } else {
    cv_noretrain[i] = 0
  }
}

#Moving window with 1 retrain
cv_mov_retrain <- c()
for (i in seq(1, length(pred_arx_7_mov[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_arx_7_mov[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_arx_7_mov[i , 3])){
    cv_mov_retrain[i] = 1
  } else {
    cv_mov_retrain[i] = 0
  }
}

#Expanding window with 1 retrain
cv_exp_retrain <- c()
for (i in seq(1, length(pred_arx_7_exp[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_arx_7_exp[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_arx_7_exp[i , 3])){
    cv_exp_retrain[i] = 1
  } else {
    cv_exp_retrain[i] = 0
  }
}

print(mean(cv_noretrain))
print(mean(cv_mov_retrain))
print(mean(cv_exp_retrain))


#Coverage graph on moving window ARX model
par(mfrow=c(1,1))
plot(covariates_df$SOMME[2193:2293], type = "l", ylab="Previsions",
     main = "Taux de couverture du modele ARX avec moving window of
     first 100 days of validation")
lines(pred_arx_7_mov[,1][0:100], col="red")
lines(pred_arx_7_mov[,2][0:100], col="blue", lty = 2)
lines(pred_arx_7_mov[,3][0:100], col="blue", lty=2)
legend(x="topright", legend=c("Observations",
                            "Previsions","Borne inf. et borne sup."), 
       col=c("black", "red", "blue"), 
       lty=1, bg="light blue", cex=0.8)


### ARX 7 model with daily retrain and MOVING window ###
 
pred_mov_p <- c()
pred_mov_lwr <- c()
pred_mov_upr <- c()
 
 for (i in 1:nrow(yt_valid)){
   #fit a model using lm with 7 lags of yt
   s = 7 + i
   e = 2191 + i
   arx_7_daily <- lm(SOMME ~ . - month, data=covariates_df[s:e,], 
              na.action = na.omit, x=T
   )
   j = e + 1
   pred_mov_daily <-predict(arx_7_daily , newdata = covariates_df[j,],
                   interval = c("prediction"), level=0.95)
   
   pred_mov_p[i] <- pred_mov_daily[,1]
   pred_mov_lwr[i] <- pred_mov_daily[,2]
   pred_mov_upr[i] <- pred_mov_daily[,3]
   print(i)
 }

### Accuracy with daily retrain MOVING window model ###
accuracy(pred_mov_p, covariates_df$SOMME[2193:2922])

#           ME  RMSE   MAE    MPE MAPE
#Test set -124 20496 15345 -0.295 3.94


### Coverage with daily retrain MOVING window model ###
cv_mov_daily <- c()
for (i in seq(1, length(pred_mov_p))){
  if ((final_data$SOMME[(end_train+i)] >= pred_mov_lwr[i]) & 
      (final_data$SOMME[(end_train+i)] <= pred_mov_upr[i])){
    cv_mov_daily[i] = 1
  } else {
    cv_mov_daily[i] = 0
  }
}

print(mean(cv_mov_daily))
#0.853

### ARX 7 model with daily retrain and EXPANDING window ###

pred_exp_p <- c()
pred_exp_lwr <- c()
pred_exp_upr <- c()

for (i in 1:nrow(yt_valid)){
  #fit a model using lm with 7 lags of yt
  s = 8
  e = 2191 + i
  arx_7_daily <- lm(SOMME ~ . - month, data=covariates_df[s:e,], 
                    na.action = na.omit, x=T
  )
  j = e + 1
  pred_exp_daily <- predict(arx_7_daily , newdata = covariates_df[j,],
                            interval = c("prediction"), level=0.95)
  
  pred_exp_p[i] <- pred_exp_daily[,1]
  pred_exp_lwr[i] <- pred_exp_daily[,2]
  pred_exp_upr[i] <- pred_exp_daily[,3]
  print(i)
}

### Accuracy with daily retrain EXPANDING window model ###
accuracy(pred_exp_p, covariates_df$SOMME[2193:2922])

#         ME  RMSE   MAE    MPE MAPE
#Test set 17 20518 15353 -0.269 3.94


### Coverage with daily retrain EXPANDING window model ###
cv_exp_daily <- c()
for (i in seq(1, length(pred_exp_p))){
  if ((final_data$SOMME[(end_train+i)] >= pred_exp_lwr[i]) & 
      (final_data$SOMME[(end_train+i)] <= pred_exp_upr[i])){
    cv_exp_daily[i] = 1
  } else {
    cv_exp_daily[i] = 0
  }
}

print(mean(cv_exp_daily))
#0.844

###Barely any difference between expanding and moving window in terms
###of coverage at 95% and MAPE
###We will choose expanding window as best model


### Quarterly evaluation of best model ###

Q.eval <- rbind(accuracy(pred_mov_p[c(1:90,366:455)], 
                          yt_valid[c(1:90,366:455)])[,1:5], 
                 accuracy(pred_mov_p[c(91:181,456:547 )], 
                          yt_valid[c(91:181,456:547)])[,1:5],
                 accuracy(pred_mov_p[c(182:273, 548:638)], 
                          yt_valid[c(182:273, 548:638)])[,1:5],
                 accuracy(pred_mov_p[c(274:365, 639:730)], 
                          yt_valid[c(274:365, 639:730)])[,1:5],
                accuracy(pred_mov_p[1:730], 
                         yt_valid[1:730])[,1:5]
                )

rownames(Q.eval) <- make.names(c("Q1","Q2","Q3",
                                  "Q4", "Overall"))

print(Q.eval)

#           ME  RMSE   MAE    MPE MAPE
#Q1        426 20868 14945 -0.213 4.12
#Q2      -1214 20363 15482 -0.570 4.10
#Q3       2099 23287 18890  0.295 4.01
#Q4      -1789 16996 12076 -0.689 3.54
#Overall  -124 20496 15345 -0.295 3.94

### Coverage on validation set ###

cov.val <- rbind(mean(cv_mov_daily[c(1:90,366:455)]), 
                     mean(cv_mov_daily[c(91:181,456:547)]), 
                     mean(cv_mov_daily[c(182:273, 548:638)]),
                     mean(cv_mov_daily[c(274:365, 639:730)]),
                     mean(cv_mov_daily[1:730]))


rownames(cov.val) <- 
  make.names(c("Q1 95%","Q2 95%","Q3 95%",
               "Q4 95%", "Overall 95%"))
cov.val

#Q1.95.      0.861
#Q2.95.      0.836
#Q3.95.      0.781
#Q4.95.      0.935
#Overall.95. 0.853

### Predictions on test set ###
### For test set, with daily retrain and expanding window, we need to
### fit the model on the train AND validation sample and predict on 
### whole test set. That way, the model parameters will be up to date 
### to last day of validation.

arx_test <- lm(SOMME ~ . - month, data=covariates_df[beg_train:end_valid,], 
                  na.action = na.omit, x=T)

pred_test <- na.omit(predict(arx_test, 
                     newdata = covariates_df[(end_valid+1:end_test),],
                          interval = c("prediction"), level=0.95))

pred_test_80 <- na.omit(predict(arx_test, 
                    newdata = covariates_df[(end_valid+1:end_test),],
                    interval = c("prediction"), level=0.80))

### Accuracy with daily retrain EXPANDING window model on TEST SET###
accuracy(pred_test[,1], covariates_df$SOMME[(end_valid+1):end_test])

#            ME  RMSE   MAE    MPE MAPE
#Test set -1326 21560 15623 -0.662  4.1


### Quarterly evaluation of best model for TEST SET ###

Q.eval.test <- rbind(accuracy(pred_test[c(1:90,366:455),1], 
                         yt_test[c(1:90,366:455)])[,1:5], 
                accuracy(pred_test[c(91:181,456:547 ),1], 
                         yt_test[c(91:181,456:547)])[,1:5],
                accuracy(pred_test[c(182:273, 548:638),1], 
                         yt_test[c(182:273, 548:638)])[,1:5],
                accuracy(pred_test[c(274:365, 639:730),1], 
                         yt_test[c(274:365, 639:730)])[,1:5],
                accuracy(pred_test[1:730], 
                         yt_test[1:730])[,1:5]
                )

rownames(Q.eval.test) <- make.names(c("Q1","Q2","Q3",
                                 "Q4", "Overall Test"))

print(Q.eval.test)

#                ME  RMSE   MAE    MPE MAPE
#Q1            -778 23971 14505 -0.381 3.88
#Q2           -6598 20556 15977 -2.206 4.56
#Q3            4415 23736 18867  0.725 4.11
#Q4           -2329 17360 13137 -0.781 3.85
#Overall.Test -1326 21560 15623 -0.662 4.10


### Quarterly evaluation PER YEAR of test set of best model ###
Q.eval.2020 <- rbind(accuracy(pred_test[c(1:90),1], 
                              yt_test[c(1:90)])[,1:5], 
                     accuracy(pred_test[c(91:181),1], 
                              yt_test[c(91:181)])[,1:5],
                     accuracy(pred_test[c(182:273),1], 
                              yt_test[c(182:273)])[,1:5],
                     accuracy(pred_test[c(274:365),1], 
                              yt_test[c(274:365)])[,1:5],
                     accuracy(pred_test[c(1:365),1], 
                              yt_test[c(1:365)])[,1:5])


rownames(Q.eval.2020) <- make.names(c("Q1 2020","Q2 2020","Q3 2020",
                                      "Q4 2020", "Overall 2020"))

Q.eval.2021 <- rbind(accuracy(pred_test[c(366:455),1], 
                              yt_test[c(366:455)])[,1:5], 
                     accuracy(pred_test[c(456:547),1], 
                              yt_test[c(456:547)])[,1:5],
                     accuracy(pred_test[c(548:638),1], 
                              yt_test[c(548:638)])[,1:5],
                     accuracy(pred_test[c(639:730),1], 
                              yt_test[c(639:730)])[,1:5],
                     accuracy(pred_test[c(366:730),1], 
                              yt_test[c(366:730)])[,1:5])

rownames(Q.eval.2021) <- make.names(c("Q1 2021","Q2 2021","Q3 2021",
                                      "Q4 2021", "Overall 2021"))

print(Q.eval.2020)
print(Q.eval.2021)


#                ME  RMSE   MAE    MPE MAPE
#Q1.2020       -436 15843 12139 -0.275 3.48
#Q2.2020      -8325 23023 17866 -2.775 5.10
#Q3.2020       4936 22072 16851  0.878 3.66
#Q4.2020      -4089 18086 12886 -1.284 3.74
#Overall.2020 -1970 19982 14943 -0.862 3.99

#                ME  RMSE   MAE    MPE MAPE
#Q1.2021      -1119 29971 16871 -0.487 4.28
#Q2.2021      -4889 17782 14110 -1.644 4.03
#Q3.2021       3889 25308 20906  0.571 4.56
#Q4.2021       -568 16602 13388 -0.278 3.96
#Overall.2021  -682 23029 16303 -0.462 4.21

### Overall coverage on TEST SET at 0.95 ###

cv_test <- c()
for (i in seq(1, length(pred_test[,1]))){
  if ((final_data$SOMME[(end_valid+i)] >= pred_test[i,2]) & 
      (final_data$SOMME[(end_valid+i)] <= pred_test[i,3])){
    cv_test[i] = 1
  } else {
    cv_test[i] = 0
  }
}

print(mean(cv_test))
#0.859#

### Overall coverage on TEST SET at 0.80 ###

cv_test_80 <- c()
for (i in seq(1, length(pred_test_80[,1]))){
  if ((final_data$SOMME[(end_valid+i)] >= pred_test_80[i,2]) & 
      (final_data$SOMME[(end_valid+i)] <= pred_test_80[i,3])){
    cv_test_80[i] = 1
  } else {
    cv_test_80[i] = 0
  }
}

print(mean(cv_test_80))
#0.696

### Coverage per quarter on test set ###

cov.2020.95 <- rbind(mean(cv_test[1:90]), 
                     mean(cv_test[91:181]), 
                     mean(cv_test[182:273]),
                     mean(cv_test[274:365]),
                     mean(cv_test[1:365]))


rownames(cov.2020.95) <- 
  make.names(c("Q1 2020 95%","Q2 2020 95%","Q3 2020 95%",
               "Q4 2020 95%", "Overall 2020 95%"))

cov.2020.80 <- rbind(mean(cv_test_80[1:90]), 
                     mean(cv_test_80[91:181]), 
                     mean(cv_test_80[182:273]),
                     mean(cv_test_80[274:365]),
                     mean(cv_test_80[1:365]))


rownames(cov.2020.80) <- 
  make.names(c("Q1 2020 80","Q2 2020 80","Q3 2020 80",
               "Q4 2020 80%", "Overall 2020 80"))

cov.2021.95 <- rbind(mean(cv_test[366:455]), 
                     mean(cv_test[456:547]), 
                     mean(cv_test[548:638]),
                     mean(cv_test[639:730]),
                     mean(cv_test[366:730]))


rownames(cov.2021.95) <- 
  make.names(c("Q1 2021 95%","Q2 2021 95%","Q3 2021 95%",
               "Q4 2021 95%", "Overall 2021 95%"))

cov.2021.80 <- rbind(mean(cv_test_80[366:455]), 
                  mean(cv_test_80[456:547]), 
                  mean(cv_test_80[548:638]),
                  mean(cv_test_80[639:730]),
                  mean(cv_test_80[366:730]))


rownames(cov.2021.80) <- 
  make.names(c("Q1 2021 80%","Q2 2021 80%","Q3 2021 80%",
               "Q4 2021 80%", "Overall 2021 80%"))

cov_results =cbind(cov.2020.95, cov.2021.95, cov.2020.80, cov.2021.80)

rownames(cov_results) <- 
  make.names(c("Q1","Q2", "Q3", "Q4", "Overall"))

colnames(cov_results) <-
  make.names(c("2020 95","2021 95", "2020 80", "2020 95"))

print(cov_results)

#           X2020.95 X2021.95 X2020.80 X2020.95
#Q1         0.900    0.878    0.767    0.711
#Q2         0.780    0.902    0.648    0.761
#Q3         0.837    0.758    0.641    0.495
#Q4         0.880    0.935    0.783    0.761
#Overall    0.849    0.868    0.710    0.682

#Residuals analysis
par(mfrow=c(1,2))
plot(arx_7$residuals)
qqnorm(arx_7$residuals)
qqline(arx_7$residuals)
acf2(arx_7$residuals)
Box.test(arx_7$residuals, lag=7 , type=c("Ljung-Box"))

#Graphique pour la presentation
par(mfrow=c(1,1))
plot(final_data$SOMME[2558:(2558+364)], type="l", xlab= "Jours
     depuis le 1er janvier 2019", main="Observations de la demande 
     journali??re en 2019",
     ylab = "Demande en MWh", lwd=1.75)
abline(v=91, col="red", lty=2)
abline(v=183, col="red", lty=2)
abline(v=274, col="red", lty=2)

