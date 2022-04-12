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


#Model with 7 lags
arx_7 = lm(SOMME ~ ., data=covariates_df, na.action = na.omit, 
           x=T, subset = seq(beg_train+7 , end_train))

#Graphical analysis
par(mfrow=c(2,2))
plot(arx_7$residuals)
qqnorm(arx_7$residuals)
qqline(arx_7$residuals)
acf(arx_7$residuals)

#Predictions on validation subset no retrain
pred_arx_7 <- predict(arx , 
                    newdata = covariates_df[(end_train+1):end_valid,],
                    interval = 'prediction',
                    level = c(0.95))

#Moving window model
arx_7_mov = lm(SOMME ~ ., data=covariates_df, na.action = na.omit, 
           x=T, subset = seq(beg_train+rp, end_train+rp))

#Predictions on 2nd part validation after retrain with moving window
pred_arx_7_mov <- predict(arx_7_mov , 
                      newdata = covariates_df
                      [(end_train+1+rp):end_valid,],
                      interval = 'prediction',
                      level = c(0.95))

#Expanding window model
arx_7_exp = lm(SOMME ~ ., data=covariates_df, na.action = na.omit, 
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
   arx_7_daily <- lm(SOMME ~ ., data=covariates_df[s:e,], 
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

#                ME     RMSE      MAE        MPE    MAPE
#Test set -144.6148 20857.59 15772.07 -0.3429603 4.02882


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


### ARX 7 model with daily retrain and EXPANDING window ###

pred_exp_p <- c()
pred_exp_lwr <- c()
pred_exp_upr <- c()

for (i in 1:nrow(yt_valid)){
  #fit a model using lm with 7 lags of yt
  s = 8
  e = 2191 + i
  arx_7_daily <- lm(SOMME ~ ., data=covariates_df[s:e,], 
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

#                ME     RMSE      MAE        MPE    MAPE
#Test set -58.92122 20871.43 15777.04 -0.3355937 4.026368


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

###Barely any difference between expanding and moving window in terms
###of coverage at 95% and MAPE
###We will choose expanding window as best model


### Quarterly evaluation of best model ###

Q.eval <- rbind(accuracy(pred_exp_p[c(1:90,366:455)], 
                          yt_valid[c(1:90,366:455)])[,1:5], 
                 accuracy(pred_exp_p[c(91:181,456:547 )], 
                          yt_valid[c(91:181,456:547)])[,1:5],
                 accuracy(pred_exp_p[c(182:273, 548:638)], 
                          yt_valid[c(182:273, 548:638)])[,1:5],
                 accuracy(pred_exp_p[c(274:365, 639:730)], 
                          yt_valid[c(274:365, 639:730)])[,1:5])

rownames(Q.eval) <- make.names(c("Q1","Q2","Q3",
                                  "Q4"))

print(Q.eval)

#           ME     RMSE      MAE        MPE     MAPE
#Q1  -277.3404 21523.00 15529.88 -0.5635001 4.258159
#Q2  -818.6025 20245.73 15594.71 -0.4566379 4.111798
#Q3  2991.9586 24126.97 19656.57  0.5026677 4.148843
#Q4 -2123.9967 16984.80 12341.71 -0.8259611 3.592842


### Predictions on test set ###
### For test set, with daily retrain and expanding window, we need to
### fit the model on the train AND validation sample and predict on 
### whole test set. That way, the model parameters will be up to date 
### to last day of validation.

arx_test <- lm(SOMME ~ ., data=covariates_df[beg_train:end_valid,], 
                  na.action = na.omit, x=T)

pred_test <- na.omit(predict(arx_test, 
                     newdata = covariates_df[(end_valid+1:end_test),],
                          interval = c("prediction"), level=0.95))

pred_test_80 <- na.omit(predict(arx_test, 
                    newdata = covariates_df[(end_valid+1:end_test),],
                    interval = c("prediction"), level=0.80))

### Accuracy with daily retrain EXPANDING window model on TEST SET###
accuracy(pred_test[,1], covariates_df$SOMME[(end_valid+1):end_test])

              #ME     RMSE      MAE        MPE     MAPE
#Test set -1933.509 22291.16 16563.02 -0.9170829 4.352689


### Quarterly evaluation of best model for TEST SET ###

Q.eval.test <- rbind(accuracy(pred_test[c(1:90,366:455),1], 
                         yt_test[c(1:90,366:455)])[,1:5], 
                accuracy(pred_test[c(91:181,456:547 ),1], 
                         yt_test[c(91:181,456:547)])[,1:5],
                accuracy(pred_test[c(182:273, 548:638),1], 
                         yt_test[c(182:273, 548:638)])[,1:5],
                accuracy(pred_test[c(274:365, 639:730),1], 
                         yt_test[c(274:365, 639:730)])[,1:5])

rownames(Q.eval.test) <- make.names(c("Q1","Q2","Q3",
                                 "Q4"))

print(Q.eval.test)

#          ME     RMSE      MAE       MPE     MAPE
#Q1 -3269.143 24430.43 15825.91 -1.172896 4.249645
#Q2 -8090.890 21694.61 17329.50 -2.683579 4.989126
#Q3  7092.231 24874.78 19969.90  1.322838 4.323427
#Q4 -3479.682 17436.15 13133.42 -1.137682 3.849616


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

#                ME     RMSE      MAE       MPE     MAPE
#Q1.2020  -3096.219 17069.92 13350.98 -1.096661 3.857853
#Q2.2020 -11314.383 24797.47 19863.88 -3.701126 5.744332
#Q3.2020   7867.630 22794.24 17785.39  1.569713 3.849847
#Q4.2020  -5121.730 18576.20 13209.96 -1.596530 3.828000
#Overall -2892.180 21049.82 16056.92 -1.199916 4.318638

#               ME     RMSE      MAE        MPE     MAPE
#Q1.2021 -3442.068 30038.48 18300.83 -1.2491312 4.641438
#Q2.2021 -4902.434 18109.82 14822.67 -1.6770924 4.242128
#Q3.2021  6308.312 26814.58 22178.42  1.0732500 4.802211
#Q4.2021 -1837.635 16216.14 13056.87 -0.6788342 3.871232
#Overall  -974.8385 23466.93 17069.12 -0.6342501 4.386739

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
#0.8479452#

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
#0.6972603

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

#         X2020.95  X2021.95  X2020.80  X2020.95
#Q1      0.9111111 0.8222222 0.7444444 0.7111111
#Q2      0.7912088 0.8804348 0.6043956 0.7826087
#Q3      0.8152174 0.7472527 0.6413043 0.5054945
#Q4      0.8913043 0.9239130 0.7826087 0.8043478
#Overall 0.8520548 0.8438356 0.6931507 0.7013699
