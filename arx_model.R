source("timeseries_x.R")

library(dynlm)
library(astsa)

#Used later
beg_train <- 1 #2012/01/01
end_train <- 2192 #2017/12/31
end_valid <- 2922 #2019/12/31

rp <- 365

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


#Performance on validation
accuracy(pred_arx_7[,1], covariates_df$SOMME[2193:2922])
accuracy(pred_arx_7_mov[,1], covariates_df$SOMME[2193:2922])
accuracy(pred_arx_7_exp[,1], covariates_df$SOMME[2193:2922])

#Coverage rates for all 3 models
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

#Moving window
cv_mov_retrain <- c()
for (i in seq(1, length(pred_arx_7_mov[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_arx_7_mov[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_arx_7_mov[i , 3])){
    cv_mov_retrain[i] = 1
  } else {
    cv_mov_retrain[i] = 0
  }
}

#Expanding window
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

