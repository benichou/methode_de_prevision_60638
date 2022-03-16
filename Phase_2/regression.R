#Exploration des modèles de regression linéaire

#Chargement des données 
load('./regression_df.Rdata')
load('./master_df.Rdata')

#libraries
library(timeSeries)
library(forecast)

pdf("Phase_2/regress0.pdf")

#data separation
beg_train <- 1 #2012/01/01
end_train <- 2192 #2017/12/31
end_valid <- 2922 #2019/12/31

rp <- 365



#-------------------------------------------------------------------
# We can start with the basic three variables of interest
# CDD , HDD and weekday
# This is just to prove that the lm models are not adequate
# Due to the nature of their errors 

model_1 <- lm( SOMME ~ CDD + HDD + factor(weekday) , 
               data = final_data , 
               subset =  seq(beg_train , end_train))

model_1_exp_1 <- lm( SOMME ~ CDD + HDD + factor(weekday) , 
                      data = final_data , 
                      subset =  seq(beg_train , end_train+rp))

model_1_mov_1 <- lm( SOMME ~ CDD + HDD + factor(weekday) , 
                     data = final_data , 
                     subset =  seq(beg_train +rp,end_train+rp))

#analysing the residuals of the model on the training data
par(mfrow=c(2,2))
plot(model_1)

summary(model_1)

#Fitted over time
par(mfrow=c(1,1))
plot(x = seq(1 , length(model_1$model$SOMME)) ,
     y = model_1$model$SOMME ,
     type = 'l',
     lty = 2 ,
     xlab = 'Time' ,
     ylab = 'Electricity Demand' ,
     col = 'blue')
lines(x = seq(1 , length(model_1$model$SOMME)) ,
     y = model_1$fitted.values ,
     type = 'l',
     lty = 2 ,
     col = 'red')

#residuals over time
par(mfrow=c(1,1))
plot(x = seq(1 , length(model_1$model$SOMME)) ,
     y = model_1$residuals ,
     type = 'l',
     lty = 2 ,
     xlab = 'Time' ,
     ylab = 'Residuals')

#ACF of residuals
acf(residuals(model_1))

#looking at the performance measures on prediction made 
#on clean covariates

#A : no retraining on all the validation
pred_1_no <- predict(model_1 , 
                    newdata = final_data[(end_train+1):end_valid,] ,
                    interval = 'prediction',
                    level = c(0.95))

#B : retraining with expending window
pred_1_exp_1 <- predict(model_1_exp_1 , 
                  newdata = final_data[(end_train+rp+1):end_valid,] ,
                  interval = 'prediction',
                  level = c(0.95))

pred_1_exp_1 <- rbind(pred_1_no[1:rp,] , pred_1_exp_1)

#C : moving window
pred_1_mov_1 <- predict(model_1_mov_1 ,
                  newdata = final_data[(end_train+rp+1):end_valid,],
                  interval = 'prediction',
                  level = c(0.95))

pred_1_mov_1 <- rbind(pred_1_no[1:rp,] , pred_1_mov_1)

#looking at the performance measures on prediction made 
#on noisy covariates


#accuracy check
accuracy(pred_1_no[,1] , final_data[(end_train+1):end_valid,2])
accuracy(pred_1_exp_1[,1] , final_data[(end_train+1):end_valid,2])
accuracy(pred_1_mov_1[,1] , final_data[(end_train+1):end_valid,2])

#couverture without retraining
cv_no_1 <- c()
for (i in seq(1, length(pred_1_no[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_1_no[i , 2]) & 
       (final_data$SOMME[(end_train+i)] <= pred_1_no[i , 3])){
    cv_no_1[i] = 1
  } else {
    cv_no_1[i] = 0
  }
}
print(mean(cv_no_1))

#couverture with retraining expanding window
cv_exp_1 <- c()
for (i in seq(1, length(pred_1_exp_1[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_1_exp_1[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_1_exp_1[i , 3])){
    cv_exp_1[i] = 1
  } else {
    cv_exp_1[i] = 0
  }
}
print(mean(cv_exp_1))

#couverture with retraining moving window
cv_mov_1 <- c()
for (i in seq(1, length(pred_1_exp_1[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_1_mov_1[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_1_mov_1[i , 3])){
    cv_mov_1[i] = 1
  } else {
    cv_mov_1[i] = 0
  }
}
print(mean(cv_mov_1))

#La couvertue est supposée être de 95%. 
#Visiblement, le fait que l'erreur est autocorrélée cause 
#un problème même avec des variables explicatives sans bruits
#Ajouter du bruit ne ferait qu'empirer les résultats.
#On élimine donc ces modèles et on passe directement au erreur arma

#--------------------------------------------------------------------
#Le modèle 2 portera sur les mêmes variables explicatives
#que le modèle 1, toutefois, les erreurs seront, cette fois 
#autocorrélées

#Dummy for the day covariate
DMon <- ifelse(factor(final_data$weekday)=="Monday", 1, 0)
DTue <- ifelse(factor(final_data$weekday)=="Tuesday", 1, 0)
DWed <- ifelse(factor(final_data$weekday)=="Wednesday", 1, 0)
DThu <- ifelse(factor(final_data$weekday)=="Thursday", 1, 0)
DSat <- ifelse(factor(final_data$weekday)=="Saturday", 1, 0)
DSun <- ifelse(factor(final_data$weekday)=="Sunday", 1, 0)

#Making the demande a time series
Yt <- timeSeries(final_data$SOMME , final_data$DATE)

#binding our covariates as a matrix
covariates_2 <- cbind(final_data$noisy_CDD , 
                      final_data$noisy_HDD ,
                      DMon , DTue , DWed , DThu , DSat, DSun)

#Fiting auto.arima
model_2 <- auto.arima(Yt[beg_train : end_train] , 
                      xreg = covariates_2[beg_train:end_train ,])

#Expanding the window
model_2_exp_1 <- arima(Yt[beg_train : (end_train+rp)] , 
                  xreg = covariates_2[beg_train:(end_train + rp) ,],
                  order= c(1,1,2))

#moving the window
model_2_mov_1 <- arima(Yt[(beg_train+rp) : (end_train+rp)] , 
            xreg = covariates_2[(beg_train+rp):(end_train + rp) ,],
            order =c(1,1,2))

#Evaluation of the model on the training set
print(model_2)
summary(model_2)

pval_model2 <- (1 - 
pnorm(abs(model_2$coef)/sqrt(diag(model_2$var.coef))))*2
print(pval_model2)

#Evaluating the residuals
par(mfrow=c(1,1))

#Autoccorelation des residus ?
acf(residuals(model_2) , 
    main ='Regression with ARIMA(1,1,2) errors')

#Variance constante et esperance de 0 ?
plot(x = model_2$fitted , y = model_2$residuals ,
     xlab = 'Fitted values' , ylab = 'Residuals' ,
     main = 'Diagnostic plot (model 2)')

plot(x = seq(1,length(model_2$fitted)),
      y = (model_2$residuals 
           - mean(model_2$residuals))/sd(model_2$residuals),
     xlab = 'Time' ,
     ylab = 'Standerdized Residuals',
     main = 'Standerdized residuals over time')

#Normal ?
qqnorm(model_2$residuals , main = 'Normal Q-Q')
qqline(model_2$residuals )

#residuals by qaurter
boxplot( (model_2$residuals - 
            mean(model_2$residuals)) / sd(model_2$residuals)  
         ~ final_data$quarter[beg_train: end_train] ,
         main = 'Boxplot of standardized residuals by quarter' ,
         xlab = 'Quarter' ,
         ylab = 'Standardize residuals')

checkresiduals(model_2)

#Performance in the training
accuracy(model_2$fitted ,final_data[beg_train:end_train,2])

#coverage in the training
upper_2 <- fitted(model_2) + 1.95 * sqrt(model_2$sigma2)
lower_2 <- fitted(model_2) - 1.95 * sqrt(model_2$sigma2)
out <- (final_data$SOMME[beg_train: end_train] < lower_2 |
        final_data$SOMME[beg_train: end_train] > upper_2 )
print(1 - mean(out))

#prediction using a loop instead
pred_test <- c()



#Model performance on validation data
pred_2_no <- predict( model_2 , n.ahead = 1 , 
                    newxreg = covariates_2[(end_train+1):end_valid,])

#No retrain prediction
pred_2_no <- cbind(pred_2_no$pred ,
(pred_2_no$pred - 1.96 * rep(pred_2_no$se , length(pred_2_no$pred))),
(pred_2_no$pred + 1.96 * rep(pred_2_no$se , length(pred_2_no$pred))),
(pred_2_no$pred - 1.282 * rep(pred_2_no$se , length(pred_2_no$pred))),
(pred_2_no$pred + 1.282 * rep(pred_2_no$se , length(pred_2_no$pred))))

colnames(pred_2_no) <- c('fit' , 
                         'lwr_95' , 'upr_95',
                         'lwr_80' , 'upr_80')


#retrain with expanding window
pred_2_exp_1 <- predict( model_2_exp_1 , n.ahead = 1 , 
                newxreg = covariates_2[(end_train+rp+1):end_valid,])

pred_2_exp_1 <- cbind(pred_2_exp_1$pred ,
(pred_2_exp_1$pred - 1.96 * rep(pred_2_exp_1$se, 
                                length(pred_2_exp_1$pred))),
(pred_2_exp_1$pred + 1.96 * rep(pred_2_exp_1$se, 
                                length(pred_2_exp_1$pred))),
(pred_2_exp_1$pred - 1.282 * rep(pred_2_exp_1$se, 
                                 length(pred_2_exp_1$pred))),
(pred_2_exp_1$pred + 1.282 * rep(pred_2_exp_1$se, 
                                 length(pred_2_exp_1$pred))))

pred_2_exp_1 <- rbind(pred_2_no[1:rp,] , pred_2_exp_1)

colnames(pred_2_exp_1) <- c('fit' , 
                            'lwr_95' , 'upr_95',
                            'lwr_80' , 'upr_80')


#retrain with moving window
pred_2_mov_1 <- predict( model_2_mov_1 , n.ahead = 1 , 
newxreg = covariates_2[(end_train+rp+1):end_valid,])

pred_2_mov_1 <- cbind(pred_2_mov_1$pred ,
(pred_2_mov_1$pred - 1.96 * rep(pred_2_mov_1$se, 
                                length(pred_2_mov_1$pred))),
(pred_2_mov_1$pred + 1.96 * rep(pred_2_mov_1$se, 
                                length(pred_2_mov_1$pred))),
(pred_2_mov_1$pred - 1.282 * rep(pred_2_mov_1$se,
                                 length(pred_2_mov_1$pred))),
(pred_2_mov_1$pred + 1.282 * rep(pred_2_mov_1$se,
                                 length(pred_2_mov_1$pred))))

pred_2_mov_1 <- rbind(pred_2_no[1:rp,] , pred_2_mov_1)


colnames(pred_2_mov_1) <- c('fit' , 
                         'lwr_95' , 'upr_95',
                         'lwr_80' , 'upr_80')


#Peformance measure 1 : accuracy
accuracy(pred_2_no[,1] , final_data[(end_train+1):end_valid,2])

#coverage without retraining
cv_no_2 <- c()
for (i in seq(1, length(pred_2_no[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_2_no[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_2_no[i , 3])){
    cv_no_2[i] = 1
  } else {
    cv_no_2[i] = 0
  }
}

print(mean(cv_no_2))

#The coverage is very bad and off, meaning that we still have major
#Issue with the distribution of our error as seen in the diagnostic 
#plots. The chosen model is simply not good enough to predict
plot(final_data$SOMME[(end_train+1): end_valid] , type = 'n' 
     , ylim=range(pred_2_no[,2] , pred_2_no[,3]) ,
     xlab = 'Validation data time' ,
     ylab = 'Electricity demand' ,
     main = 'Coverage evaluation')
polygon( c(time(final_data$SOMME[(end_train+1): end_valid]), 
           rev(time(final_data$SOMME[(end_train+1): end_valid]))),
         c(pred_2_no[,3] , rev(pred_2_no[,2])) ,
         col = rgb(0,0,0.6,0.2) , border = FALSE)
lines(final_data$SOMME[(end_train+1): end_valid] , lty = 1)
lines(c(pred_2_no[,1]) , col = 'red', lty = 1 )
out <- (final_data$SOMME[(end_train+1): end_valid] < pred_2_no[,2] |
        final_data$SOMME[(end_train+1): end_valid] > pred_2_no[,3] )
points(time(final_data$SOMME[(end_train+1): end_valid])[out],
       final_data$SOMME[(end_train+1): end_valid][out] , pch = 19)

#Expanding window
#Peformance measure 1 : accuracy
accuracy(pred_2_exp_1[,1] , final_data[(end_train+1):end_valid,2])

#coverage with retraining expanding window
cv_exp_2 <- c()
for (i in seq(1, length(pred_2_exp_1[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_2_exp_1[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_2_exp_1[i , 3])){
    cv_exp_2[i] = 1
  } else {
    cv_exp_2[i] = 0
  }
}

print(mean(cv_exp_2))

#The coverage is very bad and off, meaning that we still have major
#Issue with the distribution of our error as seen in the diagnostic 
#plots. The chosen model is simply not good enough to predict
plot(final_data$SOMME[(end_train+1): end_valid] , type = 'n' 
     , ylim=range(pred_2_exp_1[,2] , pred_2_exp_1[,3]) ,
     xlab = 'Validation data time' ,
     ylab = 'Electricity demand' ,
     main = 'Coverage evaluation')
polygon( c(time(final_data$SOMME[(end_train+1): end_valid]), 
           rev(time(final_data$SOMME[(end_train+1): end_valid]))),
         c(pred_2_exp_1[,3] , rev(pred_2_exp_1[,2])) ,
         col = rgb(0,0,0.6,0.2) , border = FALSE)
lines(final_data$SOMME[(end_train+1): end_valid] , lty = 1)
lines(c(pred_2_exp_1[,1]) , col = 'red', lty = 1 )
out <- (final_data$SOMME[(end_train+1): end_valid] < pred_2_exp_1[,2]|
        final_data$SOMME[(end_train+1): end_valid] > pred_2_exp_1[,3])
points(time(final_data$SOMME[(end_train+1): end_valid])[out],
       final_data$SOMME[(end_train+1): end_valid][out] , pch = 19)

#Moving window
accuracy(pred_2_mov_1[,1] , final_data[(end_train+1):end_valid,2])

#coverage with moving window
cv_mov_2 <- c()
for (i in seq(1, length(pred_2_mov_1[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_2_mov_1[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_2_mov_1[i , 3])){
    cv_mov_2[i] = 1
  } else {
    cv_mov_2[i] = 0
  }
}

print(mean(cv_mov_2))

#The coverage is very bad and off, meaning that we still have major
#Issue with the distribution of our error as seen in the diagnostic 
#plots. The chosen model is simply not good enough to predict
plot(final_data$SOMME[(end_train+1): end_valid] , type = 'n' 
     , ylim=range(pred_2_mov_1[,2] , pred_2_mov_1[,3]) ,
     xlab = 'Validation data time' ,
     ylab = 'Electricity demand' ,
     main = 'Coverage evaluation')
polygon( c(time(final_data$SOMME[(end_train+1): end_valid]), 
           rev(time(final_data$SOMME[(end_train+1): end_valid]))),
         c(pred_2_mov_1[,3] , rev(pred_2_mov_1[,2])) ,
         col = rgb(0,0,0.6,0.2) , border = FALSE)
lines(final_data$SOMME[(end_train+1): end_valid] , lty = 1)
lines(c(pred_2_mov_1[,1]) , col = 'red', lty = 1 )
out <- (final_data$SOMME[(end_train+1): end_valid] < pred_2_mov_1[,2]|
final_data$SOMME[(end_train+1): end_valid] > pred_2_mov_1[,3])
points(time(final_data$SOMME[(end_train+1): end_valid])[out],
       final_data$SOMME[(end_train+1): end_valid][out] , pch = 19)

#--------------------------------------------------------------------
#model 3 : include all variables

#dummy for months
Djan <- ifelse(factor(data$month)=="January", 1, 0)
Dfeb <- ifelse(factor(data$month)=="February", 1, 0)
Dmar <- ifelse(factor(data$month)=="March", 1, 0)
Dapr <- ifelse(factor(data$month)=="April", 1, 0)
Dmay <- ifelse(factor(data$month)=="May", 1, 0)
Djun <- ifelse(factor(data$month)=="June", 1, 0)
Djul <- ifelse(factor(data$month)=="July", 1, 0)
Daug <- ifelse(factor(data$month)=="August", 1, 0)
Dsep <- ifelse(factor(data$month)=="September", 1, 0)
Doct <- ifelse(factor(data$month)=="October", 1, 0)
Dnov <- ifelse(factor(data$month)=="December", 1, 0)

#binding our covariates as a matrix
covariates_3 <- cbind(final_data$noisy_CDD,
                      final_data$noisy_HDD,
                      # lag(final_data$noisy_CDD ,1),
                      # lag(final_data$noisy_HDD ,1),
                      # lag(final_data$noisy_CDD ,2),
                      # lag(final_data$noisy_HDD ,2),
                      final_data$Holiday,
                      final_data$before_holi,
                      final_data$after_holiday,
                      final_data$noisy_humidex,
                      final_data$noisy_cp,
                      DMon , DTue , DWed , DThu , DSat, DSun ,
                      Djan, Dfeb, Dmar , Dapr, Dmay, Djun , Djul,
                      Daug, Dsep, Doct , Dnov)

#Fiting auto.arima
model_3 <- auto.arima(Yt[beg_train : end_train] , 
                      xreg = covariates_3[beg_train:end_train ,])

#Expanding the window
model_3_exp_1 <- arima(Yt[beg_train : (end_train+rp)] , 
xreg = covariates_3[beg_train:(end_train + rp) ,],
order= c(1,1,2))

#moving the window
model_3_mov_1 <- arima(Yt[(beg_train+rp) : (end_train+rp)] , 
xreg = covariates_3[(beg_train+rp):(end_train + rp) ,],
order =c(1,1,2))

#Evaluation of the model on the training set
print(model_3)
summary(model_3)
#Evaluating the residuals
par(mfrow=c(1,1))

pval_model3 <- (1 - 
pnorm(abs(model_3$coef)/sqrt(diag(model_3$var.coef))))*2
print(pval_model3)

#Autoccorelation des residus ?
acf(residuals(model_3) , 
    main ='Regression with ARIMA(1,1,2) errors')

#Variance constante et esperance de 0 ?
plot(x = model_3$fitted , y = model_3$residuals ,
     xlab = 'Fitted values' , ylab = 'Residuals' ,
     main = 'Diagnostic plot (model 3)')

plot(x = seq(1,length(model_3$fitted)),
     y = (model_3$residuals 
          - mean(model_3$residuals))/sd(model_3$residuals),
     xlab = 'Time' ,
     ylab = 'Standerdized Residuals',
     main = 'Standerdized residuals over time')

#Normal ?
qqnorm(model_3$residuals , main = 'Normal Q-Q')
qqline(model_3$residuals )

#residuals by qaurter
boxplot( (model_3$residuals - 
            mean(model_3$residuals)) / sd(model_3$residuals)  
         ~ final_data$quarter[beg_train: end_train] ,
         main = 'Boxplot of standardized residuals by quarter' ,
         xlab = 'Quarter' ,
         ylab = 'Standardize residuals')

checkresiduals(model_3)

#Performance in the training
accuracy(model_3$fitted ,final_data[beg_train:end_train,2])

#coverage in the training
upper_3 <- fitted(model_3) + 1.96 * sqrt(model_3$sigma2)
lower_3 <- fitted(model_3) - 1.96 * sqrt(model_3$sigma2)
out_3 <- (final_data$SOMME[beg_train: end_train] < lower_3 |
          final_data$SOMME[beg_train: end_train] > upper_3 )
print(1 - mean(out_3))


#Model performance on validation data
pred_3_no <- predict( model_3 , n.ahead = 1 , 
                    newxreg = covariates_3[(end_train+1):end_valid,])

#No retrain prediction
pred_3_no <- cbind(pred_3_no$pred ,
(pred_3_no$pred - 1.96 * rep(pred_3_no$se , length(pred_3_no$pred))),
(pred_3_no$pred + 1.96 * rep(pred_3_no$se , length(pred_3_no$pred))),
(pred_3_no$pred - 1.282 * rep(pred_3_no$se , length(pred_3_no$pred))),
(pred_3_no$pred + 1.282 * rep(pred_3_no$se , length(pred_3_no$pred))))

colnames(pred_3_no) <- c('fit' , 
                         'lwr_95' , 'upr_95',
                         'lwr_80' , 'upr_80')


#retrain with expanding window
pred_3_exp_1 <- predict( model_3_exp_1 , n.ahead = 1 , 
newxreg = covariates_3[(end_train+rp+1):end_valid,])

pred_3_exp_1 <- cbind(pred_3_exp_1$pred ,
(pred_3_exp_1$pred - 1.96 * rep(pred_3_exp_1$se, 
length(pred_3_exp_1$pred))),
(pred_3_exp_1$pred + 1.96 * rep(pred_3_exp_1$se, 
length(pred_3_exp_1$pred))),
(pred_3_exp_1$pred - 1.282 * rep(pred_3_exp_1$se, 
length(pred_3_exp_1$pred))),
(pred_3_exp_1$pred + 1.282 * rep(pred_3_exp_1$se, 
length(pred_3_exp_1$pred))))

pred_3_exp_1 <- rbind(pred_3_no[1:rp,] , pred_3_exp_1)

colnames(pred_3_exp_1) <- c('fit' , 
                            'lwr_95' , 'upr_95',
                            'lwr_80' , 'upr_80')


#retrain with moving window
pred_3_mov_1 <- predict( model_3_mov_1 , n.ahead = 1 , 
newxreg = covariates_3[(end_train+rp+1):end_valid,])

pred_3_mov_1 <- cbind(pred_3_mov_1$pred ,
(pred_3_mov_1$pred - 1.96 * rep(pred_3_mov_1$se, 
length(pred_3_mov_1$pred))),
(pred_3_mov_1$pred + 1.96 * rep(pred_3_mov_1$se, 
length(pred_3_mov_1$pred))),
(pred_3_mov_1$pred - 1.282 * rep(pred_3_mov_1$se,
length(pred_3_mov_1$pred))),
(pred_3_mov_1$pred + 1.282 * rep(pred_3_mov_1$se,
length(pred_3_mov_1$pred))))

pred_3_mov_1 <- rbind(pred_3_no[1:rp,] , pred_3_mov_1)


colnames(pred_3_mov_1) <- c('fit' , 
                            'lwr_95' , 'upr_95',
                            'lwr_80' , 'upr_80')


#Peformance measure 1 : accuracy
accuracy(pred_3_no[,1] , final_data[(end_train+1):end_valid,2])

#coverage without retraining
cv_no_3 <- c()
for (i in seq(1, length(pred_3_no[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_3_no[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_3_no[i , 3])){
    cv_no_3[i] = 1
  } else {
    cv_no_3[i] = 0
  }
}

print(mean(cv_no_3))

#The coverage is very bad and off, meaning that we still have major
#Issue with the distribution of our error as seen in the diagnostic 
#plots. The chosen model is simply not good enough to predict
plot(final_data$SOMME[(end_train+1): end_valid] , type = 'n' 
     , ylim=range(pred_3_no[,2] , pred_3_no[,3]) ,
     xlab = 'Validation data time' ,
     ylab = 'Electricity demand' ,
     main = 'Coverage evaluation')
polygon( c(time(final_data$SOMME[(end_train+1): end_valid]), 
           rev(time(final_data$SOMME[(end_train+1): end_valid]))),
         c(pred_3_no[,3] , rev(pred_3_no[,2])) ,
         col = rgb(0,0,0.6,0.2) , border = FALSE)
lines(final_data$SOMME[(end_train+1): end_valid] , lty = 1)
lines(c(pred_3_no[,1]) , col = 'red', lty = 1 )
out <- (final_data$SOMME[(end_train+1): end_valid] < pred_3_no[,2] |
          final_data$SOMME[(end_train+1): end_valid] > pred_3_no[,3] )
points(time(final_data$SOMME[(end_train+1): end_valid])[out],
       final_data$SOMME[(end_train+1): end_valid][out] , pch = 19)

#Expanding window
#Peformance measure 1 : accuracy
accuracy(pred_3_exp_1[,1] , final_data[(end_train+1):end_valid,2])

#coverage with retraining expanding window
cv_exp_3 <- c()
for (i in seq(1, length(pred_3_exp_1[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_3_exp_1[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_3_exp_1[i , 3])){
    cv_exp_3[i] = 1
  } else {
    cv_exp_3[i] = 0
  }
}

print(mean(cv_exp_3))

#The coverage is very bad and off, meaning that we still have major
#Issue with the distribution of our error as seen in the diagnostic 
#plots. The chosen model is simply not good enough to predict
plot(final_data$SOMME[(end_train+1): end_valid] , type = 'n' , 
     ylim=range(pred_3_exp_1[,2] , pred_3_exp_1[,3]) ,
     xlab = 'Validation data time' ,
     ylab = 'Electricity demand' ,
     main = 'Coverage evaluation')
polygon( c(time(final_data$SOMME[(end_train+1): end_valid]), 
           rev(time(final_data$SOMME[(end_train+1): end_valid]))),
         c(pred_3_exp_1[,3] , rev(pred_3_exp_1[,2])) ,
         col = rgb(0,0,0.6,0.2) , border = FALSE)
lines(final_data$SOMME[(end_train+1): end_valid] , lty = 1)
lines(c(pred_3_exp_1[,1]) , col = 'red', lty = 1 )
out <- (final_data$SOMME[(end_train+1): end_valid] < pred_3_exp_1[,2]|
        final_data$SOMME[(end_train+1): end_valid] > pred_3_exp_1[,3])
points(time(final_data$SOMME[(end_train+1): end_valid])[out],
       final_data$SOMME[(end_train+1): end_valid][out] , pch = 19)

#Moving window
accuracy(pred_3_mov_1[,1] , final_data[(end_train+1):end_valid,2])

#coverage with moving window
cv_mov_3 <- c()
for (i in seq(1, length(pred_3_mov_1[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_3_mov_1[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_3_mov_1[i , 3])){
    cv_mov_3[i] = 1
  } else {
    cv_mov_3[i] = 0
  }
}

print(mean(cv_mov_3))

#The coverage is very bad and off, meaning that we still have major
#Issue with the distribution of our error as seen in the diagnostic 
#plots. The chosen model is simply not good enough to predict
plot(final_data$SOMME[(end_train+1): end_valid] , type = 'n' 
     , ylim=range(pred_3_mov_1[,2] , pred_3_mov_1[,3]) ,
     xlab = 'Validation data time' ,
     ylab = 'Electricity demand' ,
     main = 'Coverage evaluation')
polygon( c(time(final_data$SOMME[(end_train+1): end_valid]), 
           rev(time(final_data$SOMME[(end_train+1): end_valid]))),
         c(pred_3_mov_1[,3] , rev(pred_3_mov_1[,2])) ,
         col = rgb(0,0,0.6,0.2) , border = FALSE)
lines(final_data$SOMME[(end_train+1): end_valid] , lty = 1)
lines(c(pred_3_mov_1[,1]) , col = 'red', lty = 1 )
out <- (final_data$SOMME[(end_train+1): end_valid] < pred_3_mov_1[,2]|
        final_data$SOMME[(end_train+1): end_valid] > pred_3_mov_1[,3])
points(time(final_data$SOMME[(end_train+1): end_valid])[out],
       final_data$SOMME[(end_train+1): end_valid][out] , pch = 19)
#--------------------------------------------------------------------
#Auto arima surestime beaucoup la demande et c'est du au fait que 
#Le training phase possede une tendance vers la hausse alors que le
#validation set stagne, pour corriger le tout, on peut essayer arma
#seul sans l'integrated

#Model 4 , meme modele que plus tot, mais sans le integrated

#Fiting arima with AR(1)
model_4 <- arima(Yt[beg_train : end_train] , 
xreg = covariates_3[beg_train:end_train ,],
order = c(3,0,0))

model_4$fitted <-  predict(model_4 , n.head = 1 ,
newxreg =covariates_3[beg_train:end_train ,])$pred

#Expanding the window
model_4_exp_1 <- arima(Yt[beg_train : (end_train+rp)] , 
xreg = covariates_3[beg_train:(end_train + rp) ,],
order= c(3,0,0))



#moving the window
model_4_mov_1 <- arima(Yt[(beg_train+rp) : (end_train+rp)] , 
xreg = covariates_3[(beg_train+rp):(end_train + rp) ,],
order =c(3,0,0))


#Evaluation of the model on the training set
print(model_4)
summary(model_4)
#Evaluating the residuals
par(mfrow=c(1,1))

pval_model4 <- (1 - 
pnorm(abs(model_4$coef)/sqrt(diag(model_4$var.coef))))*2
print(pval_model4)


#Autoccorelation des residus ?
acf(residuals(model_4) , 
    main ='Regression with ARIMA(3,0,0) errors')

# #Variance constante et esperance de 0 ?
# plot(x = model_4$fitted , y = model_4$residuals ,
#      xlab = 'Fitted values' , ylab = 'Residuals' ,
#      main = 'Diagnostic plot (model 4)')

plot(x = seq(1,length(model_4$fitted)),
     y = (model_4$residuals 
          - mean(model_4$residuals))/sd(model_4$residuals),
     xlab = 'Time' ,
     ylab = 'Standerdized Residuals',
     main = 'Standerdized residuals over time')

#Normal ?
qqnorm(model_4$residuals , main = 'Normal Q-Q')
qqline(model_4$residuals )

#residuals by qaurter
boxplot( (model_4$residuals - 
            mean(model_4$residuals)) / sd(model_4$residuals)  
         ~ final_data$quarter[beg_train: end_train] ,
         main = 'Boxplot of standardized residuals by quarter' ,
         xlab = 'Quarter' ,
         ylab = 'Standardize residuals')

checkresiduals(model_4)

#Performance in the training
accuracy(model_4$fitted ,final_data[beg_train:end_train,2])

#coverage in the training
upper_4 <- fitted(model_4) + 1.96 * sqrt(model_4$sigma2)
lower_4 <- fitted(model_4) - 1.96 * sqrt(model_4$sigma2)
out_4 <- (final_data$SOMME[beg_train: end_train] < lower_4 |
          final_data$SOMME[beg_train: end_train] > upper_4 )
print(1 - mean(out_4))


#Model performance on validation data
pred_4_no <- predict( model_4 , n.ahead = 1 , 
newxreg = covariates_3[(end_train+1):end_valid,])

#No retrain prediction
pred_4_no <- cbind(pred_4_no$pred ,
(pred_4_no$pred - 1.96 * rep(pred_4_no$se , length(pred_4_no$pred))),
(pred_4_no$pred + 1.96 * rep(pred_4_no$se , length(pred_4_no$pred))),
(pred_4_no$pred - 1.282 * rep(pred_4_no$se , length(pred_4_no$pred))),
(pred_4_no$pred + 1.282 * rep(pred_4_no$se , length(pred_4_no$pred))))

colnames(pred_4_no) <- c('fit' , 
                         'lwr_95' , 'upr_95',
                         'lwr_80' , 'upr_80')


#retrain with expanding window
pred_4_exp_1 <- predict( model_4_exp_1 , n.ahead = 1 , 
newxreg = covariates_3[(end_train+rp+1):end_valid,])

pred_4_exp_1 <- cbind(pred_4_exp_1$pred ,
(pred_4_exp_1$pred - 1.96 * rep(pred_4_exp_1$se, 
length(pred_4_exp_1$pred))),
(pred_4_exp_1$pred + 1.96 * rep(pred_4_exp_1$se, 
length(pred_4_exp_1$pred))),
(pred_4_exp_1$pred - 1.282 * rep(pred_4_exp_1$se, 
length(pred_4_exp_1$pred))),
(pred_4_exp_1$pred + 1.282 * rep(pred_4_exp_1$se, 
length(pred_4_exp_1$pred))))

pred_4_exp_1 <- rbind(pred_4_no[1:rp,] , pred_4_exp_1)

colnames(pred_4_exp_1) <- c('fit' , 
                            'lwr_95' , 'upr_95',
                            'lwr_80' , 'upr_80')


#retrain with moving window
pred_4_mov_1 <- predict( model_4_mov_1 , n.ahead = 1 , 
newxreg = covariates_3[(end_train+rp+1):end_valid,])

pred_4_mov_1 <- cbind(pred_4_mov_1$pred ,
(pred_4_mov_1$pred - 1.96 * rep(pred_4_mov_1$se, 
length(pred_4_mov_1$pred))),
(pred_4_mov_1$pred + 1.96 * rep(pred_4_mov_1$se, 
length(pred_4_mov_1$pred))),
(pred_4_mov_1$pred - 1.282 * rep(pred_4_mov_1$se,
length(pred_4_mov_1$pred))),
(pred_4_mov_1$pred + 1.282 * rep(pred_4_mov_1$se,
length(pred_4_mov_1$pred))))

pred_4_mov_1 <- rbind(pred_4_no[1:rp,] , pred_4_mov_1)


colnames(pred_4_mov_1) <- c('fit' , 
                            'lwr_95' , 'upr_95',
                            'lwr_80' , 'upr_80')


#Peformance measure 1 : accuracy
accuracy(pred_4_no[,1] , final_data[(end_train+1):end_valid,2])

#coverage without retraining
cv_no_4 <- c()
for (i in seq(1, length(pred_4_no[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_4_no[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_4_no[i , 3])){
    cv_no_4[i] = 1
  } else {
    cv_no_4[i] = 0
  }
}

print(mean(cv_no_4))

#The coverage is very bad and off, meaning that we still have major
#Issue with the distribution of our error as seen in the diagnostic 
#plots. The chosen model is simply not good enough to predict
plot(final_data$SOMME[(end_train+1): end_valid] , type = 'n' 
     , ylim=range(pred_4_no[,2] , pred_4_no[,3]) ,
     xlab = 'Validation data time' ,
     ylab = 'Electricity demand' ,
     main = 'Coverage evaluation')
polygon( c(time(final_data$SOMME[(end_train+1): end_valid]), 
           rev(time(final_data$SOMME[(end_train+1): end_valid]))),
         c(pred_4_no[,3] , rev(pred_4_no[,2])) ,
         col = rgb(0,0,0.6,0.2) , border = FALSE)
lines(final_data$SOMME[(end_train+1): end_valid] , lty = 1)
lines(c(pred_4_no[,1]) , col = 'red', lty = 1 )
out <- (final_data$SOMME[(end_train+1): end_valid] < pred_4_no[,2] |
          final_data$SOMME[(end_train+1): end_valid] > pred_4_no[,3])
points(time(final_data$SOMME[(end_train+1): end_valid])[out],
       final_data$SOMME[(end_train+1): end_valid][out] , pch = 19)

#Expanding window
#Peformance measure 1 : accuracy
accuracy(pred_4_exp_1[,1] , final_data[(end_train+1):end_valid,2])

#coverage with retraining expanding window
cv_exp_4 <- c()
for (i in seq(1, length(pred_4_exp_1[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_4_exp_1[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_4_exp_1[i , 3])){
    cv_exp_4[i] = 1
  } else {
    cv_exp_4[i] = 0
  }
}

print(mean(cv_exp_2))

#The coverage is very bad and off, meaning that we still have major
#Issue with the distribution of our error as seen in the diagnostic 
#plots. The chosen model is simply not good enough to predict
plot(final_data$SOMME[(end_train+1): end_valid] , type = 'n' , 
     ylim=range(pred_4_exp_1[,2] , pred_4_exp_1[,3]) ,
     xlab = 'Validation data time' ,
     ylab = 'Electricity demand' ,
     main = 'Coverage evaluation')
polygon( c(time(final_data$SOMME[(end_train+1): end_valid]), 
           rev(time(final_data$SOMME[(end_train+1): end_valid]))),
         c(pred_4_exp_1[,3] , rev(pred_4_exp_1[,2])) ,
         col = rgb(0,0,0.6,0.2) , border = FALSE)
lines(final_data$SOMME[(end_train+1): end_valid] , lty = 1)
lines(c(pred_4_exp_1[,1]) , col = 'red', lty = 1 )
out <- (final_data$SOMME[(end_train+1): end_valid] < pred_4_exp_1[,2]|
        final_data$SOMME[(end_train+1): end_valid] > pred_4_exp_1[,3])
points(time(final_data$SOMME[(end_train+1): end_valid])[out],
       final_data$SOMME[(end_train+1): end_valid][out] , pch = 19)

#Moving window
accuracy(pred_4_mov_1[,1] , final_data[(end_train+1):end_valid,2])

#coverage with moving window
cv_mov_4 <- c()
for (i in seq(1, length(pred_4_mov_1[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_4_mov_1[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_4_mov_1[i , 3])){
    cv_mov_4[i] = 1
  } else {
    cv_mov_4[i] = 0
  }
}

print(mean(cv_mov_4))

#The coverage is very bad and off, meaning that we still have major
#Issue with the distribution of our error as seen in the diagnostic 
#plots. The chosen model is simply not good enough to predict
plot(final_data$SOMME[(end_train+1): end_valid] , type = 'n' 
     , ylim=range(pred_4_mov_1[,2] , pred_4_mov_1[,3]) ,
     xlab = 'Validation data time' ,
     ylab = 'Electricity demand' ,
     main = 'Coverage evaluation')
polygon( c(time(final_data$SOMME[(end_train+1): end_valid]), 
           rev(time(final_data$SOMME[(end_train+1): end_valid]))),
         c(pred_4_mov_1[,3] , rev(pred_4_mov_1[,2])) ,
         col = rgb(0,0,0.6,0.2) , border = FALSE)
lines(final_data$SOMME[(end_train+1): end_valid] , lty = 1)
lines(c(pred_4_mov_1[,1]) , col = 'red', lty = 1 )
out <- (final_data$SOMME[(end_train+1): end_valid] < pred_4_mov_1[,2]|
          final_data$SOMME[(end_train+1): end_valid] > pred_4_mov_1[,3])
points(time(final_data$SOMME[(end_train+1): end_valid])[out],
       final_data$SOMME[(end_train+1): end_valid][out] , pch = 19)

#-------------------------------------------------------------------
#Visiblement, nos prediction surestiment categoriquement la demande
#Il va donc falloir modifier le training strategy pour ameliorer 
#La qualite de nos estimations

#Donc on va entrener le model differement
new_train_beg = 1828  #'2017-01-01'
new_train_end = 2192 #'2017-12-31'


model_5 <- auto.arima(Yt[new_train_beg :new_train_end] ,
                 xreg = covariates_3[new_train_beg:new_train_end ,])

pred_5_no <- predict(model_5 , n.ahead = 1 , 
newxreg= covariates_3[(new_train_end+1) :end_valid, ])




pred_5_no <- cbind(pred_5_no$pred ,
(pred_5_no$pred - 1.96 * rep(pred_5_no$se , length(pred_5_no$pred))),
(pred_5_no$pred + 1.96 * rep(pred_5_no$se , length(pred_5_no$pred))),
(pred_5_no$pred - 1.282 * rep(pred_5_no$se , length(pred_5_no$pred))),
(pred_5_no$pred + 1.282 * rep(pred_5_no$se , length(pred_5_no$pred))))

colnames(pred_5_no) <- c('fit' , 
                         'lwr_95' , 'upr_95',
                         'lwr_80' , 'upr_80')



accuracy(pred_5_no[,1] , Yt[(new_train_end+1):end_valid, ])

#coverage no retraining
cv_no_5 <- c()
for (i in seq(1, length(pred_5_no[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_5_no[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_5_no[i , 3])){
    cv_no_5[i] = 1
  } else {
    cv_no_5[i] = 0
  }
}

print(mean(cv_no_5))

#still extremely poor results in terms of coverage



#-------------------------------------------------------------------
#different covariates and lags
covariates_6 <- cbind(final_data$noisy_CDD,
                      final_data$noisy_HDD,
                      c(lag(timeSeries(final_data$noisy_CDD) ,1)),
                      c(lag(timeSeries(final_data$noisy_HDD) , 1)),
                      c(lag(timeSeries(final_data$noisy_CDD ), 2)),
                      c(lag(timeSeries(final_data$noisy_HDD ), 2)),
                      final_data$Holiday,
                      final_data$before_holi,
                      final_data$after_holiday,
                      final_data$noisy_humidex,
                      final_data$noisy_cp,
                      DMon , DTue , DWed , DThu , DSat, DSun ,
                      Djan, Dfeb, Dmar , Dapr, Dmay, Djun , Djul,
                      Daug, Dsep, Doct , Dnov)

model_6 <- auto.arima(Yt[(new_train_beg+2) :new_train_end] ,
xreg = covariates_6[(new_train_beg+2):new_train_end ,])

pred_6_no <- predict(model_6 , n.ahead = 1 ,
newxreg = covariates_6[(new_train_end+1):end_valid , ])


pred_6_no <- cbind(pred_6_no$pred ,
(pred_6_no$pred - 1.96 * rep(pred_6_no$se , length(pred_6_no$pred))),
(pred_6_no$pred + 1.96 * rep(pred_6_no$se , length(pred_6_no$pred))),
(pred_6_no$pred - 1.282 * rep(pred_6_no$se , length(pred_6_no$pred))),
(pred_6_no$pred + 1.282 * rep(pred_6_no$se , length(pred_6_no$pred))))

colnames(pred_6_no) <- c('fit' , 
                         'lwr_95' , 'upr_95',
                         'lwr_80' , 'upr_80')



accuracy(pred_6_no[,1] , Yt[(new_train_end+1):end_valid, ])

#coverage no retraining
cv_no_6 <- c()
for (i in seq(1, length(pred_6_no[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_6_no[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_6_no[i , 3])){
    cv_no_6[i] = 1
  } else {
    cv_no_6[i] = 0
  }
}

print(mean(cv_no_6))

#Turning off dev
dev.off(dev.cur())