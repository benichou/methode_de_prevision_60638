#Exploration des modèles de regression linéaire

#Chargement des données 
load('./regression_df.Rdata')

#libraries
library(timeSeries)
library(forecast)

#
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

#Fitted vs observed
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
covariates_2 <- cbind(final_data$CDD , final_data$HDD ,
                      DMon , DTue , DWed , DThu , DSat, DSun)

#Fiting auto.arima
model_2 <- auto.arima(Yt , xreg = covariates_2 )


#Evaluating the residuals
par(mfrow=c(1,1))

#Autoccorelation des residus ?
acf(residuals(model_2) , 
    main ='Regression with ARIMA(1,1,3) errors')

#Variance constante et esperance de 0 ?
plot(x = model_2$fitted , y = model_2$residuals ,
     xlab = 'Fitted values' , ylab = 'Residuals' ,
     main = 'Diagnostic plot (model 2)')

plot(x = seq(1,length(model_2$fitted)),
     y = (model_2$residuals - mean(model_2$residuals))/sd(model_2$residuals),
     xlab = 'Time' ,
     ylab = 'Standerdized Residuals',
     main = 'Standerdized residuals over time')

#Normal ?
qqnorm(model_2$residuals , main = 'Normal Q-Q')
qqline(model_2$residuals )

# 

#--------------------------------------------------------------------


#Turning off dev
dev.off(dev.cur())