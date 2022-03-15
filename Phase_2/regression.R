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

#Express the time with t starting at 1 to length of data to
#make coding easier
final_data['t'] <- seq(1 , length(final_data$DATE) , 1)

#-------------------------------------------------------------------
# We can start with the basic two variables of interest
# temp : CDD , HDD and weekday

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

#looking at the performance measures

#A : no retraining on all the validation
pred_1_no <- predict(model_1 , 
                    newdata = final_data[(end_train+1):end_valid,])

#B : retraining with expending window
pred_1_exp_1 <- predict(model_1_exp_1 , 
                    newdata = final_data[(end_train+rp+1):end_valid,])
pred_1_exp_1 <- c(pred_1_no[1:rp] , pred_1_exp_1)

#C : moving window
pred_1_mov_1 <- predict(model_1_mov_1 ,
                  newdata = final_data[(end_train+rp+1):end_valid,])
pred_1_mov_1 <- c(pred_1_no[1:rp] , pred_1_mov_1)

#accuracy check
accuracy(pred_1_no , final_data[(end_train+1):end_valid,2])
accuracy(pred_1_exp_1 , final_data[(end_train+1):end_valid,2])
accuracy(pred_1_mov_1 , final_data[(end_train+1):end_valid,2])

#--------------------------------------------------------------------
#We should add holidays, before and after
model_2 <- lm( SOMME ~ CDD + HDD + factor(weekday) + Holiday +
                 before_holi + after_holiday, 
               data = final_data , 
               subset =  seq(beg_train , end_train))

summary(model_2)


#--------------------------------------------------------------------
#Looking at the relation between cp and HDD
plot(x = final_data$CP , y = final_data$HDD ,
     xlab = 'Effet de refroidissement' ,
     ylab = 'Heating degree days')

#Looking at the relation between humidex and CDD
plot(x = final_data$hum_final , y = final_data$CDD ,
     xlab = 'Effet de refroidissement' ,
     ylab = 'Cooling degree days')

#Because they are very similar, how important is it to have both CDD,
#HDD and all the other variables

#Turning off dev
dev.off(dev.cur())