

library(forecast)

attach (data)


#Fitting models without any noise on the explanatory variables

#Creating the dummy variables for the day of the week
#We choose Friday as base 
data$DMon <- ifelse(factor(data$weekday)=="Monday", 1, 0)
data$DTue <- ifelse(factor(data$weekday)=="Tuesday", 1, 0)
data$DWed <- ifelse(factor(data$weekday)=="Wednesday", 1, 0)
data$DThu <- ifelse(factor(data$weekday)=="Thursday", 1, 0)
data$DSat <- ifelse(factor(data$weekday)=="Saturday", 1, 0)
data$DSun <- ifelse(factor(data$weekday)=="Sunday", 1, 0)

#Creating the dummy variables for the months 
#We choose Decdember as base 
data$MJan <- ifelse(factor(data$month)=="January", 1, 0)
data$MFeb <- ifelse(factor(data$month)=="February", 1, 0)
data$MMar <- ifelse(factor(data$month)=="March", 1, 0)
data$MApr <- ifelse(factor(data$month)=="April", 1, 0)
data$MMay <- ifelse(factor(data$month)=="May", 1, 0)
data$MJun <- ifelse(factor(data$month)=="June", 1, 0)
data$MJul <- ifelse(factor(data$month)=="July", 1, 0)
data$MAug <- ifelse(factor(data$month)=="August", 1, 0)
data$MSep <- ifelse(factor(data$month)=="September", 1, 0)
data$MOct <- ifelse(factor(data$month)=="October", 1, 0)
data$MNov <- ifelse(factor(data$month)=="November", 1, 0)

data_training = data[1:2192,] #2012-2017
data_validation = data[2193:2922,]  #2018-2019
data_test = data[2923:3652,] #2020-2021

# in.sample  = training set
# out.sample = validation set
begTrain <- 1    # 2012/01/01
endTrain <- 2192    # 2017/12/31
endValid <- 2922    # 2019/12/31

in.sample  <- ts(window(SOMME, begTrain, endTrain))
out.sample <- ts(window(SOMME, endTrain+1, endValid))

data.training = cbind( data_training$HDD, data_training$CDD, data_training$CP, 
data_training$Holiday, data_training$DMon, data_training$DTue, 
data_training$DWed, data_training$DThu, data_training$DSat, data_training$DSun, 
data_training$MJan,data_training$MFeb, data_training$MMar, data_training$MApr, 
data_training$MMay, data_training$MJun, data_training$MJul, data_training$MAug, 
data_training$MSep, data_training$MOct, data_training$MNov)

data.validation = cbind(data_validation$HDD, data_validation$CDD, 
                        data_validation$CP, 
data_validation$Holiday, data_validation$DMon, data_validation$DTue, 
data_validation$DWed, data_validation$DThu, data_validation$DSat, 
data_validation$DSun, data_validation$MJan,data_validation$MFeb, 
data_validation$MMar, data_validation$MApr, data_validation$MMay,
data_validation$MJun, data_validation$MJul, data_validation$MAug, 
data_validation$MSep, data_validation$MOct, data_validation$MNov)

Yt <- timeSeries(data_training$SOMME, data_training$DATE, format="%Y-%m-%d")

#Regression 1

fit.1 <- auto.arima(Yt, xreg=cbind(data.training)) 
print(fit.1)

#Find why weekend is posing a problem 
#acf(residuals(fit.1)[-(1:2)], main=" Model 1 with arima model")

#library(astsa)
#reg.1 <- sarima(Yt, 1,1,2, xreg=cbind(data.training))

#Regression 1 predictions

pred1 <- predict(fit.1, n.ahead=1, newxreg=data.validation)

pred.model1  <- ts(pred1[["pred"]])

print(accuracy(pred.model1, out.sample)[,1:5])
#niveau d'erreur de prévision de température
#faire un entraînement annuel pour les modèles