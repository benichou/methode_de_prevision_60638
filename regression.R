#Exploration des modèles de regression linéaire

#Chargement des données 
load('regression_df.Rdata')
load('master_df.Rdata')

#libraries
library(timeSeries)
library(forecast)
library(car)


pdf("visual_output/regression.pdf")

#data separation
beg_train <- 1 #2012/01/01
end_train <- 2192 #2017/12/31
end_valid <- 2922 #2019/12/31

rp <- 365



#-----------------------------------------------------------------
# We can start with the basic three variables of interest
# CDD , HDD and weekday
# This is just to prove that the lm models are not adequate
# Due to the nature of their errors 

model_1 <- lm( SOMME ~ noisy_CDD + noisy_HDD + factor(weekday) , 
               data = final_data , 
               subset =  seq(beg_train , end_train))

model_1_exp_1 <- lm( SOMME ~ noisy_CDD + noisy_HDD + factor(weekday) , 
                      data = final_data , 
                      subset =  seq(beg_train , end_train+rp))

model_1_mov_1 <- lm( SOMME ~ noisy_CDD + noisy_HDD + factor(weekday) , 
                     data = final_data , 
                     subset =  seq(beg_train +rp,end_train+rp))

#analysing the residuals of the model on the training data
par(mfrow=c(2,2))
plot(model_1)
mtext("Analyse des résidus du modèle 1" ,
      cex=0.98 , outer=TRUE , line = -1.4)

summary(model_1)

#residuals over time
par(mfrow=c(1,1))
plot(x = seq(1 , length(model_1$model$SOMME)) ,
     y = model_1$residuals ,
     type = 'l',
     lty = 3 ,
     xlab = 'Observation' ,
     ylab = 'Résidus')
mtext("Résidus du modèle 1 sur les données d'entrainement" ,
      cex=1.03 , outer=TRUE , line = -1.4)

#ACF of residuals
acf(residuals(model_1) ,
    main = '')
mtext("L'autocorélation du modèle 1 sur les données d'entrainement" ,
      cex=1.03 , outer=TRUE , line = -1.4)

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


plot(final_data$SOMME[(end_train+1): end_valid] , type = 'n' ,
ylim=range(pred_1_no[,2] , pred_1_no[,3]) ,
xlab = 'Période t des données de validation',
ylab = 'Demande en électricité',
main = 'Couverture du modèle 1 avec fenêtre glissante : 95%')

polygon( c(time(final_data$SOMME[(end_train+1): end_valid]), 
rev(time(final_data$SOMME[(end_train+1): end_valid]))),
c(pred_1_no[,3] , rev(pred_1_no[,2])),
col = rgb(0,0,0.6,0.2) , border = FALSE)
lines(final_data$SOMME[(end_train+1): end_valid] , lty = 1)

lines(c(pred_1_no[,1]) , col = 'red', lty = 1 )

out_p_1 <- (final_data$SOMME[(end_train+1): end_valid] < pred_1_no[,2]|
final_data$SOMME[(end_train+1): end_valid] > pred_1_no[,3] )

points(time(final_data$SOMME[(end_train+1): end_valid])[out_p_1],
final_data$SOMME[(end_train+1): end_valid][out_p_1] , pch = 19)

#-----------------------------------------------------------------
# We're going to try a similar model but with more explanatorie vars

model_1.1 <- lm( SOMME ~ noisy_CDD + noisy_HDD + noisy_cp + 
                  noisy_humidex+ before_holi+after_holiday +
                  Holiday+factor(weekday), 
               data = final_data , 
               subset =  seq(beg_train , end_train))

model_1.1_exp_1 <- lm( SOMME ~ noisy_CDD + noisy_HDD + noisy_cp + 
                      noisy_humidex+ before_holi+after_holiday + 
                      Holiday + factor(weekday), 
                      data = final_data , 
                      subset =  seq(beg_train , end_train + rp))

model_1.1_mov_1 <- lm( SOMME ~ noisy_CDD + noisy_HDD + noisy_cp + 
                      noisy_humidex+ before_holi+after_holiday + 
                      Holiday + factor(weekday), 
                      data = final_data , 
                      subset =  seq(beg_train+rp,end_train +rp))

#analysing the residuals of the model on the training data
par(mfrow=c(2,2))
plot(model_1.1)
mtext("Analyse des résidus du modèle 1.1" ,
      cex=0.98 , outer=TRUE , line = -1.4)

summary(model_1)

#residuals over time
par(mfrow=c(1,1))
plot(x = seq(1 , length(model_1.1$model$SOMME)) ,
     y = model_1.1$residuals ,
     type = 'l',
     lty = 3 ,
     xlab = 'Observation' ,
     ylab = 'Résidus')
mtext("Résidus du modèle 1.1 sur les données d'entrainement" ,
      cex=1.03 , outer=TRUE , line = -1.4)

#ACF of residuals
acf(residuals(model_1.1) ,
    main = '')
mtext("L'autocorélation du modèle 1.1 sur les données d'entrainement" ,
      cex=1.03 , outer=TRUE , line = -1.4)

#looking at the performance measures on prediction made 
#on clean covariates

#A : no retraining on all the validation
pred_1.1_no <- predict(model_1.1 , 
                     newdata = final_data[(end_train+1):end_valid,] ,
                     interval = 'prediction',
                     level = c(0.95))

#B : retraining with expending window
pred_1.1_exp_1 <- predict(model_1.1_exp_1 , 
                        newdata = final_data[(end_train+rp+1):end_valid,] ,
                        interval = 'prediction',
                        level = c(0.95))

pred_1.1_exp_1 <- rbind(pred_1.1_no[1:rp,] , pred_1.1_exp_1)

#C : moving window
pred_1.1_mov_1 <- predict(model_1.1_mov_1 ,
                        newdata = final_data[(end_train+rp+1):end_valid,],
                        interval = 'prediction',
                        level = c(0.95))

pred_1.1_mov_1 <- rbind(pred_1.1_no[1:rp,] , pred_1.1_mov_1)

#looking at the performance measures on prediction made 
#on noisy covariates


#accuracy check
accuracy(pred_1.1_no[,1] , final_data[(end_train+1):end_valid,2])
accuracy(pred_1.1_exp_1[,1] , final_data[(end_train+1):end_valid,2])
accuracy(pred_1.1_mov_1[,1] , final_data[(end_train+1):end_valid,2])

#couverture without retraining
cv_no_1.1 <- c()
for (i in seq(1, length(pred_1_no[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_1.1_no[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_1.1_no[i , 3])){
    cv_no_1.1[i] = 1
  } else {
    cv_no_1.1[i] = 0
  }
}
print(mean(cv_no_1.1))

#couverture with retraining expanding window
cv_exp_1.1 <- c()
for (i in seq(1, length(pred_1_exp_1[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_1.1_exp_1[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_1.1_exp_1[i , 3])){
    cv_exp_1.1[i] = 1
  } else {
    cv_exp_1.1[i] = 0
  }
}
print(mean(cv_exp_1.1))

#couverture with retraining moving window
cv_mov_1.1 <- c()
for (i in seq(1, length(pred_1_exp_1[,1]))){
  if ((final_data$SOMME[(end_train+i)] >= pred_1.1_mov_1[i , 2]) & 
      (final_data$SOMME[(end_train+i)] <= pred_1.1_mov_1[i , 3])){
    cv_mov_1.1[i] = 1
  } else {
    cv_mov_1.1[i] = 0
  }
}
print(mean(cv_mov_1.1))


plot(final_data$SOMME[(end_train+1): end_valid] , type = 'n' ,
     ylim=range(pred_1_no[,2] , pred_1_no[,3]) ,
     xlab = 'Période t des données de validation',
     ylab = 'Demande en électricité',
     main = 'Couverture du modèle 1 avec fenêtre glissante : 95%')

polygon( c(time(final_data$SOMME[(end_train+1): end_valid]), 
           rev(time(final_data$SOMME[(end_train+1): end_valid]))),
         c(pred_1_no[,3] , rev(pred_1_no[,2])),
         col = rgb(0,0,0.6,0.2) , border = FALSE)
lines(final_data$SOMME[(end_train+1): end_valid] , lty = 1)

lines(c(pred_1_no[,1]) , col = 'red', lty = 1 )

out_p_1 <- (final_data$SOMME[(end_train+1):end_valid]<pred_1_no[,2] |
          final_data$SOMME[(end_train+1): end_valid] > pred_1_no[,3])

points(time(final_data$SOMME[(end_train+1): end_valid])[out_p_1],
       final_data$SOMME[(end_train+1): end_valid][out_p_1] , pch = 19)

#La couvertue est supposée être de 95%. 
#Visiblement, le fait que l'erreur est autocorrélée cause 
#un problème même avec des variables explicatives sans bruits
#Ajouter du bruit ne ferait qu'empirer les résultats.
#On élimine donc ces modèles et on passe directement au erreur arma

dev.off(dev.cur())
#-----------------------------------------------------------------
#Le modèle 1.2 portera sur les mêmes variables explicatives
#que le modèle 1.1, toutefois, les erreurs seront, cette fois 
#autocorrélées dans le but d'améliorer la couverture

#ce sera un arma, pas de integrated yt 
#on va tenter d l'ordre (1,0,0)
#(2,0,0) jusqu'a max (3,0,0)

#Les variables doivent être exprimées en time series on va toutes les
#transformer ici pour un usage future

#1 - demande
yt <- timeSeries(final_data$SOMME , final_data$DATE)
yt_train <- window(yt , start = '2012-01-01' , end = '2017-12-31')
yt_valid <- window(yt , start = '2018-01-01' , end = '2019-12-31')

# - CDD et HDD avec leur bruit
cddt <- timeSeries(final_data$noisy_CDD , final_data$DATE)
cddt_train <- window(cddt , start = '2012-01-01' , end = '2017-12-31')
cddt_valid <- window(cddt , start = '2018-01-01' , end = '2019-12-31')

hddt <- timeSeries(final_data$noisy_HDD , final_data$DATE)
hddt_train <- window(hddt , start = '2012-01-01' , end = '2017-12-31')
hddt_valid <- window(hddt , start = '2018-01-01' , end = '2019-12-31')

# Refroidissement avec son bruit
cpt <- timeSeries(final_data$noisy_cp, final_data$DATE)
cpt_train <- window(cpt , start = '2012-01-01' , end = '2017-12-31')
cpt_valid <- window(cpt , start = '2018-01-01' , end = '2019-12-31')

#humidex avec son bruit egalement
humt <- timeSeries(final_data$noisy_humidex, final_data$DATE)
humt_train <- window(humt , start = '2012-01-01' , end = '2017-12-31')
humt_valid <- window(humt , start = '2018-01-01' , end = '2019-12-31')

#befor holiday
befholyt <- timeSeries(final_data$before_holi, final_data$DATE)
befholy_train <- wind_t(final_data$before_holi , set= 't')
befholy_valid <- wind_t(final_data$before_holi , set= 'v')

#after holiday
aftholyt <- timeSeries(final_data$after_holiday, final_data$DATE)
aftholy_train <- wind_t(final_data$after_holiday , set= 't')
aftholy_valid <- wind_t(final_data$after_holiday , set= 'v')

#holiday 
holyt <- timeSeries(final_data$Holiday, final_data$DATE)
holy_train <- wind_t(final_data$Holiday , set= 't')
holy_valid <- wind_t(final_data$Holiday , set= 'v')

#Dummy pour la journée de la semaine vendredi est dans 
#la référence
DMon <- ifelse(factor(final_data$weekday)=="Monday", 1, 0)
DTue <- ifelse(factor(final_data$weekday)=="Tuesday", 1, 0)
DWed <- ifelse(factor(final_data$weekday)=="Wednesday", 1, 0)
DThu <- ifelse(factor(final_data$weekday)=="Thursday", 1, 0)
DSat <- ifelse(factor(final_data$weekday)=="Saturday", 1, 0)
DSun <- ifelse(factor(final_data$weekday)=="Sunday", 1, 0)

DMont <- timeSeries(DMon , final_data$DATE)
DMont_train<-window(DMont , start = '2012-01-01' , end = '2017-12-31')
DMont_valid<-window(DMont , start = '2018-01-01' , end = '2019-12-31')

DTuet <- timeSeries(DTue , final_data$DATE)
DTuet_train<-window(DTuet , start = '2012-01-01' , end = '2017-12-31')
DTuet_valid<-window(DTuet , start = '2018-01-01' , end = '2019-12-31')

DWedt <- timeSeries(DWed , final_data$DATE)
DWedt_train<-window(DWedt , start = '2012-01-01' , end = '2017-12-31')
DWedt_valid<-window(DWedt , start = '2018-01-01' , end = '2019-12-31')

DThut <- timeSeries(DThu , final_data$DATE)
DThut_train<-window(DThut , start = '2012-01-01' , end = '2017-12-31')
DThut_valid<-window(DThut , start = '2018-01-01' , end = '2019-12-31')

DSatt <- timeSeries(DSat , final_data$DATE)
DSatt_train<-window(DSatt , start = '2012-01-01' , end = '2017-12-31')
DSatt_valid<-window(DSatt , start = '2018-01-01' , end = '2019-12-31')

DSunt <- timeSeries(DSun , final_data$DATE)
DSunt_train<-window(DSunt , start = '2012-01-01' , end = '2017-12-31')
DSunt_valid<-window(DSunt , start = '2018-01-01' , end = '2019-12-31')

wind_t <- function(ts , set) {
  
  ts <- timeSeries(ts , final_data$DATE)
  
  if (set == 't') {
    return(window(ts , start = '2012-01-01' , end ='2017-12-31'))
  } else {
    return(window(ts , start = '2018-01-01' , end = '2019-12-31'))
  }
}



#Dummy pour le mois, décembre est en référence
Djan <- ifelse(factor(data$month)=="January", 1, 0)
Djant_train <- wind_t(Djan , set ='t')
Djant_valid <- wind_t(Djan , set ='v')

Dfeb <- ifelse(factor(data$month)=="February", 1, 0)
Dfebt_train <- wind_t(Dfeb , set = 't')
Dfebt_valid <- wind_t(Dfeb , set = 'v')

Dmar <- ifelse(factor(data$month)=="March", 1, 0)
Dmart_train <- wind_t(Dmar , set = 't')
Dmart_valid <- wind_t(Dmar , set = 'v')

Dapr <- ifelse(factor(data$month)=="April", 1, 0)
Daprt_train <- wind_t(Dapr , set = 't')
Daprt_valid <- wind_t(Dapr , set = 'v')

Dmay <- ifelse(factor(data$month)=="May", 1, 0)
Dmayt_train <- wind_t(Dmay , set = 't')
Dmayt_valid <- wind_t(Dmay , set = 'v')

Djun <- ifelse(factor(data$month)=="June", 1, 0)
Djunt_train <- wind_t(Djun , set = 't')
Djunt_valid <- wind_t(Djun , set = 'v')

Djul <- ifelse(factor(data$month)=="July", 1, 0)
Djult_train <- wind_t(Djul , set = 't')
Djult_valid <- wind_t(Djul , set = 'v')

Daug <- ifelse(factor(data$month)=="August", 1, 0)
Daugt_train <- wind_t(Daug , set = 't')
Daugt_valid <- wind_t(Daug , set = 'v')

Dsep <- ifelse(factor(data$month)=="September", 1, 0)
Dsept_train <- wind_t(Dsep , set = 't')
Dsept_valid <- wind_t(Dsep , set = 'v')

Doct <- ifelse(factor(data$month)=="October", 1, 0)
Doctt_train <- wind_t(Doct , set = 't')
Doctt_valid <- wind_t(Doct , set = 'v')

Dnov <- ifelse(factor(data$month)=="December", 1, 0)
Dnovt_train <- wind_t(Dnov , set = 't')
Dnovt_valid <- wind_t(Dnov , set = 'v')

q1 <- ifelse(factor(final_data$quarter)=="Q1", 1, 0)
q1t <- timeSeries(q1 , final_data$DATE)

q2 <- ifelse(factor(final_data$quarter)=="Q2", 1, 0)
q2t <- timeSeries(q2 , final_data$DATE)

q3 <- ifelse(factor(final_data$quarter)=="Q3", 1, 0)
q3t <- timeSeries(q3 , final_data$DATE)

q4 <- ifelse(factor(final_data$quarter)=="Q4", 1, 0)
q4t <- timeSeries(q4 , final_data$DATE)

quartt <- timeSeries(final_data$quarter , final_data$DATE)
quartt_train <- wind_t(quartt , set='t')
quartt_valid <- wind_t(quartt , set='v')

#modèle 1.2 arma(3,0,0) avec les mêmes variables que 
#modèle 1.1
model_1.2 <- arima(yt_train, 
                   xreg = cbind(cddt_train , hddt_train , cpt_train,
                                humt_train , befholy_train , 
                                aftholy_train , holy_train ,
                                DMont_train , DTuet_train , 
                                DWedt_train , DThut_train ,
                                DSatt_train , DSunt_train),
                   order = c(3,0,0))



#arma does not provid fitted values
summary(model_1.2)
checkresiduals(model_1.2)

print(durbinWatsonTest(c(model_1.2$residuals),max.lag=5))


#Evaluation on the training data
pval_model_1.2 <- (1 - 
pnorm(abs(model_1.2$coef)/sqrt(diag(model_1.2$var.coef))))*2
print(pval_model_1.2)

#Evaluating the residuals
par(mfrow=c(1,1))

#Autoccorelation des residus ?
acf(residuals(model_1.2) , 
    main ='Régression avec erreurs ARMA(3,0,0)')


#Variance constante et esperance de 0 ?
plot(x = fitted(model_1.2) , y = model_1.2$residuals ,
     xlab = 'Valeurs ajustées' , ylab = 'Résidus' ,
     main = 'Diagnostic plot (model 2)')


plot(x = seq(1,length(model_1.2$residuals)),
     y = (model_1.2$residuals 
          - mean(model_1.2$residuals))/sd(model_1.2$residuals),
     xlab = 'Période' ,
     ylab = 'Résidu normalisé',
     main = 'Résidu normalisé en fonction du temps du model_1.2')

#Normal ?
qqnorm(model_1.2$residuals , main = 'Normal Q-Q')
qqline(model_1.2$residuals )

#residuals by qaurter
boxplot( (model_1.2$residuals - 
            mean(model_1.2$residuals)) / sd(model_1.2$residuals)  
         ~ final_data$quarter[beg_train: end_train] ,
         main = 'Boxplot of standardized residuals by quarter' ,
         xlab = 'Quarter' ,
         ylab = 'Standardize residuals')


#Performance in the training
accuracy(model_1.2)

#Performance sur la validation sans réentrainement
pred_1.2 <- c()

for (i in 1:nrow(yt_valid)){
  pred <- predict(model_1.2 , n.ahead = 1 , newxreg = cbind(
    cddt_valid[i] , hddt_valid[i] , cpt_valid[i],
    humt_valid[i] , befholy_valid[i] , 
    aftholy_valid[i] , holy_valid[i] ,
    DMont_valid[i] , DTuet_valid[i] , 
    DWedt_valid[i] , DThut_valid[i] ,
    DSatt_valid[i] , DSunt_valid[i]
  ))
  
  pred_1.2[i] = pred[[1]][1]
}

#print performance
accuracy(pred_1.2 , yt_valid)

#coverage
upper_1.2 <- pred_1.2 + 1.95 * sqrt(model_1.2$sigma2)
lower_1.2 <- pred_1.2 - 1.95 * sqrt(model_1.2$sigma2)
out_1.2 <- (yt_valid < lower_1.2 |yt_valid > upper_1.2 )
print(1 - mean(out_1.2))


#-----------------------------------------------------------------

#Model 1.3 trying auto.arima with the same variables

model_1.3 <- auto.arima(yt_train, 
                   xreg = cbind(cddt_train , hddt_train , cpt_train,
                                humt_train , befholy_train , 
                                aftholy_train , holy_train ,
                                DMont_train , DTuet_train , 
                                DWedt_train , DThut_train ,
                                DSatt_train , DSunt_trai))
#Residus
summary(model_1.3)
checkresiduals(model_1.3)

#Evaluation on the training data
pval_model_1.3 <- (1 - 
pnorm(abs(model_1.3$coef)/sqrt(diag(model_1.3$var.coef))))*2
print(pval_model_1.3)


#Autoccorelation des residus ?
acf(residuals(model_1.3) , 
    main ='Regression with ARIMA(1,1,2) errors')

#Variance constante et esperance de 0 ?
plot(x = fitted(model_1.3) , y = model_1.3$residuals ,
     xlab = 'Fitted values' , ylab = 'Residuals' ,
     main = 'Diagnostic plot (model 1.3)')


plot(x = seq(1,length(model_1.3$residuals)),
     y = (model_1.3$residuals 
          - mean(model_1.3$residuals))/sd(model_1.3$residuals),
     xlab = 'Période' ,
     ylab = 'Résidu normalisé',
     main = 'Résidu normalisé en fonction du temps du model_1.2')

#Normal ?
qqnorm(model_1.3$residuals , main = 'Normal Q-Q')
qqline(model_1.3$residuals )

#residuals by qaurter
boxplot( (model_1.3$residuals - 
            mean(model_1.3$residuals)) / sd(model_1.3$residuals)  
         ~ final_data$quarter[beg_train: end_train] ,
         main = 'Boxplot des résidus normalisé du modèle 1.3 par semestre' ,
         xlab = 'Semestre' ,
         ylab = 'Résidus par semestre')

#Performance in the training
accuracy(model_1.3)

#Performance sur la validation sans réentrainement
pred_1.3 <- c()

for (i in 1:nrow(yt_valid)){
  pred <- predict(model_1.3 , n.ahead = 1 , newxreg = cbind(
    cddt_valid[i] , hddt_valid[i] , cpt_valid[i],
    humt_valid[i] , befholy_valid[i] , 
    aftholy_valid[i] , holy_valid[i] ,
    DMont_valid[i] , DTuet_valid[i] , 
    DWedt_valid[i] , DThut_valid[i] ,
    DSatt_valid[i] , DSunt_valid[i]
  ))
  
  pred_1.3[i] = pred[[1]][1]
}

#print performance
accuracy(pred_1.3 , yt_valid)

#coverage
upper_1.3 <- pred_1.3 + 1.95 * sqrt(model_1.3$sigma2)
lower_1.3 <- pred_1.3 - 1.95 * sqrt(model_1.3$sigma2)
out_1.3 <- (yt_valid < lower_1.3 |yt_valid > upper_1.3 )
print(1 - mean(out_1.3))

#-----------------------------------------------------------------
#model 1.4
#On va essayer d'introduire les mois et les lag dans ce modèles
#On a vu que la distributions des erreurs par semestres n'était
#tout à fait équivalente
#L'humidité est enlevée, elle n'a pas d'effet fort
# day , month , cdd , hdd , cp ,

model_1.4 <- auto.arima(yt_train, 
xreg = cbind(cddt_train , lag(cddt_train ,1), hddt_train , 
lag(hddt_train ,1), 
cpt_train, befholy_train , aftholy_train , holy_train ,
DMont_train , DTuet_train , DWedt_train , DThut_train ,
DSatt_train , DSunt_train ,Djant_train , Dfebt_train ,
Dmart_train , Daprt_train , Dmayt_train , Djunt_train,
Djult_train , Daugt_train , Dsept_train , Doctt_train,
Dnovt_train ))

#Residus
summary(model_1.4)
checkresiduals(model_1.4)

#Evaluation on the training data
pval_model_1.4 <- (1 - 
pnorm(abs(model_1.4$coef)/sqrt(diag(model_1.3$var.coef))))*2
print(pval_model_1.4)


#Autoccorelation des residus ?
acf(residuals(model_1.4)[2:end_train] , 
    main ='Regression with ARIMA(5,1,0) errors')

#Variance constante et esperance de 0 ?
plot(x = fitted(model_1.4) , y = model_1.4$residuals ,
     xlab = 'Fitted values' , ylab = 'Residuals' ,
     main = 'Diagnostic plot (model 1.4)')


plot(x = seq(1,length(model_1.4$residuals[2:end_train])),
     y = (model_1.4$residuals[2:end_train] 
          - mean(model_1.4$residuals[2:end_train]))/
       sd(model_1.4$residuals[2:end_train]),
     xlab = 'Période' ,
     ylab = 'Résidu normalisé',
     main = 'Résidu normalisé en fonction du temps du model_1.4')

#Normal ?
qqnorm(model_1.3$residuals[2:end_train] , main = 'Normal Q-Q')
qqline(model_1.3$residuals[2:end_train] )

#residuals by qaurter
boxplot( (model_1.4$residuals[2:end_train] - 
            mean(model_1.4$residuals[2:end_train])) / 
           sd(model_1.4$residuals[2:end_train])  
         ~ final_data$quarter[2: end_train] ,
         main = 'Boxplot des résidus normalisé du modèle 1.4 par semestre' ,
         xlab = 'Semestre' ,
         ylab = 'Résidus par semestre')

#Performance in the training
accuracy(model_1.4)

#Performance sur la validation sans réentrainement
pred_1.4 <- c()

for (i in 2:nrow(yt_valid)){
  pred <- predict(model_1.4 , n.ahead = 1 , newxreg = cbind(
    cddt_valid[i] , cddt_valid[i-1], hddt_valid[i] , 
    hddt_valid[i-1], cpt_valid[i], befholy_valid[i] , aftholy_valid[i]
    ,holy_valid[i] , DMont_valid[i] , DTuet_valid[i] , DWedt_valid[i] , 
    DThut_valid[i]
    , DSatt_valid[i] , DSunt_valid[i] ,Djant_valid[i] , Dfebt_valid[i],
    Dmart_valid[i] , Daprt_valid[i] , Dmayt_valid[i] , Djunt_valid[i],
    Djult_valid[i] , Daugt_valid[i] , Dsept_valid[i] , Doctt_valid[i],
    Dnovt_train[i] 
  ))
  pred_1.4[i] = pred[[1]][1]
}

#print performance
accuracy(pred_1.4[2:730] , yt_valid[2:730])

#coverage
upper_1.4 <- pred_1.4[2:730] + 1.95 * sqrt(model_1.4$sigma2)
lower_1.4 <- pred_1.4[2:730] - 1.95 * sqrt(model_1.4$sigma2)
out_1.4 <- (yt_valid[2:730] < lower_1.4 |yt_valid[2:730]> upper_1.4 )
print(1 - mean(out_1.4))
#-------------------------------------------------------------------
# #The arima models seem to all over estimate the demand and byt 
# #quit an important marging 
# #out startegy to correct this is to make a model that retrains
# #at each iteration for the whole validation period
# #it will only take a portion of the training
# 
# #new split
# nt = 1096
# end_t = 2191
# 
# 
# pred_p <- c()
# pred_upr <- c()
# pred_lwr <- c()
# 
# for (i in 1:nrow(yt_valid)){
#   #fit a model using ARIMA(1,1,2)
#   s = nt + i
#   e = end_t + i
#   model_t <- arima( yt[s:e], xreg = cbind(
#     cddt[s:e] , hddt[s:e],
#     cpt[s:e] , befholyt[s:e] , aftholyt[s:e] , holyt[s:e] , 
#     q1t[s:e] , q2t[s:e] , q3t[s:e],
#     DMont[s:e] , DTuet[s:e] , DWedt[s:e] , DThut[s:e] , DSatt[s:e],
#     DSunt[s:e]
#   ), order = c(1,1,2))
#   
#   j = e + 1
#   pred <- predict(model_t , n.ahead = 1 , newxreg = cbind(
#     cddt[j] , hddt[j] ,cpt[j] , befholyt[j] , aftholyt[j] , holyt[j], 
#     q1t[j] , q2t[j] , q3t[j],
#     DMont[j] , DTuet[j] , DWedt[j] , DThut[j] , DSatt[j],
#     DSunt[j]
#   ))
#   
#   pred_p[i] <- pred[[1]][1]
#   pred_upr[i] <- pred_p[i] + 1.96 * sqrt(model_t$sigma2)
#   pred_lwr[i] <- pred_p[i] - 1.96 * sqrt(model_t$sigma2)
#   print(i)
# }
# 
# #analyse des residus
# accuracy(model_t)
# 
# #acf
# acf(residuals(model_t) , 
#     main ='Regression with ARIMA(5,1,0) errors')
# 
# #Variance constante et esperance de 0 ?
# plot(x = fitted(model_t) , y = model_t$residuals ,
#      xlab = 'Fitted values' , ylab = 'Residuals' ,
#      main = 'Diagnostic plot (model t)')
# 
# 
# plot(x = seq(1,length(model_t$residuals)),
#      y = (model_t$residuals 
#           - mean(model_t$residuals))/
#        sd(model_t$residuals),
#      xlab = 'Période' ,
#      ylab = 'Résidu normalisé',
#      main = 'Résidu normalisé en fonction du temps du model_t')
# 
# #Normal ?
# qqnorm(model_t$residuals , main = 'Normal Q-Q')
# qqline(model_t$residuals )
# 
# #residuals by qaurter
# boxplot( (model_t$residuals - 
#             mean(model_t$residuals)) / 
#            sd(model_t$residuals)  
#          ~ final_data$quarter[s: e] ,
#          main = 'Boxplot des résidus normalisé du modèle t par semestre' ,
#          xlab = 'Semestre' ,
#          ylab = 'Résidus par semestre')
# 
# plot(yt ,
#      ylab = 'Somme de la Demande journalière (MW))',
#      main = 'Évolution de la demande journalière à travers le temps')
# 
# #performance on the validation set using moving windown
# accuracy(pred_p , yt_valid)
# 
# #couvreture
# out_t <- (yt_valid < pred_lwr | yt_valid> pred_upr )
# print(1 - mean(out_t))



#-------------------------------------------------------------------
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