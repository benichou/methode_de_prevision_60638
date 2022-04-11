# Program: regression_correction.R
#
# Purpose: Adjusts the regression models both manually and 
## automatically with moving and expanding window and with or without
## retraining to minimize the forecast error
# 
#
# Written by: Team G, April 7th 2022
#
# Updated: Team G, April 10th 2022
#          
#         
#          
#         
#          
# ------------------------------------------------------




source('timeseries_x.R')

#Ce script a pour but de corriger les erreurs en lien avec le
#mod??le de regression de la phase 2

#La performance ??tait tr??s mauvais ??tant donn??e qu'on n'avait pas
#Inclut la variable temps on va essayer avec et voir si ca fait 
#une difference


#The arima models seem over estimate the demand and with
#quit an important marging
#our startegy to correct this is to make a model that retrains
#at each iteration for the whole validation period
#it will only take a portion of the training
b_train = 1
e_train = 2192

#Without noise
xreg_nn = cbind(
  cddt_nn , hddt_nn , lag(cddt_nn , 1) , 
  lag(hddt_nn ,1) , lag(cddt_nn , 2) , 
  lag(hddt_nn , 2) ,
  DMont , 
  DTuet , DWedt , DThut ,
  DSatt , DSunt ,
  humt , cpt ,
  holyt, befholyt , aftholyt ,
  Djan , Dfeb ,
  Dmar , Dapr, Dmay , Djun,
  Djul , Daug , Dsep , Doct,
  Dnov 
  )

#with noise
xreg = cbind(
  cddt , hddt , lag(cddt , 1) , 
  lag(hddt ,1) , lag(cddt , 2) , 
  lag(hddt , 2) ,
  DMont , 
  DTuet , DWedt , DThut ,
  DSatt , DSunt ,
  humt , cpt , holyt,
  befholyt , aftholyt ,
  Djan , Dfeb ,
  Dmar , Dapr, Dmay , Djun,
  Djul , Daug , Dsep , Doct,
  Dnov )

#No retraining all training period
model_nr <- arima(
  yt[b_train +2  : e_train ] ,
  xreg = xreg[b_train +2 :e_train , ] ,
  order = c(5 , 0 , 2)
)

summary(model_nr)
checkresiduals(model_nr)
qqnorm(model_nr$residuals , main = 'Normal Q-Q')
qqline(model_nr$residuals)

#Performance sur la validation sans reentrainement , AVEC NOISE
pred_nr <- c()

for (i in 1:nrow(yt_valid)){
  pred <- predict(model_nr , n.ahead = 1 , 
                  newxreg = xreg[e_train + i ,  ]
  )
  
  pred_nr[i] = pred[[1]][1]
}

print("Performance avec noise train since day 1")
accuracy(pred_nr , yt_valid)

#coverage
upper_nr <- pred_nr + 1.96 * sqrt(model_nr$sigma2)
lower_nr <- pred_nr - 1.96 * sqrt(model_nr$sigma2)
out_nr <- (yt_valid < lower_nr |yt_valid> upper_nr)
print(1 - mean(out_nr))

#Performance sur la validation sans reentrainement , SANS NOISE
pred_nr_nn <- c()

for (i in 1:nrow(yt_valid)){
  pred <- predict(model_nr , n.ahead = 1 , 
                  newxreg = xreg_nn[e_train + i ,  ]
  )
  
  pred_nr_nn[i] = pred[[1]][1]
}

print("Performance sans noise train since day 1")
accuracy(pred_nr_nn , yt_valid)

#coverage
upper_nr_noise <- pred_nr_nn + 1.96 * sqrt(model_nr$sigma2)
lower_nr_noise <- pred_nr_nn - 1.96 * sqrt(model_nr$sigma2)
out_nr_noise <- (yt_valid < lower_nr_noise |yt_valid> upper_nr_noise)
print(1 - mean(out_nr_noise))

#no retraining , training set starting 2015 -> 1096
b_train = 1096

model_nr_2 <- arima(
  yt[b_train +2  : e_train ] ,
  xreg = xreg[b_train +2 :e_train , ] ,
  order = c(5 , 0 , 2)
)

#validity
summary(model_nr_2)
checkresiduals(model_nr_2)
qqnorm(model_nr_2$residuals , main = 'Normal Q-Q')
qqline(model_nr_2$residuals)


#Performance sur la validation sans reentrainement , AVEC NOISE
pred_nr_2 <- c()

for (i in 1:nrow(yt_valid)){
  pred <- predict(model_nr_2 , n.ahead = 1 , 
                  newxreg = xreg[e_train + i ,  ]
  )
  
  pred_nr_2[i] = pred[[1]][1]
}

print("Performance avec noise et train from 2015")
accuracy(pred_nr_2 , yt_valid)

#coverage
upper_nr_2 <- pred_nr_2 + 1.96 * sqrt(model_nr$sigma2)
lower_nr_2 <- pred_nr_2 - 1.96 * sqrt(model_nr$sigma2)
out_nr_2 <- (yt_valid < lower_nr_2 |yt_valid> upper_nr_2)
print(1 - mean(out_nr_2))

#Performance sur la validation sans reentrainement , SANS NOISE
pred_nr_2_nn <- c()

for (i in 1:nrow(yt_valid)){
  pred <- predict(model_nr_2 , n.ahead = 1 , 
                  newxreg = xreg_nn[e_train + i ,  ]
  )
  
  pred_nr_2_nn[i] = pred[[1]][1]
}

print("Performance SANS NOISE et train from 2015")
accuracy(pred_nr_2_nn , yt_valid)

#coverage
upper_nr_2_nn <- pred_nr_2_nn + 1.96 * sqrt(model_nr$sigma2)
lower_nr_2_nn <- pred_nr_2_nn - 1.96 * sqrt(model_nr$sigma2)
out_nr_2_nn <- (yt_valid < lower_nr_2_nn |yt_valid> upper_nr_2_nn)
print(1 - mean(out_nr_2_nn))

#model expanding window on retrain 1 fois

b_train = 0
e_train = 2191

#with noise
pred_r_1 <- c()
pred_r_1_upr <- c()
pred_r_1_lwr <- c()
pred_r_1_upr_0.8 <- c()
pred_r_1_lwr_0.8 <- c()

#without noise
pred_r_1_nn <- c()
pred_r_1_upr_nn <- c()
pred_r_1_lwr_nn <- c()
pred_r_1_upr_0.8_nn <- c()
pred_r_1_lwr_0.8_nn <- c()

for (i in 1:nrow(yt_valid)){
  #we have lag 2 data so we cant start at 1
  s = b_train + 2 + i
  e = e_train + i
  
  if(s %in% c(3 , 368)) {
  model_r <- arima(
    yt[s:e] , xreg = xreg[s:e , ] ,
    order = c(5 , 0 , 2)
  )}

  j = e + 1
  pred <- predict(model_r , n.ahead = 1 , 
                  newxreg = xreg[j ,  ]
  )

  pred_r_1[i] <- pred[[1]][1]
  pred_r_1_upr[i] <- pred_r_1[i] + 1.96 * sqrt(model_r$sigma2)
  pred_r_1_lwr[i] <- pred_r_1[i] - 1.96 * sqrt(model_r$sigma2)
  pred_r_1_upr_0.8[i] <- pred_r_1[i] + 1.28 * sqrt(model_r$sigma2)
  pred_r_1_lwr_0.8[i] <- pred_r_1[i] - 1.28 * sqrt(model_r$sigma2)


  pred_nn <- predict(model_r , n.ahead = 1 , 
                  newxreg = xreg_nn[j ,  ]
  )

  pred_r_1_nn[i] <- pred_nn[[1]][1]
  pred_r_1_upr_nn[i] <- pred_r_1_nn[i] + 1.96 * sqrt(model_r$sigma2)
  pred_r_1_lwr_nn[i] <- pred_r_1_nn[i] - 1.96 * sqrt(model_r$sigma2)
  pred_r_1_upr_0.8_nn[i] <- pred_r_1_nn[i] + 
    1.28 * sqrt(model_r$sigma2)
  pred_r_1_lwr_0.8_nn[i] <- pred_r_1_nn[i] - 
    1.28 * sqrt(model_r$sigma2)

  print(i)
  print(j)
}


#performance with noise
print("Performance WITH et 1 RETRAIN")
accuracy(pred_r_1 , yt_valid)

#coverage with noise
print("coverage 0.95")
out_r_1 <- (yt_valid < pred_r_1_lwr |yt_valid> pred_r_1_upr)
print(1 - mean(out_r_1))

print("coverage 0.8")
out_r_1_0.8 <- (yt_valid < pred_r_1_lwr_0.8 |
                  yt_valid> pred_r_1_upr_0.8)
print(1 - mean(out_r_1_0.8))



#performance without noise
print("Performance WITHOUT et 1 RETRAIN")
accuracy(pred_r_1_nn , yt_valid)

#coverage without
print("coverage 0.95")
out_r_1_nn <- (yt_valid < pred_r_1_lwr_nn |yt_valid> pred_r_1_upr_nn)
print(1 - mean(out_r_1_nn))

#coverage without
print("coverage 0.8")
out_r_1_0.8_nn <- (yt_valid < pred_r_1_lwr_0.8_nn |
                     yt_valid> pred_r_1_upr_0.8_nn)
print(1 - mean(out_r_1_0.8_nn))

#-----------------------------------------------------------------
#The code below is for the best model
#daily retrain

#with noise
pred_r_2 <- c()
pred_r_2_upr <- c()
pred_r_2_lwr <- c()
pred_r_2_upr_0.8 <- c()
pred_r_2_lwr_0.8 <- c()

#without noise
pred_r_2_nn <- c()
pred_r_2_upr_nn <- c()
pred_r_2_lwr_nn <- c()
pred_r_2_upr_0.8_nn <- c()
pred_r_2_lwr_0.8_nn <- c()

for (i in 1:nrow(yt_valid)){
  #we have lag 2 data so we cant start at 1
  s = b_train + 2 + i
  e = e_train + i
  
  model_r_2 <- arima(
    yt[s:e] , xreg = xreg[s:e , ] ,
    order = c(5 , 0 , 2))
  
  j = e + 1
  pred <- predict(model_r_2 , n.ahead = 1 , 
                  newxreg = xreg[j ,  ]
  )
  
  pred_r_2[i] <- pred[[1]][1]
  pred_r_2_upr[i] <- pred_r_2[i] + 1.96 * sqrt(model_r_2$sigma2)
  pred_r_2_lwr[i] <- pred_r_2[i] - 1.96 * sqrt(model_r_2$sigma2)
  pred_r_2_upr_0.8[i] <- pred_r_2[i] + 1.28 * sqrt(model_r_2$sigma2)
  pred_r_2_lwr_0.8[i] <- pred_r_2[i] - 1.28 * sqrt(model_r_2$sigma2)
  
  
  pred_nn <- predict(model_r_2 , n.ahead = 1 , 
                     newxreg = xreg_nn[j ,  ]
  )
  
  pred_r_2_nn[i] <- pred_nn[[1]][1]
  pred_r_2_upr_nn[i] <- pred_r_2_nn[i] + 1.96 * sqrt(model_r_2$sigma2)
  pred_r_2_lwr_nn[i] <- pred_r_2_nn[i] - 1.96 * sqrt(model_r_2$sigma2)
  pred_r_2_upr_0.8_nn[i] <- pred_r_2_nn[i] + 
    1.28 * sqrt(model_r_2$sigma2)
  pred_r_2_lwr_0.8_nn[i] <- pred_r_2_nn[i] - 
    1.28 * sqrt(model_r_2$sigma2)
  
  print(i)
  print(j)
}



# Les vecteurs qui t'interesse pour le rapport sont :
# pred_r_2 , pred_r_2_lwr , pred_r_2_upr ,pred_r_2_lwr_0.8 ,
# pred_r_2_upr_0.8

#performance with noise
print("Performance WITH et 1 RETRAIN")
accuracy(pred_r_2 , yt_valid)

# Results for Franck
# ME    RMSE      MAE       MPE     MAPE
# Test set 1303.982 20976.1 15921.74 0.1111377 4.108066


#coverage with noise
print("coverage 0.95")
out_r_2 <- (yt_valid < pred_r_2_lwr |yt_valid> pred_r_2_upr)
print(1 - mean(out_r_2))

# Result 0.95
# 0.8452055

print("coverage 0.8")
out_r_2_0.8 <- (yt_valid < pred_r_2_lwr_0.8 |
                  yt_valid> pred_r_2_upr_0.8)
print(1 - mean(out_r_2_0.8))

# Result 0.8 
# 0.6753425


#performance without noise
print("Performance WITHOUT et 1 RETRAIN")
accuracy(pred_r_2_nn , yt_valid)

#coverage without
print("coverage 0.95")
out_r_2_nn <- (yt_valid < pred_r_2_lwr_nn |yt_valid> pred_r_2_upr_nn)
print(1 - mean(out_r_2_nn))

#coverage without
print("coverage 0.8")
out_r_2_0.8_nn <- (yt_valid < pred_r_2_lwr_0.8_nn |
                     yt_valid> pred_r_2_upr_0.8_nn)
print(1 - mean(out_r_2_0.8_nn))

#??valuation par trimestre sur la validation
df_1 <- data.frame(yt_valid , 
                                  pred_r_2 , 
                                  pred_r_2_lwr ,
                                  pred_r_2_upr ,
                                  pred_r_2_lwr_0.8 ,
                                  pred_r_2_upr_0.8 ,
                                  final_data$quarter[(e-729) : e]
                                  )

colnames(df_1) <- c("obs",
                             "pred" , 
                             "lwr_0.95",
                             "upr_0.95",
                             "lwr_0.8" ,
                             "upr_0.8" ,
                             "quart")

print("Performance for quarter 1")
q1_filter <- which(df_1$quart == "Q1")
q2_filter <- which(df_1$quart == "Q2")
q3_filter <- which(df_1$quart == "Q3")
q4_filter <- which(df_1$quart == "Q4")

#MAPE
mapes <- c(accuracy(df_1[q1_filter , 1] , 
                    df_1[q1_filter , 2])[5] ,
           accuracy(df_1[q2_filter , 1] , 
                    df_1[q2_filter , 2])[5] ,
           accuracy(df_1[q3_filter , 1] , 
                    df_1[q3_filter , 2])[5] ,
           accuracy(df_1[q4_filter , 1] , 
                    df_1[q4_filter , 2])[5])
print(mapes)
cv_0.95 <-c(
  ( 1 - mean(df_1[q1_filter , 1]  < df_1[q1_filter, 3] |
            df_1[q1_filter , 1]  > df_1[q1_filter, 4])) ,
    
    1 - mean(df_1[q2_filter , 1]  < df_1[q2_filter, 3] |
             df_1[q2_filter , 1]  > df_1[q2_filter, 4]) ,
    
    1 - mean(df_1[q3_filter , 1]  < df_1[q3_filter, 3] |
             df_1[q3_filter , 1]  > df_1[q3_filter, 4]) ,
    
    1 - mean(df_1[q4_filter , 1]  < df_1[q4_filter, 3] |
             df_1[q4_filter , 1]  > df_1[q4_filter, 4])
  )

cv_0.80 <-c(
  ( 1 - mean(df_1[q1_filter , 1]  < df_1[q1_filter, 5] |
               df_1[q1_filter , 1]  > df_1[q1_filter, 6])) ,
  
  1 - mean(df_1[q2_filter , 1]  < df_1[q2_filter, 5] |
             df_1[q2_filter , 1]  > df_1[q2_filter, 6]) ,
  
  1 - mean(df_1[q3_filter , 1]  < df_1[q3_filter, 5] |
             df_1[q3_filter , 1]  > df_1[q3_filter, 6]) ,
  
  1 - mean(df_1[q4_filter , 1]  < df_1[q4_filter, 5] |
             df_1[q4_filter , 1]  > df_1[q4_filter, 6])
)

print(cv_0.80)

#Table performance 
table_1 <- data.frame(mapes , cv_0.95 , cv_0.80 )
rownames(table_1) <- c('Q1' , 'Q2' , 'Q3' , 'Q4')

print(table_1)

# Results for Franck
# mapes   cv_0.95   cv_0.80
# Q1 4.331857 0.8500000 0.7111111
# Q2 4.261618 0.8241758 0.6263736
# Q3 4.016730 0.7880435 0.5760870
# Q4 3.829962 0.9184783 0.7880435


#-------------------------------------------------------------------
#Sur le test set

pred_test <- c()
pred_test_upr <- c()
pred_test_lwr <- c()
pred_test_upr_0.8 <- c()
pred_test_lwr_0.8 <- c()



for (i in 1:730){
  #we have lag 2 data so we cant start at 1
  s =  731 + i
  e = 2921 + i
  
  model_r_2 <- arima(
    yt[s:e] , xreg = xreg[s:e , ] ,
    order = c(5 , 0 , 2))
  
  j = e + 1
  pred <- predict(model_r_2 , n.ahead = 1 , 
                  newxreg = xreg[j ,  ]
  )
  
  pred_test[i] <- pred[[1]][1]
  pred_test_upr[i] <- pred_test[i] + 1.96 * sqrt(model_r_2$sigma2)
  pred_test_lwr[i] <- pred_test[i] - 1.96 * sqrt(model_r_2$sigma2)
  pred_test_upr_0.8[i] <- pred_test[i] + 1.28 * sqrt(model_r_2$sigma2)
  pred_test_lwr_0.8[i] <- pred_test[i] - 1.28 * sqrt(model_r_2$sigma2)
  
  print(i)
  print(j)
}



# Les vecteurs qui t'interesse pour le rapport sont :
# pred_r_2 , pred_r_2_lwr , pred_r_2_upr ,pred_r_2_lwr_0.8 ,
# pred_r_2_upr_0.8

yt_test <- final_data$SOMME[2923:3652]
#performance with noise
print("Performance WITH et 1 RETRAIN")
accuracy(pred_test , yt_test )

# Results for Franck
# ME     RMSE      MAE        MPE     MAPE
# Test set 1229.67 20296.71 15039.73 0.09132924 3.946035


#coverage with noise
print("coverage 0.95")
out_test <- (yt_test < pred_test_lwr |yt_test> pred_test_upr)
print(1 - mean(out_test))

# Result 0.95
# 0.9068493

print("coverage 0.8")
out_test_0.8 <- (yt_test < pred_test_lwr_0.8 |
                  yt_test> pred_test_upr_0.8)
print(1 - mean(out_test_0.8))

# Result 0.8 
# 0.7534247

#??valuation par trimestre sur la validation
df_2 <- data.frame(yt_test , 
                   pred_test , 
                   pred_test_lwr ,
                   pred_test_upr ,
                   pred_test_lwr_0.8 ,
                   pred_test_upr_0.8 ,
                   final_data$quarter[(e-729) : e]
)

colnames(df_2) <- c("obs",
                    "pred" , 
                    "lwr_0.95",
                    "upr_0.95",
                    "lwr_0.8" ,
                    "upr_0.8" ,
                    "quart")

print("Performance for quarter 1")
q1_filter <- which(df_2$quart == "Q1")
q2_filter <- which(df_2$quart == "Q2")
q3_filter <- which(df_2$quart == "Q3")
q4_filter <- which(df_2$quart == "Q4")

#MAPE
mapes <- c(accuracy(df_2[q1_filter , 1] , 
                    df_2[q1_filter , 2])[5] ,
           accuracy(df_2[q2_filter , 1] , 
                    df_2[q2_filter , 2])[5] ,
           accuracy(df_2[q3_filter , 1] , 
                    df_2[q3_filter , 2])[5] ,
           accuracy(df_2[q4_filter , 1] , 
                    df_2[q4_filter , 2])[5])
print(mapes)
cv_0.95 <-c(
  ( 1 - mean(df_2[q1_filter , 1]  < df_2[q1_filter, 3] |
               df_2[q1_filter , 1]  > df_2[q1_filter, 4])) ,

  1 - mean(df_2[q2_filter , 1]  < df_2[q2_filter, 3] |
             df_2[q2_filter , 1]  > df_2[q2_filter, 4]) ,
  
  1 - mean(df_2[q3_filter , 1]  < df_2[q3_filter, 3] |
             df_2[q3_filter , 1]  > df_2[q3_filter, 4]) ,
  
  1 - mean(df_2[q4_filter , 1]  < df_2[q4_filter, 3] |
             df_2[q4_filter , 1]  > df_2[q4_filter, 4])
)

cv_0.80 <-c(
  ( 1 - mean(df_2[q1_filter , 1]  < df_2[q1_filter, 5] |
               df_2[q1_filter , 1]  > df_2[q1_filter, 6])) ,
  
  1 - mean(df_2[q2_filter , 1]  < df_2[q2_filter, 5] |
             df_2[q2_filter , 1]  > df_2[q2_filter, 6]) ,
  
  1 - mean(df_2[q3_filter , 1]  < df_2[q3_filter, 5] |
             df_2[q3_filter , 1]  > df_2[q3_filter, 6]) ,
  
  1 - mean(df_2[q4_filter , 1]  < df_2[q4_filter, 5] |
             df_2[q4_filter , 1]  > df_2[q4_filter, 6])
)

print(cv_0.80)

#Table performance 
table_2 <- data.frame(mapes , cv_0.95 , cv_0.80 )
rownames(table_2) <- c('Q1' , 'Q2' , 'Q3' , 'Q4')

print(table_2)

#results
# mapes   cv_0.95   cv_0.80
# Q1 3.740571 0.9116022 0.7845304
# Q2 4.292909 0.8791209 0.7142857
# Q3 3.824086 0.9076087 0.6956522
# Q4 3.911163 0.9289617 0.8196721

#------------------------------------------------------
#results

# Validation 
# Results for Franck 
#               ME    RMSE      MAE       MPE     MAPE
# Test set 1303.982 20976.1 15921.74 0.1111377 4.108066

# Couverture 
#  0.95
# 0.8452055

# Result 0.8 
# 0.6753425

# Results for Franck
#       mapes   cv_0.95   cv_0.80
# Q1 4.331857 0.8500000 0.7111111
# Q2 4.261618 0.8241758 0.6263736
# Q3 4.016730 0.7880435 0.5760870
# Q4 3.829962 0.9184783 0.7880435

#Test set
# Results for Franck
#             ME     RMSE      MAE        MPE     MAPE
# Test set 1229.67 20296.71 15039.73 0.09132924 3.946035

# Couverture
# Result 0.95
# 0.9068493

# Result 0.8 
# 0.7534247
#results
#       mapes   cv_0.95   cv_0.80
# Q1 3.740571 0.9116022 0.7845304
# Q2 4.292909 0.8791209 0.7142857
# Q3 3.824086 0.9076087 0.6956522
# Q4 3.911163 0.9289617 0.8196721

#changing the name
armamodel_valid <- c(pred_r_2)
armamodel_test <- c(pred_test)


save(armamodel_valid, file = "armamodel_valid.Rdata")
save(armamodel_test, file = "armamodel_test.Rdata")

