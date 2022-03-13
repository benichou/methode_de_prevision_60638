# Program: smoothing_methods.R
#
# Purpose: adjust smoothing methods and assess their performance
# in the purpose of forecasting 
#  and special effects of week ends, stats holidays etc on 
# daily Electricity demand
#
# Written by: Team G, March 10 2022
#
# Updated: Team G, , March 13 2022
#          
#         
#          
#         
#          
# ------------------------------------------------------

# library(timeSeries)
# library(forecast)
options(digits=3)

# Training set  : 2012/01/01-2017/12/31
# Validation set: 2018/01/01-2019/12/31
# Test set      : 2019/01/01-2021/12/31

SOMME.ts <- ts(data$SOMME, start=1)

# in.sample  = training set
# out.sample = validation set
begTrain <- 1    # 2012/01/01
endTrain <- 2192    # 2017/12/31
endValid <- 2922    # 2019/12/31

in.sample  <- ts(window(SOMME.ts, begTrain, endTrain))
out.sample <- ts(window(SOMME.ts, endTrain+1, endValid))

n.out <- length(out.sample)


#==========================================================
# (1) Simple exponential Expanding Window 
# no retrain
#==========================================================
ses.opt <-ses(in.sample, h=1)
cat("SES - Optimal value of alpha: ", ses.opt$model$par[1], "\n")

ses.val <- ts(numeric(n.out)) # to store our forecasts

# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameter
for (i in 1:n.out) {
  tmp.ts <- ts(window(SOMME.ts, begTrain, endTrain+i-1))
  fc <- forecast(tmp.ts, model=ses.opt, h=1, use.initial.values=T)
  ses.val[i] <- fc$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - Simple Exponential Smoothing:","\n")
ses.eval <- rbind(accuracy(ses.opt$fitted, in.sample)[,1:5],
                  accuracy(ses.val, out.sample)[,1:5])
rownames(ses.eval) <- make.names(c("Training","Validation"))

# To report accuracy measure for the forecasts per quarters
cat("Performance - Simple Exponential Smoothing:","\n")
sesQ.eval <- rbind(
  accuracy(ses.val[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(ses.val[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(ses.val[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(ses.val[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])


rownames(sesQ.eval) <- make.names(c( 
  "Validation Q1","Validation Q2", 
  "Validation Q3", "Validation Q4"))
print(sesQ.eval)
print(ses.eval)

### SUMMARY
# # (1) Simple exponential Expanding Window 
# # no retrain

# SES - Optimal value of alpha:  0.9999

#                    ME     RMSE      MAE        MPE     MAPE
# Training     91.24825 31924.89 22984.56 -0.3606658 6.396238
# Validation -221.23165 33345.97 24307.80 -0.4193545 6.333325

#                       ME     RMSE      MAE        MPE     MAPE
# Validation.Q1 -1515.7793 41326.49 30531.12 -0.9460413 8.073912
# Validation.Q2  1834.2569 27941.83 21104.17  0.2023600 5.664004
# Validation.Q3  -549.7717 29956.77 21864.50 -0.3560993 4.829119
# Validation.Q4  -672.3893 32737.90 23835.99 -0.5853646 6.792290


#==========================================================
# (1) Simple exponential smoothing moving window
# no retrain
#==========================================================

ses1.val <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameter
for (i in 1:n.out) {
  tmp.ts <- ts(window(SOMME.ts, begTrain+i-1, endTrain+i-1))
  fcNew <- forecast(tmp.ts, model=ses.opt, h=1, use.initial.values=T)
  ses1.val[i] <- fcNew$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - Simple Exponential Smoothing:","\n")
ses1.eval <- rbind(accuracy(ses.opt$fitted, in.sample)[,1:5],
                   accuracy(ses1.val, out.sample)[,1:5])
rownames(ses1.eval) <- make.names(c("Training","Validation"))


# To report accuracy measure for the forecasts per quarters
cat("Performance - Simple Exponential Smoothing:","\n")
ses1Q.eval <- rbind(
  accuracy(ses1.val[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(ses1.val[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(ses1.val[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(ses1.val[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])


rownames(ses1Q.eval) <- make.names(c( 
  "Validation Q1","Validation Q2", 
  "Validation Q3", "Validation Q4"))

print("SES Moving Window")
print(ses1.eval)
print("SES Moving Window - Quarterly")
print(ses1Q.eval)

### SUMMARY (1) Simple exponential smoothing moving window
###  no retrain

# SES - Optimal value of alpha:  0.9999 (same fitted model
# as in expanding window)

#                    ME     RMSE      MAE        MPE     MAPE
# Training     91.24825 31924.89 22984.56 -0.3606658 6.396238
# Validation -221.23165 33345.97 24307.80 -0.4193545 6.333325

#                       ME     RMSE      MAE        MPE     MAPE
# Validation.Q1 -1515.7793 41326.49 30531.12 -0.9460413 8.073912
# Validation.Q2  1834.2569 27941.83 21104.17  0.2023600 5.664004
# Validation.Q3  -549.7717 29956.77 21864.50 -0.3560993 4.829119
# Validation.Q4  -672.3893 32737.90 23835.99 -0.5853646 6.792290

## no difference between moving and expanding window, we will
## keep the expanding window


#==========================================================
# (1) Simple exponential smoothing expanding window + RETRAIN
# EVERY YEAR
#==========================================================

ses2.val <- ts(numeric(n.out))
newEndTrainSes = endTrain
# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameter
for (i in 1:n.out) {
  if (i%%365 == 0){
    newEndTrainSes = newEndTrainSes + 365
    ses.opt = ses(ts(window(SOMME.ts, begTrain, 
                            newEndTrainSes)), h=1, 
                  initial="optimal")
  }
  tmp.ts <- ts(window(SOMME.ts, begTrain, endTrain+i-1))
  fcNew2 <- forecast(tmp.ts, model=ses.opt, h=1, 
                     use.initial.values=T)
  ses2.val[i] <- fcNew2$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - Simple Exponential Smoothing:","\n")
ses2.eval <- rbind(accuracy(ses.opt$fitted, in.sample)[,1:5],
                   accuracy(ses2.val, out.sample)[,1:5])
rownames(ses2.eval) <- make.names(c("Training","Validation"))


# To report accuracy measure for the forecasts per quarters
cat("Performance - Simple Exponential Smoothing:","\n")
ses2Q.eval <- rbind(
  accuracy(ses2.val[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(ses2.val[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(ses2.val[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(ses2.val[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])


rownames(ses2Q.eval) <- make.names(c( 
  "Validation Q1","Validation Q2", 
  "Validation Q3", "Validation Q4"))

print("SES Expanding Window Retrain")
print(ses2.eval)

print("SES Expanding Window Retrain - Quarterly")
print(ses2Q.eval)

#==========================================================
# SUMMARY (1) Simple exponential 
# smoothing expanding window + RETRAIN
# EVERY YEAR
#==========================================================

# alpha parameter on first year of validation : 0.9999 
# alpha parameter on second year of validation after retraining:
## alpha = 0.9999

#                    ME     RMSE      MAE        MPE     MAPE
# Training     91.24825 31924.89 22984.56 -0.3606658 6.396238
# Validation -221.23165 33345.97 24307.80 -0.4193545 6.333325

#                       ME     RMSE      MAE        MPE     MAPE
# Validation.Q1 -1515.7793 41326.49 30531.12 -0.9460413 8.073912
# Validation.Q2  1834.2569 27941.83 21104.17  0.2023600 5.664004
# Validation.Q3  -549.7717 29956.77 21864.50 -0.3560993 4.829119
# Validation.Q4  -672.3893 32737.90 23835.99 -0.5853646 6.792290


#==========================================================
# (2) Holt Method expanding window
#==========================================================
holt.opt <- holt(in.sample, h=1, initial="optimal")

cat("Holt - optimal value of alpha:", holt.opt$model$par[1],
    "; optimal value of beta:",holt.opt$model$par[2],"\n")

holt.val <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameters
for (i in 1:n.out) {
  tmp.ts <- ts(window(SOMME.ts, begTrain, endTrain+i-1))
  fc <- forecast(tmp.ts, model=holt.opt, h=1, use.initial.values=T)
  holt.val[i] <- fc$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - Holt Method:","\n")
holt.eval <- rbind(accuracy(holt.opt$fitted, in.sample)[,1:5],
                   accuracy(holt.val, out.sample)[,1:5])
rownames(holt.eval) <- make.names(c("Training","Validation"))
print(holt.eval)


# To report accuracy measure for the forecasts per quarters
cat("Performance - Simple Exponential Smoothing:","\n")
holtQ.eval <- rbind(
  accuracy(holt.val[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(holt.val[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(holt.val[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(holt.val[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])


rownames(holtQ.eval) <- make.names(c( 
  "Validation Q1","Validation Q2", 
  "Validation Q3", "Validation Q4"))
print(holtQ.eval)
print(holt.eval)

## SUMMARY Holt Method Expanding Window
##
# Smoothing parameters:
#     alpha = 0.9999
#     beta  = 0.0006

# ##                   ME     RMSE      MAE        MPE     MAPE
# Training   -800.8256 31949.22 23009.08 -0.6151321 6.413165
# Validation -554.1832 33361.31 24328.78 -0.5061833 6.341998

#                       ME     RMSE      MAE        MPE     MAPE
# Validation.Q1 -1859.5994 41356.17 30585.50 -1.0433585 8.092534
# Validation.Q2  1497.3229 27929.80 21064.51  0.1127491 5.657479
# Validation.Q3  -926.5884 29976.07 21881.58 -0.4364188 4.834505
# Validation.Q4  -947.1209 32755.88 23888.49 -0.6656398 6.809616


#==========================================================
# (2) Holt Method moving window
#==========================================================

holt1.val <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameters
for (i in 1:n.out) {
  tmp.ts <- ts(window(SOMME.ts, begTrain+i-1, endTrain+i-1))
  fc <- forecast(tmp.ts, model=holt.opt, h=1, use.initial.values=T)
  holt1.val[i] <- fc$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - Holt Method:","\n")
holt1.eval <- rbind(accuracy(holt.opt$fitted, in.sample)[,1:5],
                    accuracy(holt1.val, out.sample)[,1:5])
rownames(holt1.eval) <- make.names(c("Training","Validation"))
print(holt1.eval)


# To report accuracy measure for the forecasts per quarters
cat("Performance - Simple Exponential Smoothing:","\n")
holt1Q.eval <- rbind(
  accuracy(holt1.val[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(holt1.val[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(holt1.val[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(holt1.val[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])


rownames(holt1Q.eval) <- make.names(c( 
  "Validation Q1","Validation Q2", 
  "Validation Q3", "Validation Q4"))
print(holt1Q.eval)
print(holt1.eval)

## Summary moving window holt method

# Smoothing parameters:
#     alpha = 0.9999
#     beta  = 0.0006
#                   ME     RMSE      MAE        MPE     MAPE
# Training   -800.8256 31949.22 23009.08 -0.6151321 6.413165
# Validation -635.6045 33362.35 24334.16 -0.5278516 6.344043

#                      ME     RMSE      MAE         MPE     MAPE
# Validation.Q1 -1911.897 41357.29 30589.33 -1.05848977 8.094069
# Validation.Q2  1425.232 27926.63 21055.80  0.09300175 5.655903
# Validation.Q3 -1017.808 29978.15 21888.20 -0.45583473 4.836263
# Validation.Q4 -1056.568 32759.52 23908.18 -0.69785372 6.816048


#==========================================================
# (2) Holt Method expanding window with Retraining every year
#==========================================================


holt2.val <- ts(numeric(n.out))
newEndTrainHolt = endTrain
# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameters
for (i in 1:n.out) {
  if (i%%365 == 0){
    newEndTrainHolt = newEndTrainHolt + 365
    holt.opt <- holt(ts(window(SOMME.ts, begTrain, 
                               newEndTrainHolt)), h=1, 
                     initial="optimal")
    print(holt.opt$model)
  }
  tmp.ts <- ts(window(SOMME.ts, begTrain, endTrain+i-1))
  fc <- forecast(tmp.ts, model=holt.opt, h=1, use.initial.values=T)
  holt2.val[i] <- fc$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - Holt Method:","\n")
holt2.eval <- rbind(accuracy(holt.opt$fitted, in.sample)[,1:5],
                    accuracy(holt2.val, out.sample)[,1:5])
rownames(holt2.eval) <- make.names(c("Training","Validation"))
print(holt2.eval)


# To report accuracy measure for the forecasts per quarters
cat("Performance - Simple Exponential Smoothing:","\n")
holt2Q.eval <- rbind(
  accuracy(holt2.val[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(holt2.val[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(holt2.val[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(holt2.val[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])


rownames(holt2Q.eval) <- make.names(c( 
  "Validation Q1","Validation Q2", 
  "Validation Q3", "Validation Q4"))
print("Performance Holt Method Moving Window Yearly Retrain")
print(holt2Q.eval)
print(holt2.eval)
  
## SUMMARY Holt Method Expanding Window Retrain

#  Smoothing parameters (First year):
#     alpha = 0.9999
#     beta  = 0.0006

# Smoothing parameters (second year):
#     alpha = 0.9999
#     beta  = 0.0007

# Smoothing parameters (at the end of the second year):
#     alpha = 0.9999
#     beta  = 0.0006

#                       ME     RMSE      MAE        MPE     MAPE
# Validation.Q1 -1852.8571 41356.22 30585.36 -1.0414151 8.092442
# Validation.Q2  1503.8683 27930.18 21065.30  0.1146104 5.657629
# Validation.Q3  -921.2345 29976.21 21881.06 -0.4352997 4.834380
# Validation.Q4  -940.8061 32755.78 23887.50 -0.6637751 6.809296

#                   ME     RMSE      MAE        MPE     MAPE
# Training   -816.8991 31949.22 23009.24 -0.6197713 6.413403
# Validation -547.9461 33361.41 24328.56 -0.5044870 6.341901

#==========================================================
# (3) Holt-Winters Method with expanding window
#==========================================================
# in.sample with weekly seasonality
in.ts <- ts(in.sample, start=c(1,1), frequency=7)

hw.a.opt <- hw(in.ts, seasonal="additive", h=1, 
                initial="optimal")


cat("Holt-Winters Additive - optimal value of \n",
    "alpha =", hw.a.opt$model$par[1], "beta =", 
    hw.a.opt$model$par[2],
    "gamma =", hw.a.opt$model$par[3], "\n")

hw.a.val <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameters
for (i in 1:n.out) {
tmp.ts <- ts(window(SOMME.ts, begTrain, endTrain+i-1), 
                                frequency=7)
fc.a <- forecast(tmp.ts, model=hw.a.opt, h=1, use.initial.values=T)
hw.a.val[i] <- fc.a$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - Holt-Winter Method:","\n")
hw.eval <- rbind(accuracy(hw.a.opt$fitted, in.ts)[,1:5],
                accuracy(hw.a.val, out.sample)[,1:5])
rownames(hw.eval) <- make.names(c("Training    Additive",
                                "Validation  Additive"))
print(hw.eval)
hwQ.eval <- rbind(
accuracy(hw.a.val[c(1:90,366:455)], 
            out.sample[c(1:90,366:455)])[,1:5], 
accuracy(hw.a.val[c(91:181,456:547 )], 
            out.sample[c(91:181,456:547)])[,1:5],
accuracy(hw.a.val[c(182:273, 548:638)], 
            out.sample[c(182:273, 548:638)])[,1:5],
accuracy(hw.a.val[c(274:365, 639:730)], 
            out.sample[c(274:365, 639:730)])[,1:5])

rownames(hwQ.eval) <- make.names(c("ValQ1 Additive",
                                    "ValQ2  Additive",
                                    "ValQ3  Additive",
                                    "ValQ4  Additive"))
print(hwQ.eval)


## SUMMARY # (3) Holt-Winters Method with expanding window
# Holt-Winters Additive - optimal value of
#  alpha = 0.9991364 beta = 0.001803589 gamma = 0.0008635853
#                           ME     RMSE      MAE        MPE     MAPE
# Training..Additive -780.4728 29091.99 20175.91 -0.5296123 5.595588
# Validation.Additive-323.5712 31511.26 22496.61 -0.3674094 5.847076

#                         ME     RMSE      MAE        MPE     MAPE
# ValQ1.Additive  -1413.8325 39726.06 28307.70 -0.8064692 7.395648
# ValQ2..Additive  1627.4537 25952.60 19741.94  0.1854057 5.279189
# ValQ3..Additive  -723.5665 27784.82 19696.04 -0.3376096 4.360669
# ValQ4..Additive  -799.6114 30950.45 22336.89 -0.5173428 6.375300


#==========================================================
# (3) Holt-Winters Method with moving window
#==========================================================
# in.sample with weekly seasonality
in.ts <- ts(in.sample, start=c(1,1), frequency=7)

cat("Holt-Winters Additive - optimal value of \n",
    "alpha =", hw.a.opt$model$par[1], "beta =", 
               hw.a.opt$model$par[2],
    "gamma =", hw.a.opt$model$par[3], "\n")

hw.a.val1 <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameters
for (i in 1:n.out) {
tmp.ts <- ts(window(SOMME.ts, begTrain+i-1, 
                    endTrain+i-1), frequency=7)
fc.a <- forecast(tmp.ts, model=hw.a.opt, h=1, use.initial.values=T)
hw.a.val1[i] <- fc.a$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - Holt-Winter Method:","\n")
hw.eval1 <- rbind(accuracy(hw.a.opt$fitted, in.ts)[,1:5],
                accuracy(hw.a.val1, out.sample)[,1:5])
rownames(hw.eval1) <- make.names(c("Training    Additive",
                                    "Validation  Additive"))
print(hw.eval1)

hw1Q.eval <- rbind(
accuracy(hw.a.val1[c(1:90,366:455)], 
            out.sample[c(1:90,366:455)])[,1:5], 
accuracy(hw.a.val1[c(91:181,456:547 )], 
            out.sample[c(91:181,456:547)])[,1:5],
accuracy(hw.a.val1[c(182:273, 548:638)], 
            out.sample[c(182:273, 548:638)])[,1:5],
accuracy(hw.a.val1[c(274:365, 639:730)], 
            out.sample[c(274:365, 639:730)])[,1:5])

rownames(hw1Q.eval) <- make.names(c("ValQ1    Additive",
                                    "ValQ2      Additive",
                                    "ValQ3      Additive",
                                    "ValQ4      Additive"))
print(hw1Q.eval)


#==========================================================
# SUMMARY (3) Holt-Winters Method with moving window
#==========================================================
# Holt-Winters Additive - optimal value of
#  alpha = 0.9991364 beta = 0.001803589 gamma = 0.0008635853

#                        ME     RMSE      MAE        MPE     MAPE
# Training.Addive -780.4728 29091.99 20175.91 -0.5296123 5.595588
# Validation.Addive-20607.4467 38647.10 30389.67 -5.8678396 8.158434

#                         ME     RMSE      MAE       MPE      MAPE
# ValQ1...Additive   -21811.16 46273.57 37363.04 -6.783426 10.327389
# ValQ2.....Additive -18580.36 33187.52 25189.19 -5.364929  6.968536
# ValQ3.....Additive -21059.69 35880.21 27707.27 -4.756496  6.149992
# ValQ4.....Additive -20996.18 38146.77 31407.93 -6.577638  9.217590


#==========================================================
# (3) Holt-Winters Method with Expanding window + retraining
#==========================================================

# in.sample with weekly seasonality
in.ts <- ts(in.sample, start=c(1,1), frequency=7)

hw.a.opt <- hw(in.ts, seasonal="additive", 
               h=1, initial="optimal", damped=TRUE)

cat("Holt-Winters Additive - optimal value of \n",
    "alpha =", hw.a.opt$model$par[1], "beta =", 
     hw.a.opt$model$par[2],
    "gamma =", hw.a.opt$model$par[3], "\n")

hw.a.val2 <- ts(numeric(n.out))

newEndTrainHw = endTrain
# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameters
for (i in 1:n.out) {
if (i%%365 == 0){
    newEndTrainHw = newEndTrainHw + 365
    hw.a.opt = hw(ts(window(SOMME.ts, begTrain, 
                            newEndTrainHw),start=c(1,1), 
                    frequency=7), h=1, 
                seasonal = "additive",
                initial="optimal")
    print(hw.a.opt$model)
}
tmp.ts <- ts(window(SOMME.ts, begTrain, endTrain+i-1), 
            frequency=7)
fc.a <- forecast(tmp.ts, model=hw.a.opt, h=1, use.initial.values=T)
hw.a.val2[i] <- fc.a$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - Holt-Winter Method:","\n")
hw.eval2 <- rbind(accuracy(hw.a.opt$fitted, in.ts)[,1:5],
                accuracy(hw.a.val2, out.sample)[,1:5])
rownames(hw.eval2) <- make.names(c("Training    Additive",
                                    "Validation  Additive"))
print(hw.eval2)
hw2Q.eval <- rbind(
accuracy(hw.a.val2[c(1:90,366:455)], 
            out.sample[c(1:90,366:455)])[,1:5], 
accuracy(hw.a.val2[c(91:181,456:547 )], 
            out.sample[c(91:181,456:547)])[,1:5],
accuracy(hw.a.val2[c(182:273, 548:638)], 
            out.sample[c(182:273, 548:638)])[,1:5],
accuracy(hw.a.val2[c(274:365, 639:730)], 
            out.sample[c(274:365, 639:730)])[,1:5])     

rownames(hw2Q.eval) <- make.names(c("ValQ1    Additive",
                                    "ValQ2      Additive",
                                    "ValQ3      Additive",
                                    "ValQ4      Additive"))
print(hw2Q.eval)


## SUMMARY Expanding window + retraining

# before retrain (first year val)
# Holt-Winters Additive - optimal value of
#  alpha = 0.9981835 beta = 0.0001000007 gamma = 0.001816477

# after retrain (second year val)
#  Smoothing parameters:
#     alpha = 0.9989
#     beta  = 0.0017
#     gamma = 0.0011

# # at the end of the second year of val
#  Smoothing parameters:
#     alpha = 0.9989
#     beta  = 0.0016
#     gamma = 0.0011

#                         ME     RMSE      MAE        MPE     MAPE
# Training.Addive -916.7118 29103.39 20165.86 -0.5753770 5.591756
# Validation..Addite -288.9764 31469.84 22473.49 -0.3635371 5.840536

#                       ME     RMSE      MAE        MPE     MAPE
# ValQ1.Additive   -1399.6030 39674.85 28263.86 -0.8071458 7.384881
# ValQ2..Additive  1675.7906 25949.14 19764.96  0.1928933 5.280442
# ValQ3..Additive  -625.6032 27717.14 19628.23 -0.3185224 4.344797
# ValQ4...Additive  -821.7856 30910.71 22332.59 -0.5277484 6.374423

#==========================================================
# (4) Taylor Method expanding window
#==========================================================
# in.sample with double seasonality m1=7 & m2=364
# use 364 (multiple of 7) instead of 365 for the annual seasonality
in.msts <- msts(in.sample, seasonal.periods=c(7,364))

dshw.opt <- dshw(in.msts, h=1)

dshw.val <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameters
for (i in 1:n.out) {
tmp <- msts(window(SOMME.ts, begTrain, endTrain+i-1),
            seasonal.periods=c(7,364))
# refit model with smoothing parameters found in optimal model
tmp.fit <- dshw(tmp,model=dshw.opt)
dshw.val[i] <- forecast(tmp.fit, h=1)$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - Taylor Method:","\n")
dshw.eval <- rbind(accuracy(dshw.opt$fitted, in.msts)[,1:5],
                    accuracy(dshw.val, out.sample)[,1:5])
rownames(dshw.eval) <- make.names(c("Training","Validation"))
print(dshw.eval)

dshwQ.eval <- rbind(
accuracy(dshw.val[c(1:90,366:455)], 
            out.sample[c(1:90,366:455)])[,1:5], 
accuracy(dshw.val[c(91:181,456:547 )], 
            out.sample[c(91:181,456:547)])[,1:5],
accuracy(dshw.val[c(182:273, 548:638)], 
            out.sample[c(182:273, 548:638)])[,1:5],
accuracy(dshw.val[c(274:365, 639:730)], 
            out.sample[c(274:365, 639:730)])[,1:5])


rownames(dshwQ.eval) <- make.names(c("ValQ1    dshw",
                                    "ValQ2      dshw",
                                    "ValQ3      dshw",
                                    "ValQ4      dshw"))
print(dshwQ.eval)

## SUMMARY Taylor's method expanding window
#                   ME     RMSE      MAE        MPE     MAPE
# Training   -365.4403 30678.49 21249.88 -0.5646632 5.901283
# Validation -776.6971 32259.50 23613.13 -0.7537099 6.144760
#                        ME     RMSE      MAE        MPE     MAPE
# ValQ1....dshw    1569.025 40487.27 28562.31 -0.3767887 7.489569
# ValQ2......dshw -3241.065 25806.49 19900.88 -1.2733712 5.408393
# ValQ3......dshw  1367.913 29877.39 22700.14  0.0164799 4.947959
# ValQ4......dshw -2753.406 31225.30 23371.66 -1.3716041 6.751846

#==========================================================
# (4) Taylor Method moving window
#==========================================================
# in.sample with double seasonality m1=7 & m2=364
# use 364 (multiple of 7) instead of 365 for the annual seasonality
in.msts <- msts(in.sample, seasonal.periods=c(7,364))

dshw.opt <- dshw(in.msts, h=1)

dshw.val1 <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameters
for (i in 1:n.out) {
tmp <- msts(window(SOMME.ts, begTrain+i-1, endTrain+i-1),
            seasonal.periods=c(7,364))
# refit model with smoothing parameters found in optimal model
tmp.fit <- dshw(tmp,model=dshw.opt)
dshw.val1[i] <- forecast(tmp.fit, h=1)$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - Taylor Method:","\n")
dshw.eval1 <- rbind(accuracy(dshw.opt$fitted, in.msts)[,1:5],
                    accuracy(dshw.val1, out.sample)[,1:5])
rownames(dshw.eval1) <- make.names(c("Training","Validation"))
print(dshw.eval1)


dshwQ1.eval <- rbind(
accuracy(dshw.val1[c(1:90,366:455)], 
            out.sample[c(1:90,366:455)])[,1:5], 
accuracy(dshw.val1[c(91:181,456:547 )], 
            out.sample[c(91:181,456:547)])[,1:5],
accuracy(dshw.val1[c(182:273, 548:638)], 
            out.sample[c(182:273, 548:638)])[,1:5],
accuracy(dshw.val1[c(274:365, 639:730)], 
            out.sample[c(274:365, 639:730)])[,1:5])

rownames(dshwQ1.eval) <- make.names(c("ValQ1    dshw",
                                    "ValQ2      dshw",
                                    "ValQ3      dshw",
                                    "ValQ4      dshw"))
print(dshwQ1.eval)

## SUMMARY TAYLOR'S METHOD MOVING WINDOW
#
#
#                   ME     RMSE      MAE        MPE     MAPE
# Training   -365.4403 30678.49 21249.88 -0.5646632 5.901283
# Validation -459.1616 32726.39 23925.54 -0.7099595 6.230403
#                         ME     RMSE      MAE        MPE     MAPE
# ValQ1....dshw     825.3955 41491.23 29279.06 -0.5962795 7.681735
# ValQ2......dshw -2262.5039 25778.62 19786.40 -1.0303528 5.351715
# ValQ3......dshw  3101.3678 29827.73 22727.40  0.3720590 4.954249
# ValQ4......dshw -3463.4305 31925.85 23996.69 -1.5786541 6.953755

#==========================================================
# (4) Taylor Method expanding window re-training every year
#==========================================================
# in.sample with double seasonality m1=7 & m2=364
# use 364 (multiple of 7) instead of 365 for the annual seasonality
in.msts <- msts(in.sample, seasonal.periods=c(7,364))

dshw.opt <- dshw(in.msts, h=1)

dshw.val3 <- ts(numeric(n.out))
newEndTrainDshw = endTrain
# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameters
for (i in 1:n.out) {
if (i%%365 == 0){
    newEndTrainDshw = newEndTrainDshw + 365
    hw.a.opt = dshw(msts(window(SOMME.ts, begTrain, 
                                newEndTrainDshw),
                        seasonal.periods=c(7,364)
    ), h=1)
}
tmp <- msts(window(SOMME.ts, begTrain, endTrain+i-1),
            seasonal.periods=c(7,364))
# refit model with smoothing parameters found in optimal model
tmp.fit <- dshw(tmp,model=dshw.opt)
dshw.val3[i] <- forecast(tmp.fit, h=1)$mean[1]
}
  
# To report accuracy measure for the forecasts
cat("Performance - Taylor Method:","\n")
dshw.eval3 <- rbind(accuracy(dshw.opt$fitted, in.msts)[,1:5],
                    accuracy(dshw.val3, out.sample)[,1:5])
rownames(dshw.eval3) <- make.names(c("Training","Validation"))
print(dshw.eval3)
  
  
dshwQ.eval3 <- rbind(
accuracy(dshw.val3[c(1:90,366:455)], 
            out.sample[c(1:90,366:455)])[,1:5], 
accuracy(dshw.val3[c(91:181,456:547 )], 
            out.sample[c(91:181,456:547)])[,1:5],
accuracy(dshw.val3[c(182:273, 548:638)], 
            out.sample[c(182:273, 548:638)])[,1:5],
accuracy(dshw.val3[c(274:365, 639:730)], 
            out.sample[c(274:365, 639:730)])[,1:5])

rownames(dshwQ.eval3) <- make.names(c("ValQ1    dshw",
                                    "ValQ2      dshw",
                                    "ValQ3      dshw",
                                    "ValQ4      dshw"))
print(dshwQ.eval3)


## SUMMARY # (4) 
## Taylor Method expanding window re-training every year

#                   ME     RMSE      MAE        MPE     MAPE
# Training   -365.4403 30678.49 21249.88 -0.5646632 5.901283
# Validation -776.6971 32259.50 23613.13 -0.7537099 6.144760
#                        ME     RMSE      MAE        MPE     MAPE
# ValQ1....dshw    1569.025 40487.27 28562.31 -0.3767887 7.489569
# ValQ2......dshw -3241.065 25806.49 19900.88 -1.2733712 5.408393
# ValQ3......dshw  1367.913 29877.39 22700.14  0.0164799 4.947959
# ValQ4......dshw -2753.406 31225.30 23371.66 -1.3716041 6.751846

# METHODS Ranking TAKEAWAYS

# 1) Holt winters MAPE :5.840536 Expanding window + retraining
# 2) Taylor's method MAPE : 6.144760  expanding window re-training 
# 3) SES : MAPE : 6.333325 expanding window re-training
# 4) Holt's method : 6.341901 expanding window re-training

## Diebold Mariano TESTS

 ## Diebold Mariani Test
## Let us compare Holt Winters vs and Taylor's method
## and Holt Winters vs our baseline from naive methods (naive no
## change with a MAPE at 6.34)

# No major statistical difference at 5% alpha between Holt Winters
# and DSHW method

## holt winters vs taylor's (two sided)
print(dm.test((hw.a.val2-out.sample), (dshw.val3-out.sample))) 
# data:  (hw.a.val2 - out.sample)(dshw.val3 - out.sample)
# DM = -1.305, Forecast horizon = 1, Loss function power 
# = 2, p-value =
# 0.1923
# alternative hypothesis: two.sided
## holt winters vs taylor's (one sided less)
print(dm.test((hw.a.val2-out.sample), (dshw.val3-out.sample), 
      alternative="less")) 
# data:  (hw.a.val2 - out.sample)(dshw.val3 - out.sample)
# DM = -1.305, Forecast horizon = 1, Loss function power = 2, 
# p-value =
# 0.09614
# alternative hypothesis: less
  
## holt winters vs taylor's (one sided greater)
print(dm.test((hw.a.val2-out.sample), (dshw.val3-out.sample), 
      alternative="greater")) 
# data:  (hw.a.val2 - out.sample)(dshw.val3 - out.sample)
# DM = -1.305, Forecast horizon = 1, Loss function power = 2, 
# p-value =
# 0.9039
# alternative hypothesis: greater

## THIS IS THE y forecast with naive no change
## forecast_next_day
print(dm.test((hw.a.val2[c(1:729)]-out.sample[c(1:729)]), 
              (forecast_next_day-out.sample[c(1:729)])))

# DM = 12.591, Forecast horizon = 1, Loss function power = 2, 
# p-value <
# 0.00000000000000022
# alternative hypothesis: two.sided
## We can reject the null hypothesis that the quadratic average error
## on the forecast is the same between Holt Winters and naive no 
## change --> Holt Winters is our new baseline 






