# Program: smoothing_methods.R
#
# Purpose: adjust smoothing methods and assess their performance
# in the purpose of forecasting 
#  and special effects of week ends, stats holidays etc on 
# daily Electricity demand
#
# Written by: Team G, March 10 2022
#
# Updated: Team G NA
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
print("SES Expanding window")
print(ses.eval)
print("SES Moving Window")
print(ses1.eval)
print("SES Expanding Window - Quarterly")
print(sesQ.eval)
print("SES Moving Window - Quarterly")
print(ses1Q.eval)


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

print("SES Expanding Window")
print(ses.eval)
print("SES Moving Window")
print(ses1.eval)
print("SES Expanding Window Retrain")
print(ses2.eval)

print("SES Expanding Window - Quarterly")
print(sesQ.eval)
print("SES Moving Window - Quarterly")
print(ses1Q.eval)
print("SES Expanding Window Retrain - Quarterly")
print(ses2Q.eval)

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

#==========================================================
# (2) Holt Method expanding window with Retraining every year
#==========================================================

#TODO: Make sure to have a logging of the parameters during each 
## retraining

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
  
print("Performance Holt Method Moving Window")
print(holt1Q.eval)
print(holt1.eval)
  
print("Performance Holt Method Expanding Window")
print(holtQ.eval)
print(holt.eval)


#==========================================================
# (3) Holt-Winters Method with expanding window
#==========================================================
# in.sample with weekly seasonality
in.ts <- ts(in.sample, start=c(1,1), frequency=7)

hw.a.opt <- hw(in.ts, seasonal="additive", h=1, 
                initial="simple")


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
# (3) Holt-Winters Method with Expanding window + retraining
#==========================================================

# in.sample with weekly seasonality
in.ts <- ts(in.sample, start=c(1,1), frequency=7)

hw.a.opt <- hw(in.ts, seasonal="additive", h=1, initial="optimal", 
                damped=TRUE)

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

## TODO: compare the best 2 with a diebold mariano


