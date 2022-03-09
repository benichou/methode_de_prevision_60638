# Program: smoothing_methods.R
#
# Purpose: adjust smoothing methods and assess their performance
# in the purpose of forecasting 
#  and special effects of week ends, stats holidays etc on 
# daily Electricity demand
#
# Written by: Team G, January 30 2022
#
# Updated: Team G, Feb 10th 2022
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
## keep the moving window


#==========================================================
# (1) Simple exponential smoothing moving window + RETRAIN
# EVERY YEAR
#==========================================================

ses2.val <- ts(numeric(n.out))
newEndTrainSes = endTrain
newBegTrain = begTrain
# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameter
for (i in 1:n.out) {
  if (i%%365 == 0){
    print("ooo")
    newEndTrainSes = newEndTrainSes + 365
    newBegTrain = newBegTrain + 365
    ses.opt = ses(ts(window(SOMME.ts, newBegTrain, 
                            newEndTrainSes)), h=1, 
                  initial="optimal")
  }
  tmp.ts <- ts(window(SOMME.ts, begTrain+i-1, endTrain+i-1))
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
print("SES Moving Window Retrain")
print(ses2.eval)

print("SES Expanding Window - Quarterly")
print(sesQ.eval)
print("SES Moving Window - Quarterly")
print(ses1Q.eval)
print("SES Moving Window Retrain - Quarterly")
print(ses2Q.eval)









