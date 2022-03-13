#********************************************************#
#   State Space Model expanding window
#********************************************************#

# library(timeSeries)
# library(forecast)
# options(digits=3)


# Training set  : 2012/01/01-2017/12/31
# Validation set: 2018/01/01-2019/12/31
# Test set      : 2019/01/01-2021/12/31

SOMME.ts <- ts(data$SOMME, start=1)

# in.sample  = training set
# out.sample = validation set
begTrain <- 1 # 2012/01/01
endTrain <- 2192 # 2017/12/31
endValid <- 2922 # 2019/12/31
in.sample  <- ts(window(SOMME.ts, begTrain, endTrain))
out.sample <- ts(window(SOMME.ts, endTrain+1, endValid))

n.out <- length(out.sample)

# in.sample with weekly seasonality
in.ts <- ts(in.sample, start=c(1,1), frequency=7)
ets.opt <- ets(in.ts)

ets.val <- ts(numeric(n.out))
ets.lower.conf <- ts(numeric(n.out))
ets.upper.conf <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal model
for (i in 1:n.out) {
  tmp.ts <- ts(window(SOMME.ts, begTrain, endTrain+i-1), frequency=7)
  fc <- forecast(tmp.ts, model=ets.opt, h=1, use.initial.values=T)
  ets.val[i] <- fc$mean[1]
  ets.lower.conf[i] <- fc$lower[2]
  ets.upper.conf[i] <- fc$upper[2]
}

# To report accuracy measure for the forecasts
cat("Performance - State Space Model:","\n")
ets.eval <- rbind(accuracy(ets.opt$fitted, in.ts)[,1:5],
                  accuracy(ets.val, out.sample)[,1:5])
rownames(ets.eval) <- make.names(c("Training","Validation"))
print(ets.eval)

cat("Performance - Simple Exponential Smoothing:","\n")
etsQ.eval <- rbind(
  accuracy(ets.val[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(ets.val[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(ets.val[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(ets.val[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])


rownames(etsQ.eval) <- make.names(c( 
  "Validation Q1","Validation Q2", 
  "Validation Q3", "Validation Q4"))

print(etsQ.eval)


## seeing confidence intervals
validation.period = 1:length(out.sample)
plot(validation.period ,out.sample,type="l", 
     ylab="Actual vs Confidence Interval", 
     col="red")
lines(ets.lower.conf, col="blue", lty=3, type="l")
lines(ets.upper.conf, col="blue", lty=3, type ="l")


plot(validation.period ,out.sample,type="l", 
     ylab="Actual vs Forecast", 
     col="red")
lines(ets.val, col="blue", lty=3, type="l")

#########################ETS (A, N, N) expanding window######

SOMME.ts <- ts(data$SOMME, start=1)

# in.sample  = training set
# out.sample = validation set
begTrain <- 1 # 2012/01/01
endTrain <- 2192 # 2017/12/31
endValid <- 2922 # 2019/12/31

in.sample  <- ts(window(SOMME.ts, begTrain, endTrain))
out.sample <- ts(window(SOMME.ts, endTrain+1, endValid))

n.out <- length(out.sample)

# in.sample with weekly seasonality
in.ts <- ts(in.sample, start=c(1,1), frequency=7)
ets.optnew <- ets(in.ts, "ANN")

ets.valnew <- ts(numeric(n.out))
ets.lower.confnew <- ts(numeric(n.out))
ets.upper.confnew <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal model
for (i in 1:n.out) {
  tmp.ts <- ts(window(SOMME.ts, begTrain, endTrain+i-1), frequency=7)
  fc <- forecast(tmp.ts, model=ets.optnew, h=1, 
                 use.initial.values=T)
  ets.valnew[i] <- fc$mean[1]
  ets.lower.confnew[i] <- fc$lower[2]
  ets.upper.confnew[i] <- fc$upper[2]
}

# To report accuracy measure for the forecasts
cat("Performance - State Space Model:","\n")
ets.evalnew <- rbind(accuracy(ets.optnew$fitted, in.ts)[,1:5],
                     accuracy(ets.valnew, out.sample)[,1:5])
rownames(ets.evalnew) <- make.names(c("Training","Validation"))
print(ets.evalnew)

cat("Performance - Simple Exponential Smoothing:","\n")
etsQ.evalnew <- rbind(
  accuracy(ets.valnew[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(ets.valnew[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(ets.valnew[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(ets.valnew[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])


rownames(etsQ.evalnew) <- make.names(c( 
  "Validation Q1","Validation Q2", 
  "Validation Q3", "Validation Q4"))

print(etsQ.evalnew)


#********************************************************#
#   State Space Model moving window
#********************************************************#


# Training set  : 2012/01/01-2017/12/31
# Validation set: 2018/01/01-2019/12/31
# Test set      : 2019/01/01-2021/12/31

SOMME.ts <- ts(data$SOMME, start=1)

# in.sample  = training set
# out.sample = validation set
begTrain <- 1 # 2012/01/01
endTrain <- 2192 # 2017/12/31
endValid <- 2922 # 2019/12/31

in.sample  <- ts(window(SOMME.ts, begTrain, endTrain))
out.sample <- ts(window(SOMME.ts, endTrain+1, endValid))

n.out <- length(out.sample)

# in.sample with weekly seasonality
in.ts <- ts(in.sample, start=c(1,1), frequency=7)
ets.opt <- ets(in.ts)

ets.val1 <- ts(numeric(n.out))
ets.lower.conf1 <- ts(numeric(n.out))
ets.upper.conf1 <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal model
for (i in 1:n.out) {
  tmp.ts <- ts(window(SOMME.ts, begTrain+i-1, endTrain+i-1), 
               frequency=7)
  fc <- forecast(tmp.ts, model=ets.opt, h=1, use.initial.values=T)
  ets.val1[i] <- fc$mean[1]
  ets.lower.conf1[i] <- fc$lower[2]
  ets.upper.conf1[i] <- fc$upper[2]
}

# To report accuracy measure for the forecasts
cat("Performance - State Space Model:","\n")
ets.eval1 <- rbind(accuracy(ets.opt$fitted, in.ts)[,1:5],
                   accuracy(ets.val1, out.sample)[,1:5])
rownames(ets.eval1) <- make.names(c("Training","Validation"))
print(ets.eval1)

cat("Performance - Simple Exponential Smoothing:","\n")
etsQ.eval1 <- rbind(
  accuracy(ets.val1[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(ets.val1[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(ets.val1[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(ets.val1[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])



rownames(etsQ.eval1) <- make.names(c( 
  "Validation Q1","Validation Q2", 
  "Validation Q3", "Validation Q4"))

print(etsQ.eval1)

#########################ETS (A, N, N) moving window######

SOMME.ts <- ts(data$SOMME, start=1)

# in.sample  = training set
# out.sample = validation set
begTrain <- 1 # 2012/01/01
endTrain <- 2192 # 2017/12/31
endValid <- 2922 # 2019/12/31

in.sample  <- ts(window(SOMME.ts, begTrain, endTrain))
out.sample <- ts(window(SOMME.ts, endTrain+1, endValid))

n.out <- length(out.sample)

# in.sample with weekly seasonality
in.ts <- ts(in.sample, start=c(1,1), frequency=7)
ets.optnew <- ets(in.ts, "ANN")

ets.val1new <- ts(numeric(n.out))
ets.lower.conf1new <- ts(numeric(n.out))
ets.upper.conf1new <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal model
for (i in 1:n.out) {
  tmp.ts <- ts(window(SOMME.ts, begTrain+i-1, endTrain+i-1), 
               frequency=7)
  fc <- forecast(tmp.ts, model=ets.optnew, h=1, 
                 use.initial.values=T)
  ets.val1new[i] <- fc$mean[1]
  ets.lower.conf1new[i] <- fc$lower[2]
  ets.upper.conf1new[i] <- fc$upper[2]
}

# To report accuracy measure for the forecasts
cat("Performance - State Space Model:","\n")
ets.eval1new <- rbind(accuracy(ets.optnew$fitted, in.ts)[,1:5],
                      accuracy(ets.val1new, out.sample)[,1:5])
rownames(ets.eval1new) <- make.names(c("Training","Validation"))
print(ets.eval1new)

cat("Performance - Simple Exponential Smoothing:","\n")
etsQ.eval1new <- rbind(
  accuracy(ets.val1new[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(ets.val1new[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(ets.val1new[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(ets.val1new[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])



rownames(etsQ.eval1new) <- make.names(c( 
  "Validation Q1","Validation Q2", 
  "Validation Q3", "Validation Q4"))
print(etsQ.eval1new)


## seing the confidence interval

validation.period = 1:length(out.sample)
plot(validation.period ,out.sample,type="l", 
     ylab="Actual vs Confidence Interval", 
     col="red")
lines(ets.lower.conf1, col="blue", lty=3, type="l")
lines(ets.upper.conf1, col="blue", lty=3, type ="l")


plot(validation.period ,out.sample,type="l", 
     ylab="Actual vs Forecast", 
     col="red")
lines(ets.val1, col="blue", lty=3, type="l")


#***************************************************************#
#   State Space Model expanding window with retraining every year
#***************************************************************#

SOMME.ts <- ts(data$SOMME, start=1)

# in.sample  = training set
# out.sample = validation set
begTrain <- 1 # 2012/01/01
endTrain <- 2192 # 2017/12/31
endValid <- 2922 # 2019/12/31

in.sample  <- ts(window(SOMME.ts, begTrain, endTrain))
out.sample <- ts(window(SOMME.ts, endTrain+1, endValid))

n.out <- length(out.sample)

# in.sample with weekly seasonality
in.ts <- ts(in.sample, start=c(1,1), frequency=7)
ets.opt <- ets(in.ts)

ets.val2 <- ts(numeric(n.out))
ets.lower.conf2 <- ts(numeric(n.out))
ets.upper.conf2 <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal model
newEndTrainEts = endTrain
begTrainEts = begTrain
for (i in 1:n.out) {
  if (i%%365 == 0){
    newEndTrainEts = newEndTrainEts + 365
    in.ts <- ts(window(SOMME.ts, begTrainEts, 
                       newEndTrainEts),start=c(1,1), 
                frequency=7)
    ets.opt <- ets(in.ts)
    print(ets.opt)
  }
  tmp.ts <- ts(window(SOMME.ts, begTrain, endTrain+i-1), frequency=7)
  fc <- forecast(tmp.ts, model=ets.opt, h=1, use.initial.values=T)
  ets.val2[i] <- fc$mean[1]
  ets.lower.conf2[i] <- fc$lower[2]
  ets.upper.conf2[i] <- fc$upper[2]
}

# To report accuracy measure for the forecasts
cat("Performance - State Space Model:","\n")
ets.eval2 <- rbind(accuracy(ets.opt$fitted, in.ts)[,1:5],
                   accuracy(ets.val2, out.sample)[,1:5])
rownames(ets.eval2) <- make.names(c("Training","Validation"))
print(ets.eval2)

cat("Performance - Simple Exponential Smoothing:","\n")
etsQ.eval2 <- rbind(
  accuracy(ets.val2[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(ets.val2[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(ets.val2[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(ets.val2[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])


rownames(etsQ.eval2) <- make.names(c( 
  "Validation Q1","Validation Q2", 
  "Validation Q3", "Validation Q4"))

print(etsQ.eval2)


## additive error expanding window


SOMME.ts <- ts(data$SOMME, start=1)

# in.sample  = training set
# out.sample = validation set
begTrain <- 1 # 2012/01/01
endTrain <- 2192 # 2017/12/31
endValid <- 2922 # 2019/12/31

in.sample  <- ts(window(SOMME.ts, begTrain, endTrain))
out.sample <- ts(window(SOMME.ts, endTrain+1, endValid))

n.out <- length(out.sample)

# in.sample with weekly seasonality
in.ts <- ts(in.sample, start=c(1,1), frequency=7)
ets.optnew <- ets(in.ts, "ANN")

ets.val2new <- ts(numeric(n.out))
ets.lower.conf2new <- ts(numeric(n.out))
ets.upper.conf2new <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal model
newEndTrainEts = endTrain
begTrainEts = begTrain
for (i in 1:n.out) {
  if (i%%365 == 0){
    newEndTrainEts = newEndTrainEts + 365
    in.ts <- ts(window(SOMME.ts, begTrainEts, 
                       newEndTrainEts),start=c(1,1), 
                frequency=7)
    ets.optnew <- ets(in.ts)
    print(ets.optnew)
  }
  tmp.ts <- ts(window(SOMME.ts, begTrain, endTrain+i-1), frequency=7)
  fc <- forecast(tmp.ts, model=ets.optnew, h=1,
                 use.initial.values=T)
  ets.val2new[i] <- fc$mean[1]
  ets.lower.conf2new[i] <- fc$lower[2]
  ets.upper.conf2new[i] <- fc$upper[2]
}

# To report accuracy measure for the forecasts
cat("Performance - State Space Model:","\n")
ets.eval2new <- rbind(accuracy(ets.optnew$fitted, in.ts)[,1:5],
                      accuracy(ets.val2new, out.sample)[,1:5])
rownames(ets.eval2new) <- make.names(c("Training","Validation"))
print(ets.eval2new)

cat("Performance - Simple Exponential Smoothing:","\n")
etsQ.eval2new <- rbind(
  accuracy(ets.val2new[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(ets.val2new[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(ets.val2new[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(ets.val2new[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])


rownames(etsQ.eval2new) <- make.names(c( 
  "Validation Q1","Validation Q2", 
  "Validation Q3", "Validation Q4"))

print(etsQ.eval2new)


#********************************************************#
#   State Space Model Let us check holt with ets
# Not covered in report
#********************************************************#

## Same Results as SES with the SES Package
## additive error
in.ts <- ts(in.sample, start=c(1,1), frequency=7)
holtEts <- ets(in.ts, "MAN") 

holtEts.val <- ts(numeric(n.out))
holtEts.lower.conf <- ts(numeric(n.out))
holtEts.upper.conf <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal model
for (i in 1:n.out) {
  tmp.ts <- ts(window(WFEC.ts, begTrain, endTrain+i-1), frequency=7)
  fc <- forecast(tmp.ts, model=holtEts, h=1, use.initial.values=T)
  holtEts.val[i] <- fc$mean[1]
  holtEts.lower.conf[i] <- fc$lower[2]
  holtEts.upper.conf[i] <- fc$upper[2]
}

# To report accuracy measure for the forecasts
cat("Performance - State Space Model:","\n")
holtEts.eval <- rbind(accuracy(as.numeric(holtEts$fitted), 
                               as.numeric(in.ts))[,1:5],
                      accuracy(holtEts.val, out.sample)[,1:5])
rownames(holtEts.eval) <- make.names(c("Training","Validation"))
print(holtEts.eval)

cat("Performance - Simple Exponential Smoothing:","\n")
holtEtsQ.eval <- rbind(
  accuracy(holtEts.val[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(holtEts.val[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(holtEts.val[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(holtEts.val[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])

rownames(holtEtsQ.eval) <- make.names(c( 
  "Validation Q1","Validation Q2", 
  "Validation Q3", "Validation Q4"))

print(holtEtsQ.eval)