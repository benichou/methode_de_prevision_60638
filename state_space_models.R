# Program: state_space_models.R
#
# Purpose: Adjusts the state space models both manually and 
## automatically with moving and expanding window and with or without
## retraining to minimize the forecast error
# 
#
# Written by: Team G, March 12 2022
#
# Updated: Team G, March 13 2022
#          
#         
#          
#         
#          
# ------------------------------------------------------

# library(timeSeries)
# library(forecast)
# options(digits=3)

# REMINDER: OUR NEW BASELINE is HOLTWINTERS with MAPE at 5.840536
# we are going to compare based on performance of course
#and also assess the forecast intervals produced by the 
# models

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

# Confidence Interval
CI <- matrix(nrow=n.out, ncol=8)
colnames(CI) <- c("observed","lo80","hi80","in CI80",
                  "lo95","hi95","in CI95","forecast")

# Calculating rolling forecasts for the validation set 
# using the optimal model
for (i in 1:n.out) {
  tmp.ts <- ts(window(SOMME.ts, begTrain, endTrain+i-1), frequency=7)
  fc <- forecast(tmp.ts, model=ets.opt, h=1, use.initial.values=T)
  ets.val[i] <- fc$mean[1]
  ets.lower.conf[i] <- fc$lower[2]
  ets.upper.conf[i] <- fc$upper[2]
  # confidence interval
  in80 <- (out.sample[i] >= fc$lower[1] &&
             out.sample[i] <= fc$upper[1])
  in95 <- (out.sample[i] >= fc$lower[2] &&
             out.sample[i] <= fc$upper[2])
  CI[i,] <- c(out.sample[i], fc$lower[1], fc$upper[1],in80,
              fc$lower[2], fc$upper[2], in95, ets.val[i])
}

# To report accuracy measure for the forecasts
cat("Performance - State Space Model:","\n")
ets.eval <- rbind(accuracy(ets.opt$fitted, in.ts)[,1:5],
                  accuracy(ets.val, out.sample)[,1:5])
rownames(ets.eval) <- make.names(c("Training","Validation"))
print(ets.eval)

cat("Performance:","\n")
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

df <- data.frame(CI)
df$date <-as.Date(data$DATE[(endTrain+1):endValid])

cat("Report on the prediction interval:")
# Calculate the % of the observed values fall into the CI
rep <- rbind(cbind(sum(df$in.CI80), nrow(df), 
                   sum(df$in.CI80)/nrow(df)*100),
             cbind(sum(df$in.CI95), nrow(df), 
                   sum(df$in.CI95)/nrow(df)*100))
rownames(rep) <- make.names(c("Prediction interval 80%",
                              "Prediction interval 95%"))
colnames(rep) <- make.names(c("Observed","Total","Percentage"))
print(rep)

# ## SUMMARY State Space Model Expanding Window

# # ETS(M,A,A)

# # Call:
# #  ets(y = in.ts)

# #   Smoothing parameters:
# #     alpha = 0.9999
# #     beta  = 0.0003
# #     gamma = 0.0001

# #      AIC     AICc      BIC
# # 61903.69 61903.83 61972.00

#                   ME     RMSE      MAE        MPE     MAPE
# Training   -2524.877 29128.41 20204.09 -1.0347982 5.624618
# Validation -1993.689 31555.15 22542.40 -0.8214235 5.871230

#                        ME     RMSE      MAE        MPE     MAPE
# Validation.Q1 -3170.41613 39818.37 28577.30 -1.3172920 7.486441
# Validation.Q2   -65.89119 26027.29 19537.90 -0.2843276 5.240896
# Validation.Q3 -2266.48431 27747.49 19694.43 -0.6715977 4.366839
# Validation.Q4 -2488.54962 30982.72 22459.37 -1.0195232 6.414254

#                         Observed Total Percentage
# Prediction.interval.80.      611   730   83.69863
# Prediction.interval.95.      690   730   94.52055


#########################ETS (A, A, A) expanding window######
## Holt Winters

SOMME.ts <- ts(data$SOMME, start=1)

# in.sample  = training set
# out.sample = validation set
begTrain <- 1 # 2012/01/01
endTrain <- 2192 # 2017/12/31
endValid <- 2922 # 2019/12/31

in.sample  <- ts(window(SOMME.ts, begTrain, endTrain))
out.sample <- ts(window(SOMME.ts, endTrain+1, endValid))

n.out <- length(out.sample)


# Confidence Interval
CI <- matrix(nrow=n.out, ncol=8)
colnames(CI) <- c("observed","lo80","hi80","in CI80",
                  "lo95","hi95","in CI95","forecast")

# in.sample with weekly seasonality
in.ts <- ts(in.sample, start=c(1,1), frequency=7)
ets.optnew <- ets(in.ts, "AAA")

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
  in80 <- (out.sample[i] >= fc$lower[1] &&
             out.sample[i] <= fc$upper[1])
  in95 <- (out.sample[i] >= fc$lower[2] &&
             out.sample[i] <= fc$upper[2])
  CI[i,] <- c(out.sample[i], fc$lower[1], fc$upper[1],in80,
              fc$lower[2], fc$upper[2], in95, ets.val[i])
}

# To report accuracy measure for the forecasts
cat("Performance - State Space Model:","\n")
ets.evalnew <- rbind(accuracy(ets.optnew$fitted, in.ts)[,1:5],
                     accuracy(ets.valnew, out.sample)[,1:5])
rownames(ets.evalnew) <- make.names(c("Training","Validation"))
print(ets.evalnew)

cat("Performance :","\n")
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

df <- data.frame(CI)
df$date <-as.Date(data$DATE[(endTrain+1):endValid])

cat("Report on the prediction interval:")
# Calculate the % of the observed values fall into the CI
rep <- rbind(cbind(sum(df$in.CI80), nrow(df), 
                   sum(df$in.CI80)/nrow(df)*100),
             cbind(sum(df$in.CI95), nrow(df), 
                   sum(df$in.CI95)/nrow(df)*100))
rownames(rep) <- make.names(c("Prediction interval 80%",
                              "Prediction interval 95%"))
colnames(rep) <- make.names(c("Observed","Total","Percentage"))
print(rep)

## SUMMARY EXPANDING WINDOW ETS(A,A,A)
## ETS(A,Ad,A)

# Call:
#  ets(y = in.ts, model = "AAA")

#   Smoothing parameters:
#     alpha = 0.9982
#     beta  = 0.0001
#     gamma = 0.0018
#     phi   = 0.9728

#   Initial states:
#     l = 314949.2291
#     b = 3484.4431
#     s = -15523.14 2809.37 9780.515 10596.39 8805.088 6603.415
#            -23071.64

#   sigma:  29137.74

#      AIC     AICc      BIC
# 61942.68 61942.84 62016.68
# ##
#                    ME     RMSE      MAE        MPE     MAPE
# Training     48.85774 29057.88 20161.63 -0.2951458 5.580315
# Validation -263.40880 31465.29 22468.79 -0.3588995 5.838487

#                       ME     RMSE      MAE        MPE     MAPE
# Validation.Q1 -1397.2756 39667.24 28243.51 -0.8075616 7.378506
# Validation.Q2  1690.0724 25930.43 19748.80  0.1955532 5.277823
# Validation.Q3  -526.0082 27735.99 19657.24 -0.2980200 4.350357
# Validation.Q4  -835.8835 30900.68 22321.08 -0.5319788 6.369608

#                         Observed Total Percentage
# Prediction.interval.80.      599   730   82.05479
# Prediction.interval.95.      676   730   92.60274


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

# Confidence Interval
CI <- matrix(nrow=n.out, ncol=8)
colnames(CI) <- c("observed","lo80","hi80","in CI80",
                  "lo95","hi95","in CI95","forecast")

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
  in80 <- (out.sample[i] >= fc$lower[1] &&
             out.sample[i] <= fc$upper[1])
  in95 <- (out.sample[i] >= fc$lower[2] &&
             out.sample[i] <= fc$upper[2])
  CI[i,] <- c(out.sample[i], fc$lower[1], fc$upper[1],in80,
              fc$lower[2], fc$upper[2], in95, ets.val[i])
}

# To report accuracy measure for the forecasts
cat("Performance - State Space Model:","\n")
ets.eval1 <- rbind(accuracy(ets.opt$fitted, in.ts)[,1:5],
                   accuracy(ets.val1, out.sample)[,1:5])
rownames(ets.eval1) <- make.names(c("Training","Validation"))
print(ets.eval1)

cat("Performance :","\n")
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

df <- data.frame(CI)
df$date <-as.Date(data$DATE[(endTrain+1):endValid])

cat("Report on the prediction interval:")
# Calculate the % of the observed values fall into the CI
rep <- rbind(cbind(sum(df$in.CI80), nrow(df), 
                   sum(df$in.CI80)/nrow(df)*100),
             cbind(sum(df$in.CI95), nrow(df), 
                   sum(df$in.CI95)/nrow(df)*100))
rownames(rep) <- make.names(c("Prediction interval 80%",
                              "Prediction interval 95%"))
colnames(rep) <- make.names(c("Observed","Total","Percentage"))
print(rep)

# SUMMARY MOVING WINDOW
##
# ETS(M,A,A)

# Call:
#  ets(y = in.ts)

#   Smoothing parameters:
#     alpha = 0.9999
#     beta  = 0.0003
#     gamma = 0.0001

#   Initial states:
#     l = 340411.8207
#     b = 3483.8758
#     s = -17773.31 2808.673 9785.026 10592.44 8803.335 6603.394
#            -20819.56

#   sigma:  0.0805

#      AIC     AICc      BIC
# 61903.69 61903.83 61972.00

#                    ME     RMSE      MAE       MPE     MAPE
# Training    -2524.877 29128.41 20204.09 -1.034798 5.624618
# Validation -28610.446 43864.44 35898.66 -8.010325 9.705567

#                      ME     RMSE      MAE       MPE      MAPE
# Validation.Q1 -29892.98 50924.81 42556.49 -9.108979 11.965870
# Validation.Q2 -26556.62 38458.17 30915.99 -7.532539  8.593170
# Validation.Q3 -28957.35 41566.98 33251.58 -6.452930  7.349339
# Validation.Q4 -29053.45 43671.93 36973.84 -8.959673 10.944177

#                         Observed Total Percentage
# Prediction.interval.80.      567   730   77.67123
# Prediction.interval.95.      691   730   94.65753



#########################ETS (A, A, A) moving window######

SOMME.ts <- ts(data$SOMME, start=1)

# in.sample  = training set
# out.sample = validation set
begTrain <- 1 # 2012/01/01
endTrain <- 2192 # 2017/12/31
endValid <- 2922 # 2019/12/31

in.sample  <- ts(window(SOMME.ts, begTrain, endTrain))
out.sample <- ts(window(SOMME.ts, endTrain+1, endValid))

n.out <- length(out.sample)

# Confidence Interval
CI <- matrix(nrow=n.out, ncol=8)
colnames(CI) <- c("observed","lo80","hi80","in CI80",
                  "lo95","hi95","in CI95","forecast")

# in.sample with weekly seasonality
in.ts <- ts(in.sample, start=c(1,1), frequency=7)
ets.optnew <- ets(in.ts, "AAA")

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
  in80 <- (out.sample[i] >= fc$lower[1] &&
             out.sample[i] <= fc$upper[1])
  in95 <- (out.sample[i] >= fc$lower[2] &&
             out.sample[i] <= fc$upper[2])
  CI[i,] <- c(out.sample[i], fc$lower[1], fc$upper[1],in80,
              fc$lower[2], fc$upper[2], in95, ets.val[i])
}

# To report accuracy measure for the forecasts
cat("Performance - State Space Model:","\n")
ets.eval1new <- rbind(accuracy(ets.optnew$fitted, in.ts)[,1:5],
                      accuracy(ets.val1new, out.sample)[,1:5])
rownames(ets.eval1new) <- make.names(c("Training","Validation"))
print(ets.eval1new)

cat("Performance:","\n")
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

df <- data.frame(CI)
df$date <-as.Date(data$DATE[(endTrain+1):endValid])

cat("Report on the prediction interval:")
# Calculate the % of the observed values fall into the CI
rep <- rbind(cbind(sum(df$in.CI80), nrow(df), 
                   sum(df$in.CI80)/nrow(df)*100),
             cbind(sum(df$in.CI95), nrow(df), 
                   sum(df$in.CI95)/nrow(df)*100))
rownames(rep) <- make.names(c("Prediction interval 80%",
                              "Prediction interval 95%"))
colnames(rep) <- make.names(c("Observed","Total","Percentage"))
print(rep)

## SUMMARY ##########ETS (A, A, A) moving window######
##
##ETS(A,Ad,A)
# Call:
#  ets(y = in.ts, model = "AAA")

#   Smoothing parameters:
#     alpha = 0.9982
#     beta  = 0.0001
#     gamma = 0.0018
#     phi   = 0.9728

#   Initial states:
#     l = 314949.2291
#     b = 3484.4431
#     s = -15523.14 2809.37 9780.515 10596.39 8805.088 6603.415
#            -23071.64

#   sigma:  29137.74

#      AIC     AICc      BIC 
# 61942.68 61942.84 62016.68

#                      ME     RMSE      MAE        MPE     MAPE
# Training       48.85774 29057.88 20161.63 -0.2951458 5.580315
# Validation -12777.45000 34665.97 26041.23 -3.7774862 6.911782

#                      ME     RMSE      MAE       MPE     MAPE
# Validation.Q1 -13969.75 42740.20 33145.67 -4.527726 8.971959
# Validation.Q2 -10786.09 29350.53 21527.80 -3.252882 5.874742
# Validation.Q3 -13095.96 31301.25 23007.65 -3.046282 5.119181
# Validation.Q4 -13274.83 33904.33 26597.23 -4.292539 7.710652

#                         Observed Total Percentage
# Prediction.interval.80.      604   730   82.73973
# Prediction.interval.95.      681   730   93.28767

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

# Confidence Interval
CI <- matrix(nrow=n.out, ncol=8)
colnames(CI) <- c("observed","lo80","hi80","in CI80",
                  "lo95","hi95","in CI95","forecast")

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
  in80 <- (out.sample[i] >= fc$lower[1] &&
             out.sample[i] <= fc$upper[1])
  in95 <- (out.sample[i] >= fc$lower[2] &&
             out.sample[i] <= fc$upper[2])
  CI[i,] <- c(out.sample[i], fc$lower[1], fc$upper[1],in80,
              fc$lower[2], fc$upper[2], in95, ets.val[i])
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

df <- data.frame(CI)
df$date <-as.Date(data$DATE[(endTrain+1):endValid])

cat("Report on the prediction interval:")
# Calculate the % of the observed values fall into the CI
rep <- rbind(cbind(sum(df$in.CI80), nrow(df), 
                   sum(df$in.CI80)/nrow(df)*100),
             cbind(sum(df$in.CI95), nrow(df), 
                   sum(df$in.CI95)/nrow(df)*100))
rownames(rep) <- make.names(c("Prediction interval 80%",
                              "Prediction interval 95%"))
colnames(rep) <- make.names(c("Observed","Total","Percentage"))
print(rep)

## SUMMARY State Space Model expanding window 
## with retraining every year

## Across retraining the model remains MAA
## first year of val
#   Smoothing parameters:
#     alpha = 0.9999
#     beta  = 0.0003
#     gamma = 0.0001
# ## second year of val
# Smoothing parameters:
#     alpha = 0.9999
#     beta  = 0.0002
#     gamma = 0.0001

#                   ME     RMSE      MAE       MPE     MAPE
# Training   -2570.323 29754.99 20780.32 -1.037043 5.684317
# Validation -2006.176 31538.00 22527.87 -0.826124 5.868139

#                        ME     RMSE      MAE        MPE     MAPE
# Validation.Q1 -3184.95843 39794.48 28572.63 -1.3226986 7.485867
# Validation.Q2   -75.54729 26036.91 19546.78 -0.2884884 5.240957
# Validation.Q3 -2278.09805 27728.70 19628.89 -0.6748539 4.353030
# Validation.Q4 -2502.71301 30952.15 22462.65 -1.0255062 6.416225

#                         Observed Total Percentage
# Prediction.interval.80.      613   730   83.97260
# Prediction.interval.95.      690   730   94.52055

## additive error expanding ETS(A, A, A) 
# window with retraining (like normal
## holt winters but additive errors this time)

SOMME.ts <- ts(data$SOMME, start=1)

# in.sample  = training set
# out.sample = validation set
begTrain <- 1 # 2012/01/01
endTrain <- 2192 # 2017/12/31
endValid <- 2922 # 2019/12/31

in.sample  <- ts(window(SOMME.ts, begTrain, endTrain))
out.sample <- ts(window(SOMME.ts, endTrain+1, endValid))

n.out <- length(out.sample)

# Confidence Interval
CI <- matrix(nrow=n.out, ncol=8)
colnames(CI) <- c("observed","lo80","hi80","in CI80",
                  "lo95","hi95","in CI95","forecast")

# in.sample with weekly seasonality
in.ts <- ts(in.sample, start=c(1,1), frequency=7)
ets.optnew <- ets(in.ts, "AAA")

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
  in80 <- (out.sample[i] >= fc$lower[1] &&
             out.sample[i] <= fc$upper[1])
  in95 <- (out.sample[i] >= fc$lower[2] &&
             out.sample[i] <= fc$upper[2])
  CI[i,] <- c(out.sample[i], fc$lower[1], fc$upper[1],in80,
              fc$lower[2], fc$upper[2], in95, ets.val[i])
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

df <- data.frame(CI)
df$date <-as.Date(data$DATE[(endTrain+1):endValid])

cat("Report on the prediction interval:")
# Calculate the % of the observed values fall into the CI
rep <- rbind(cbind(sum(df$in.CI80), nrow(df), 
                   sum(df$in.CI80)/nrow(df)*100),
             cbind(sum(df$in.CI95), nrow(df), 
                   sum(df$in.CI95)/nrow(df)*100))
rownames(rep) <- make.names(c("Prediction interval 80%",
                              "Prediction interval 95%"))
colnames(rep) <- make.names(c("Observed","Total","Percentage"))
print(rep)

# SUMMARY additive error expanding ETS(A, A, A) 
## retraining
## ETS(M,A,A) ON FIRST YEAR OF VAL
## alpha = 0.9999
    # beta  = 0.0003
    # gamma = 0.0001
# ETS(M,A,A)  ON SECOND YEAR OF VAL
#   Smoothing parameters:
#     alpha = 0.9999
#     beta  = 0.0002
#     gamma = 0.0001

#                   ME     RMSE      MAE        MPE     MAPE
# Training   -2570.323 29754.99 20780.32 -1.0370429 5.684317
# Validation -1099.531 31482.32 22526.81 -0.5835351 5.859101

#                       ME     RMSE      MAE         MPE     MAPE
# Validation.Q1 -2255.9682 39676.94 28363.77 -1.05370354 7.419872
# Validation.Q2   840.5141 26012.43 19723.35 -0.04521989 5.273048
# Validation.Q3 -1355.9316 27656.26 19665.88 -0.47126627 4.353941
# Validation.Q4 -1642.7268 30959.80 22450.33 -0.77063611 6.412106

#                         Observed Total Percentage
# Prediction.interval.80.      607   730   83.15068
# Prediction.interval.95.      684   730   93.69863


#********************************************************#
#   State Space Model Let us check holt with ets
# 
#********************************************************#

## Same Results as holt with the holt Package
## additive error
in.ts <- ts(in.sample, start=c(1,1), frequency=7)
holtEts <- ets(in.ts, "AAN") 

holtEts.val <- ts(numeric(n.out))
holtEts.lower.conf <- ts(numeric(n.out))
holtEts.upper.conf <- ts(numeric(n.out))

# Confidence Interval
CI <- matrix(nrow=n.out, ncol=8)
colnames(CI) <- c("observed","lo80","hi80","in CI80",
                  "lo95","hi95","in CI95","forecast")


# Calculating rolling forecasts for the validation set 
# using the optimal model
for (i in 1:n.out) {
  tmp.ts <- ts(window(SOMME.ts, begTrain, endTrain+i-1), frequency=7)
  fc <- forecast(tmp.ts, model=holtEts, h=1, use.initial.values=T)
  holtEts.val[i] <- fc$mean[1]
  holtEts.lower.conf[i] <- fc$lower[2]
  holtEts.upper.conf[i] <- fc$upper[2]
  in80 <- (out.sample[i] >= fc$lower[1] &&
             out.sample[i] <= fc$upper[1])
  in95 <- (out.sample[i] >= fc$lower[2] &&
             out.sample[i] <= fc$upper[2])
  CI[i,] <- c(out.sample[i], fc$lower[1], fc$upper[1],in80,
              fc$lower[2], fc$upper[2], in95, ets.val[i])
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

df <- data.frame(CI)
df$date <-as.Date(data$DATE[(endTrain+1):endValid])

cat("Report on the prediction interval:")
# Calculate the % of the observed values fall into the CI
rep <- rbind(cbind(sum(df$in.CI80), nrow(df), 
                   sum(df$in.CI80)/nrow(df)*100),
             cbind(sum(df$in.CI95), nrow(df), 
                   sum(df$in.CI95)/nrow(df)*100))
rownames(rep) <- make.names(c("Prediction interval 80%",
                              "Prediction interval 95%"))
colnames(rep) <- make.names(c("Observed","Total","Percentage"))
print(rep)

## SUMMARY Holt method in ETS
##
# ## Call:
# ETS(A,Ad,N)
#   Smoothing parameters:
#     alpha = 0.9999
#     beta  = 0.0001
#     phi   = 0.8357

#                    ME     RMSE      MAE        MPE     MAPE
# Training     87.20926 31926.01 22985.89 -0.3618556 6.396710
# Validation -221.19614 33346.85 24308.36 -0.4192705 6.333463

#                       ME     RMSE      MAE        MPE     MAPE
# Validation.Q1 -1515.3694 41327.60 30531.95 -0.9457945 8.074106
# Validation.Q2  1833.3309 27942.70 21104.44  0.2021522 5.664090
# Validation.Q3  -549.4925 29957.53 21865.17 -0.3559883 4.829258
# Validation.Q4  -672.0061 32738.69 23836.50 -0.5851764 6.792425

#                         Observed Total Percentage
# Prediction.interval.80.      599   730   82.05479
# Prediction.interval.95.      676   730   92.60274