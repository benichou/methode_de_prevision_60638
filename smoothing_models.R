# Program: smoothing_models.R
#
# Purpose: adjust ETS and TBATS models and assess their performance
# in the purpose of forecasting on daily Electricity demand
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
# Test set      : 2020/01/01-2021/12/31

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
# (5) ETS expanding window
#==========================================================
#ets doesn't accept frequencies higher than 24, thus s is set to 7
ets.ts = ts(in.sample, start = 1, frequency=7)

ets.opt = ets(ets.ts, allow.multiplicative.trend = TRUE)

ets.val <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameters

for (i in 1:n.out) {
  tmp.ts <- ts(window(SOMME.ts, begTrain, endTrain+i-1))
  fc <- forecast(tmp.ts, model=ets.opt, h=1, use.initial.values=T)
  ets.val[i] <- fc$fitted[1]
}

# To report accuracy measure for the forecasts
cat("Performance - ETS Method:","\n")
ets.eval <- rbind(accuracy(ets.opt$fitted, ets.ts)[,1:5],
                    accuracy(ets.val, out.sample)[,1:5])
rownames(ets.eval) <- make.names(c("Training","Validation"))
print(ets.eval)

etsQ.eval <- rbind(
  accuracy(ets.val[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(ets.val[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(ets.val[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(ets.val[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])


rownames(etsQ.eval) <- make.names(c("ValQ1      ets",
                                      "ValQ2      ets",
                                      "ValQ3      ets",
                                      "ValQ4      ets"))
print(etsQ.eval)

#==========================================================
# (5) ETS moving window
#==========================================================
#ets doesn't accept frequencies higher than 24, thus s is set to 7
ets.ts = ts(in.sample, start = 1, frequency=7)

ets.opt = ets(ets.ts, allow.multiplicative.trend = TRUE)

ets.val1 <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameters

for (i in 1:n.out) {
  tmp.ts <- ts(window(SOMME.ts, begTrain+i-1, endTrain+i-1))
  fc <- forecast(tmp.ts, model=ets.opt, h=1, use.initial.values=T)
  ets.val1[i] <- fc$fitted[1]
}

# To report accuracy measure for the forecasts
cat("Performance - ETS Method:","\n")
ets.eval1 <- rbind(accuracy(ets.opt$fitted, ets.ts)[,1:5],
                  accuracy(ets.val1, out.sample)[,1:5])
rownames(ets.eval1) <- make.names(c("Training","Validation"))
print(ets.eval1)

etsQ.eval1 <- rbind(
  accuracy(ets.val1[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(ets.val1[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(ets.val1[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(ets.val1[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])


rownames(etsQ.eval1) <- make.names(c("ValQ1      ets",
                                    "ValQ2      ets",
                                    "ValQ3      ets",
                                    "ValQ4      ets"))
print(etsQ.eval1)

#==========================================================
# (5) ETS expanding window with re-training every year
#==========================================================
#ets doesn't accept frequencies higher than 24, thus s is set to 7
# ets.ts = ts(in.sample, start = 1, frequency=7)
# 
# ets.opt = ets(ets.ts, allow.multiplicative.trend = TRUE)
# 
# ets.val3 <- ts(numeric(n.out))
# 
# newEndTrainETS = endTrain
# 
# # Calculating rolling forecasts for the validation set 
# # using the optimal value of the parameters
# 
# for (i in 1:n.out) {
#   if (i%%365 == 0){
#     newEndTrainETS = newEndTrainETS + 365
#     ets.opt = ets(ts(window(SOMME.ts, begTrain, 
#                             newEndTrainTBATS),
#                      start = 1, frequency=7)
#     ))
#   }
#   tmp <- msts(window(SOMME.ts, begTrain, endTrain+i-1),
#               seasonal.periods=c(7,365.25))
#   # refit model with smoothing parameters found in optimal model
#   tmp.fit <- tbats(tmp,model=tbats.opt)
#   ets.val3[i] <- forecast(tmp.fit, h=1)$fitted[1]
# }
# 
# # To report accuracy measure for the forecasts
# cat("Performance - ETS Method:","\n")
# ets.eval3 <- rbind(accuracy(ets.opt$fitted, ets.ts)[,1:5],
#                    accuracy(ets.val3, out.sample)[,1:5])
# rownames(ets.eval3) <- make.names(c("Training","Validation"))
# print(ets.eval3)
# 
# etsQ.eval3 <- rbind(
#   accuracy(ets.val3[c(1:90,366:455)], 
#            out.sample[c(1:90,366:455)])[,1:5], 
#   accuracy(ets.val3[c(91:181,456:547 )], 
#            out.sample[c(91:181,456:547)])[,1:5],
#   accuracy(ets.val3[c(182:273, 548:638)], 
#            out.sample[c(182:273, 548:638)])[,1:5],
#   accuracy(ets.val3[c(274:365, 639:730)], 
#            out.sample[c(274:365, 639:730)])[,1:5])
# 
# 
# rownames(etsQ.eval3) <- make.names(c("ValQ1      ets",
#                                      "ValQ2      ets",
#                                      "ValQ3      ets",
#                                      "ValQ4      ets"))
# print(etsQ.eval3)

#==========================================================
# (6) TBATS expanding window
#==========================================================
#On peut utiliser des saisonalites non-imbriquees avec TBATS
tbats.msts = msts(in.sample, seasonal.periods=c(7,365.25))

tbats.opt = tbats(tbats.msts)

tbats.val <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameters

for (i in 1:n.out) {
  tmp <- msts(window(SOMME.ts, begTrain, endTrain+i-1),
              seasonal.periods=c(7,365.25))
  # refit model with smoothing parameters found in optimal model
  tmp.fit <- tbats(tmp,model=tbats.opt)
  tbats.val[i] <- forecast(tmp.fit, h=1)$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - TBATS Method:","\n")
tbats.eval <- rbind(accuracy(tbats.opt$fitted, tbats.msts)[,1:5],
                    accuracy(tbats.val, out.sample)[,1:5])
rownames(tbats.eval) <- make.names(c("Training","Validation"))
print(tbats.eval)

tbatsQ.eval <- rbind(
  accuracy(tbats.val[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(tbats.val[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(tbats.val[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(tbats.val[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])


rownames(tbatsQ.eval) <- make.names(c("ValQ1     tbats",
                                      "ValQ2      tbats",
                                      "ValQ3      tbats",
                                      "ValQ4      tbats"))
print(tbatsQ.eval)

#==========================================================
# (6) TBATS moving window
#==========================================================

tbats.msts = msts(in.sample, seasonal.periods=c(7,365.25))

tbats.opt = tbats(tbats.msts)

tbats.val1 <- ts(numeric(n.out))

# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameters

for (i in 1:n.out) {
  tmp <- msts(window(SOMME.ts, begTrain+i-1, endTrain+i-1),
              seasonal.periods=c(7,365.25))
  # refit model with smoothing parameters found in optimal model
  tmp.fit <- tbats(tmp,model=tbats.opt)
  tbats.val1[i] <- forecast(tmp.fit, h=1)$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - TBATS Method:","\n")
tbats.eval1 <- rbind(accuracy(tbats.opt$fitted, tbats.msts)[,1:5],
                     accuracy(tbats.val1, out.sample)[,1:5])
rownames(tbats.eval1) <- make.names(c("Training","Validation"))
print(tbats.eval1)

tbatsQ.eval1 <- rbind(
  accuracy(tbats.val1[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(tbats.val1[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(tbats.val1[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(tbats.val1[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])


rownames(tbatsQ.eval1) <- make.names(c("ValQ1     tbats",
                                       "ValQ2      tbats",
                                       "ValQ3      tbats",
                                       "ValQ4      tbats"))
print(tbatsQ.eval1)

#==========================================================
# (6) TBATS expanding window re-training every year
#==========================================================

tbats.msts = msts(in.sample, seasonal.periods=c(7,365.25))

tbats.opt = tbats(tbats.msts)

tbats.val3 <- ts(numeric(n.out))

newEndTrainTBATS = endTrain

# Calculating rolling forecasts for the validation set 
# using the optimal value of the parameters
for (i in 1:n.out) {
  if (i%%365 == 0){
    newEndTrainTBATS = newEndTrainTBATS + 365
    tbats.a.opt = tbats(msts(window(SOMME.ts, begTrain, 
                                    newEndTrainTBATS),
                             seasonal.periods=c(7,365.25)
    ))
  }
  tmp <- msts(window(SOMME.ts, begTrain, endTrain+i-1),
              seasonal.periods=c(7,365.25))
  # refit model with smoothing parameters found in optimal model
  tmp.fit <- tbats(tmp,model=tbats.opt)
  tbats.val3[i] <- forecast(tmp.fit, h=1)$mean[1]
}

# To report accuracy measure for the forecasts
cat("Performance - TBATS Method:","\n")
tbats.eval3 <- rbind(accuracy(tbats.opt$fitted, tbats.msts)[,1:5],
                     accuracy(tbats.val3, out.sample)[,1:5])
rownames(tbats.eval3) <- make.names(c("Training","Validation"))
print(tbats.eval3)


tbatsQ.eval3 <- rbind(
  accuracy(tbats.val3[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(tbats.val3[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(tbats.val3[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(tbats.val3[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])

rownames(tbatsQ.eval3) <- make.names(c("ValQ1     tbats",
                                       "ValQ2      tbats",
                                       "ValQ3      tbats",
                                       "ValQ4      tbats"))
print(tbatsQ.eval3)

