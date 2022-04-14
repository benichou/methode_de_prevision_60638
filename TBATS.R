# Program: TBATS.R
#
# Purpose: Adjusts the TBATS model between moving and expanding 
# window with and without retraining
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

# REMINDER: OUR NEW BASELINE is ETS(A, Ad, A) with MAPE at 5.838487
# we are going to compare based on performance of course
#and also assess the forecast intervals produced by the 
# models

# library(timeSeries)
# library(forecast)
options(digits=3)

pdf("visual_output/TBATS.pdf")

# Training set  : 2012/01/01-2017/12/31
# Validation set: 2018/01/01-2019/12/31
# Test set      : 2019/01/01-2021/12/31

SOMME.ts <- ts(data$SOMME, start=1)

autoplot(mstl(msts(SOMME.ts,seasonal.periods=c(7,365.25),
                   start=c(2012,1), end=c(2017,365))))

# in.sample  = training set
# out.sample = validation set
begTrain <- 1 # 2012/01/01
endTrain <- 2192 # 2017/12/31
endValid <- 2922 # 2019/12/31

in.sample  <- msts(window(SOMME.ts, begTrain, endTrain),
                   seasonal.periods=c(7,365.25), start=c(2012,1))
out.sample <- msts(window(SOMME.ts, endTrain+1, endValid),
                   seasonal.periods=c(7,365.25), start=c(2018,1))

# Fit the TBATS model
tfit <- tbats(in.sample)

n <- length(out.sample)
fc.exp <- fc.mov <- msts(numeric(n), seasonal.periods=c(7,365.25), 
                         start=c(2018,1))

# Confidence Interval
CI <- matrix(nrow=n, ncol=8)
colnames(CI) <- c("observed","lo80","hi80","in CI80",
                  "lo95","hi95","in CI95","forecast")

for (i in 1:n) {
  # Expanding window
  tmp.msts <- msts(window(SOMME.ts, begTrain, endTrain+i-1), 
                   seasonal.periods=c(7, 365.25))
  f.tbats <- forecast(tmp.msts, model=tfit, h=1)
  fc.exp[i] <- f.tbats$mean[1]

  # confidence interval
  in80 <- (out.sample[i] >= f.tbats$lower[1] &&
             out.sample[i] <= f.tbats$upper[1])
  in95 <- (out.sample[i] >= f.tbats$lower[2] &&
             out.sample[i] <= f.tbats$upper[2])
  CI[i,] <- c(out.sample[i], f.tbats$lower[1], f.tbats$upper[1],in80,
              f.tbats$lower[2], f.tbats$upper[2], in95, fc.exp[i])
  
  # Moving window -very poor performance compare to Expanding window
  tmp.msts <- msts(window(SOMME.ts, begTrain+i-1, endTrain+i-1), 
                   seasonal.periods=c(7, 365.25))
  f.tbats <- forecast(tmp.msts, model=tfit, h=1)
  fc.mov[i] <- f.tbats$mean[1]
}

cat("Performance of TBATS using function <accuracy>:","\n")
tbat.eval <- rbind(accuracy(tfit$fitted, in.sample)[,1:5],
                   accuracy(fc.exp, out.sample)[,1:5],
                   accuracy(fc.mov, out.sample)[,1:5])
rownames(tbat.eval) <- make.names(c("Training",
                                    "Validation  Expanding",
                                    "Validation  Moving"))
print(tbat.eval)

Q.eval <- rbind(accuracy(fc.exp[1:90], out.sample[1:90])[,1:5],
                accuracy(fc.exp[91:181], out.sample[91:181])[,1:5],
                accuracy(fc.exp[182:273], out.sample[182:273])[,1:5],
                accuracy(fc.exp[274:365], out.sample[274:365])[,1:5],
                accuracy(fc.exp[366:455], out.sample[366:455])[,1:5],
                accuracy(fc.exp[456:546], out.sample[456:546])[,1:5],
                accuracy(fc.exp[547:638], out.sample[547:638])[,1:5],
                accuracy(fc.exp[639:730], out.sample[639:730])[,1:5])

rownames(Q.eval) <- make.names(c("Q1_2018","Q2_2018","Q3_2018",
                                 "Q4_2018","Q1_2019","Q2_2019",
                                 "Q3_2019","Q4_2019"))
print(Q.eval)

Q1.eval <- rbind(accuracy(fc.exp[c(1:90,366:455)], 
           out.sample[c(1:90,366:455)])[,1:5], 
  accuracy(fc.exp[c(91:181,456:547 )], 
           out.sample[c(91:181,456:547)])[,1:5],
  accuracy(fc.exp[c(182:273, 548:638)], 
           out.sample[c(182:273, 548:638)])[,1:5],
  accuracy(fc.exp[c(274:365, 639:730)], 
           out.sample[c(274:365, 639:730)])[,1:5])

rownames(Q1.eval) <- make.names(c("Q1","Q2","Q3",
                                 "Q4"))
print(Q1.eval)

## SUMMARY MOVING AND EXPANDING WINDOW NO RETRAINING

# TBATS(0.772, {1,2}, -, {<7,3>, <365.25,2>})

# Call: tbats(y = in.sample)

# Parameters
#   Lambda: 0.771715
#   Alpha: 0.8272725
#   Gamma-1 Values: 0.0001424344 0.0003011435
#   Gamma-2 Values: 0.00009269187 -0.0002007421
#   AR coefficients: 0.664784
#   MA coefficients: -0.531989 -0.453573

#                      ME     RMSE      MAE        MPE     MAPE
# Training         727.4340 26338.20 18697.50 -0.2857807 5.171782
# Validati.Expanding  634.1568 28476.57 20721.05 -0.3740767 5.360130
# Validat..Moving -19334.2071 38606.46 31544.56 -6.2552918 8.884415

# FOR EXPANDING A DEEPER ANALYSIS SINCE IT IS BETTER 
#           ME     RMSE      MAE        MPE     MAPE
# Q1   540.379 34497.48 24223.67 -0.6196787 6.408772
# Q2  2042.827 24505.36 18821.70  0.1265523 4.984795
# Q3  2011.426 27105.29 20899.22  0.1241521 4.587163
# Q4 -2044.903 26926.93 19006.41 -1.1272429 5.476346


# ==================================================================
#   Re-train after 1 year
# ==================================================================
in.sample.r  <- msts(window(SOMME.ts, begTrain, endTrain+365),
                     seasonal.periods=c(7,365.25), start=c(2012,1))
out.sample.r <- msts(window(SOMME.ts, endTrain+366, endValid),
                     seasonal.periods=c(7,365.25), start=c(2019,1))

CI.r <- matrix(nrow=n, ncol=8)
colnames(CI.r) <- c("observed","lo80","hi80","in CI80",
                  "lo95","hi95","in CI95","forecast")


tfit.r <- tbats(in.sample.r)
n <- length(out.sample)-365
fcasts.r <- msts(numeric(n), seasonal.periods=c(7,365.25), 
                 start=c(2019,1))

for (i in 1:n) {
  # use expanding window only since moving window performs poorly
  tmp.msts <- msts(window(SOMME.ts, begTrain, (endTrain+365+i-1)), 
                   seasonal.periods=c(7, 365.25))
  f.r.tbat <- forecast(tmp.msts, model=tfit.r, h=1)
  fcasts.r[i] <- f.r.tbat$mean[1]

  # confidence interval
  in80.r <- (out.sample.r[i] >= f.r.tbat$lower[1] &&
             out.sample.r[i] <= f.r.tbat$upper[1])
  in95.r <- (out.sample.r[i] >= f.r.tbat$lower[2] &&
             out.sample.r[i] <= f.r.tbat$upper[2])
  CI.r[i,] <- c(out.sample.r[i], f.r.tbat$lower[1], 
              f.r.tbat$upper[1], in80.r, f.r.tbat$lower[2], 
              f.r.tbat$upper[2], in95.r, fcasts.r[i])
}
# print("11")
# print(CI.r)
CI.r = CI.r[1:365,] # quick fix

cat("Performance of TBATS if re-train after one year:","\n")
r.tbat.eval <- rbind(accuracy(tfit.r$fitted, in.sample.r)[,1:5],
                     accuracy(fcasts.r, out.sample.r)[,1:5])
rownames(r.tbat.eval) <- make.names(c("Training_set",
                                      "Validation_set"))
print(r.tbat.eval)

R.eval <- rbind(
  accuracy(fcasts.r[1:90], out.sample.r[1:90])[,1:5],
  accuracy(fcasts.r[91:181], out.sample.r[91:181])[,1:5],
  accuracy(fcasts.r[182:273], out.sample.r[182:273])[,1:5],
  accuracy(fcasts.r[274:365], out.sample.r[274:365])[,1:5])

rownames(R.eval) <- make.names(c("Q1_2019","Q2_2019",
                                 "Q3_2019","Q4_2019"))
print(R.eval)

## SUMMARY RETRAIN
# TBATS(0, {2,1}, -, {<7,3>, <365.25,5>})

# Call: tbats(y = in.sample.r)

# Parameters
#   Lambda: 0.000003
#   Alpha: 0.01295757
#   Gamma-1 Values: -0.0001150675 -0.0009998646
#   Gamma-2 Values: 0.0007998321 -0.0004689449
#   AR coefficients: 0.528947 0.04136
#   MA coefficients: 0.418922

#                      ME     RMSE      MAE        MPE     MAPE
# Training_set   1284.866 26475.28 18661.72 -0.1236762 5.083095
# Validation_set 1511.150 29632.85 21722.36 -0.1926832 5.597554

#                ME     RMSE      MAE        MPE     MAPE
# Q1_2019  4899.271 33692.29 24327.08  0.6325301 6.413325
# Q2_2019 -4627.117 26190.66 18697.74 -1.5752442 5.181551
# Q3_2019  8356.568 28596.12 23411.26  1.5403419 4.927446
# Q4_2019 -2577.188 29615.27 20477.10 -1.3654488 5.881107

# ==================================================================
# Overall evaluation (with/without annual re-training)
# ==================================================================
cat("Overall Performance of TBATS:","\n")
all.eval <- rbind(accuracy(fc.exp, out.sample),
                   accuracy(c(fc.exp[1:365], fcasts.r[1:365]),
                            out.sample))
rownames(all.eval) <- make.names(c("No_retrain", "Retrain"))
print(all.eval)

## By quarters 
cat("Overall Quarterly Performance:","\n")
qall.eval <- rbind(
  # NO annual re-training (expanding window)
  cbind(
    accuracy(fc.exp[c(1:90,366:455)], 
             out.sample[c(1:90,366:455)])[,5],
    accuracy(fc.exp[c(91:181,456:546)], 
             out.sample[c(91:181,456:546)])[,5],
    accuracy(fc.exp[c(182:273,547:638)], 
             out.sample[c(182:273,547:638)])[,5],
    accuracy(fc.exp[c(274:365,639:730)], 
             out.sample[c(274:365,639:730)])[,5]),
  # With annual re-training (expanding window)
  cbind(
    accuracy(c(fc.exp[1:90], fcasts.r[1:90]), 
             out.sample[c(1:90,366:455)])[,5],
    accuracy(c(fc.exp[91:181], fcasts.r[91:181]),
             out.sample[c(91:181,456:546)])[,5],
    accuracy(c(fc.exp[182:273], fcasts.r[182:273]),
             out.sample[c(182:273,547:638)])[,5],
    accuracy(c(fc.exp[274:365], fcasts.r[274:365]),
             out.sample[c(274:365,639:730)])[,5]))
rownames(qall.eval) <- make.names(c("No_retrain","Retrain"))
colnames(qall.eval) <- make.names(c("Q1","Q2","Q3","Q4"))
print(qall.eval)

# Confidence interval Analysis for Expanding
df <- data.frame(CI)
df$date <-as.Date(data$DATE[(endTrain+1):endValid])

matplot(df$date, cbind(df$lo95,df$hi95), type="l", lty=c(1,1),
        col=c("lightblue","lightblue"), ylim=c(200000, 650000),
        ylab="Sum daily demand (in MWh)", xlab="Date")
polygon(c(df$date, rev(df$date)), c(df$lo95, rev(df$hi95)),
        col = "lightblue", border=F)
polygon(c(df$date, rev(df$date)), c(df$lo80, rev(df$hi80)),
        col = "cyan", border=F)
lines(df$date, df$observed,col="purple")
legend("topright", legend=c("95% CI", "80% CI", "Observed"), lty=1, 
       col=c("lightblue"," cyan", "purple"), lwd=c(5, 5, 1)
)

cat("Report on the prediction interval for Expanding:")
# Calculate the % of the observed values fall into the CI
rep <- rbind(cbind(sum(df$in.CI80), nrow(df), 
                   sum(df$in.CI80)/nrow(df)*100),
             cbind(sum(df$in.CI95), nrow(df), 
                   sum(df$in.CI95)/nrow(df)*100))
rownames(rep) <- make.names(c("Prediction interval 80%",
                              "Prediction interval 95%"))
colnames(rep) <- make.names(c("Observed","Total","Percentage"))
print(rep)

# Confidence interval Analysis for Expanding with Retrain
# print("12")
# print(CI.r)
df.r <- data.frame(rbind(CI[1:365,], CI.r))
df.r$date <-as.Date(data$DATE[(endTrain+1):endValid])

## replace the second year of CI by the full single year of CI.r

matplot(df.r$date, cbind(df.r$lo95,df.r$hi95), type="l", lty=c(1,1),
        col=c("lightblue","lightblue"), ylim=c(200000, 650000),
        ylab="Sum daily demand (in MWh)", xlab="Date")
polygon(c(df.r$date, rev(df.r$date)), c(df.r$lo95, rev(df.r$hi95)),
        col = "lightblue", border=F)
polygon(c(df.r$date, rev(df.r$date)), c(df.r$lo80, rev(df.r$hi80)),
        col = "cyan", border=F)
lines(df.r$date, df.r$observed,col="purple")
legend("topright", legend=c("95% CI", "80% CI", "Observed"), lty=1, 
       col=c("lightblue"," cyan", "purple"), lwd=c(5, 5, 1)
)

cat("Report on the prediction interval for Expanding 
    with retraining:")
# Calculate the % of the observed values fall into the CI
rep.r <- rbind(cbind(sum(df.r$in.CI80), nrow(df.r), 
                   sum(df.r$in.CI80)/nrow(df.r)*100),
             cbind(sum(df.r$in.CI95), nrow(df.r), 
                   sum(df.r$in.CI95)/nrow(df.r)*100))
rownames(rep.r) <- make.names(c("Prediction interval 80%",
                              "Prediction interval 95%"))
colnames(rep.r) <- make.names(c("Observed","Total","Percentage"))
print(rep.r)

# analysis val per quarter

rep.tbats_val_quarter <- rbind(
cbind(sum(df$in.CI80[c(1:90,366:455)]), 180,
sum(df$in.CI80[c(1:90,366:455)])/180*100),
cbind(sum(df$in.CI80[c(91:181,456:546)]), 180,
sum(df$in.CI80[c(91:181,456:546)])/180*100),
cbind(sum(df$in.CI80[c(182:273,547:638)]), 180,
sum(df$in.CI80[c(182:273,547:638)])/180*100),
cbind(sum(df$in.CI80[c(274:365,639:730)]), 180,
sum(df$in.CI80[c(274:365,639:730)])/180*100),
cbind(sum(df$in.CI95[c(1:90,366:455)]), 180,
sum(df$in.CI95[c(1:90,366:455)])/180*100),
cbind(sum(df$in.CI95[c(91:181,456:546)]), 180,
sum(df$in.CI95[c(91:181,456:546)])/180*100),
cbind(sum(df$in.CI95[c(182:273,547:638)]), 180,
sum(df$in.CI95[c(182:273,547:638)])/180*100),
cbind(sum(df$in.CI95[c(274:365,639:730)]), 180,
sum(df$in.CI95[c(274:365,639:730)])/180*100))


rownames(rep.tbats_val_quarter) <- make.names(c("CI 80% Q1",
                                         "CI 80% Q2",
                                         "CI 80% Q3", 
                                         "CI 80% Q4", 
                                         "CI 95% Q1",
                                         "CI 95% Q2",
                                         "CI 95% Q3", 
                                         "CI 95% Q4"))
colnames(rep.tbats_val_quarter) <- make.names(c("Observed",
                                          "Total","Percentage"))
print(rep.tbats_val_quarter)


dev.off(dev.cur())

## SUMMARY

# EXPANDING WINDOW
#            Overall_MAPE
# No_retrain     5.360130
# Retrain        5.347029

#              ME  RMSE   MAE    MPE MAPE    ACF1 Theil's U
# No_retrain  634 28477 20721 -0.374 5.36 0.00861     0.845
# Retrain    1042 28640 20752 -0.272 5.35 0.02930     0.848

#                  Q1       Q2       Q3       Q4
# No_retrain 6.408772 5.003329 4.570991 5.476346
# Retrain    6.213673 4.966751 4.718078 5.504322

# #                         Observed Total Percentage
# Prediction.interval.80.      606   730   83.01370
# Prediction.interval.95.      687   730   94.10959


# TBATS quarter validation analysis
#           Observed Total Percentage
# CI.80..Q1      142   180       78.9
# CI.80..Q2      150   180       83.3
# CI.80..Q3      158   180       87.8
# CI.80..Q4      156   180       86.7
# CI.95..Q1      162   180       90.0
# CI.95..Q2      176   180       97.8
# CI.95..Q3      174   180       96.7
# CI.95..Q4      175   180       97.2


## Final Ranking of methods and models
## amongst the smoothing methods/models, the state space models
## and the TBATS

# 1. TBATS with retrain (expanding window) : 5.347029
# 2. No retrain TBATS (expanding window) : 5.360130
# 3. ETS(A, Ad, A) : MAPE: 5.838487
# 4. Holt Winters : MAPE: 5.840536
# Part 1 Baseline: naive no change: MAPE: 6.34

# DM test

# The previous best in state space models was ETS(A, Ad, A)
# so let us compare with TBATS with retrain to see if they 
# are different

print((dm.test(c(fc.exp[1:365],fcasts.r)-c(out.sample), 
              c(ets.valnew)-c(out.sample))))


# data:  
# c(fc.exp[1:365], fcasts.r) - c(out.sample)
# c(ets.valnew) - c(out.sample)
# DM = -5, Forecast horizon = 1, Loss function power = 2, p-value =
# 0.000002
# alternative hypothesis: two.sided
## We can reject the null hypothesis that the quadratic average error
## on the forecast is the same between ETSHolt Winters with damped
# trend and additive error and TBATS with retraining.
# Thus TBATS with retraining can be selected over ETS(A, Ad, A)

# DM Test TBATS vs benchmark

print((dm.test(c(fc.exp[1:365],fcasts.r)-c(out.sample), 
              c(c(forecast_next_day))-c(out.sample))))

#        Diebold-Mariano Test

# data:  c(fc.exp[1:365], fcasts.r) 
# - c(out.sample)c(c(forecast_next_day)) - c(out.sample)
# DM = 10, Forecast horizon = 1, Loss function power = 2, p-value
# <0.0000000000000002
# alternative hypothesis: two.sided



## CONCLUSION SMOOTHING METHODS VS ETS VS TBATS

# # The TBATS model with retraining can be selected
# # with forecast interval as follows:
#                         Observed Total Percentage
# Prediction.interval.80.      605   730       82.9
# Prediction.interval.95.      686   730       94.0

### TBATS (RETRAINED TBATS WITH MAPE at 5.34) ANALYSIS TEST ###

startTest <- endValid + 1
endTest <- 3652

testSample <- msts(window(SOMME.ts, startTest, endTest),
                   seasonal.periods=c(7,365.25), start=c(2020,1))

n_last <- length(out.sample)
fcasts.r_test <- msts(numeric(n_last), seasonal.periods=c(7,365.25), 
                 start=c(2020,1))
CI.r_test <- matrix(nrow=n_last, ncol=8)
colnames(CI.r_test) <- c("observed","lo80","hi80","in CI80",
                  "lo95","hi95","in CI95","forecast")

for (i in 1:n_last) {

  tmp.msts_test <- msts(window(SOMME.ts, begTrain, (endValid+i-1)), 
                   seasonal.periods=c(7, 365.25))
  f.r.tbat_test <- forecast(tmp.msts_test, model=tfit.r, h=1)
  fcasts.r_test[i] <- f.r.tbat_test$mean[1]

  # confidence interval
  in80.r_test <- (testSample[i] >= f.r.tbat_test$lower[1] &&
             testSample[i] <= f.r.tbat_test$upper[1])
  in95.r_test <- (testSample[i] >= f.r.tbat_test$lower[2] &&
             testSample[i] <= f.r.tbat_test$upper[2])
  CI.r_test[i,] <- c(testSample[i], f.r.tbat_test$lower[1], 
              f.r.tbat_test$upper[1], in80.r_test, 
              f.r.tbat_test$lower[2], 
              f.r.tbat_test$upper[2], in95.r_test, fcasts.r_test[i])
}

cat("Overall Test Performance of TBATS:","\n")
test.eval_tbats <- rbind(accuracy(fcasts.r_test, testSample))
rownames(test.eval_tbats) <- make.names(c("Test Set"))
print(test.eval_tbats)


## By quarters 
cat("Overall Quarterly Test Performance:","\n")
qall.eval_test <- rbind(
  cbind(
    accuracy(fcasts.r_test[c(1:90,366:455)], 
             testSample[c(1:90,366:455)])[,5],
    accuracy(fcasts.r_test[c(91:181,456:546)], 
             testSample[c(91:181,456:546)])[,5],
    accuracy(fcasts.r_test[c(182:273,547:638)], 
             testSample[c(182:273,547:638)])[,5],
    accuracy(fcasts.r_test[c(274:365,639:730)], 
             testSample[c(274:365,639:730)])[,5]))
rownames(qall.eval_test) <- make.names(c("Test Performance"))
colnames(qall.eval_test) <- make.names(c("Q1","Q2","Q3","Q4"))
print(qall.eval_test)

## 2020 vs 2021

cat("Overall Separate Yearly Test Performance:","\n")
all.eval_test_sep_year <- rbind(
  cbind(
    accuracy(fcasts.r_test[c(1:365)], 
             testSample[c(1:365)])[,5],
    accuracy(fcasts.r_test[c(366:730)], 
             testSample[c(366:730)])[,5]))
rownames(all.eval_test_sep_year) <- make.names(c("Test Performance"))
colnames(all.eval_test_sep_year) <- make.names(c("2020","2021"))
print(all.eval_test_sep_year)

# each quarter separate
cat("Overall Separate Quarters Test Performance:","\n")
qall.eval_test_sep <- rbind(
  cbind(
    accuracy(fcasts.r_test[c(1:90)], 
             testSample[c(1:90)])[,5],
    accuracy(fcasts.r_test[c(91:181)], 
             testSample[c(91:181)])[,5],
    accuracy(fcasts.r_test[c(182:273)], 
             testSample[c(182:273)])[,5],
    accuracy(fcasts.r_test[c(274:365)], 
             testSample[c(274:365)])[,5],
    accuracy(fcasts.r_test[c(366:455)], 
             testSample[c(366:455)])[,5],
    accuracy(fcasts.r_test[c(456:546)], 
             testSample[c(456:546)])[,5],
    accuracy(fcasts.r_test[c(547:638)], 
             testSample[c(547:638)])[,5],
    accuracy(fcasts.r_test[c(639:730)], 
             testSample[c(639:730)])[,5]))       
rownames(qall.eval_test_sep) <- make.names(c("Test Performance"))
colnames(qall.eval_test_sep) <- make.names(c("Q1 2020","Q2 2020",
                                        "Q3 2020","Q4 2020",
                                        "Q1 2021","Q2 2021",
                                        "Q3 2021","Q4 2021"))
print(qall.eval_test_sep)


## Confidence interval analysis ##

# Confidence interval Analysis for Expanding
df <- data.frame(CI.r_test)
df$date <-as.Date(data$DATE[(startTest):endTest])

matplot(df$date, cbind(df$lo95,df$hi95), type="l", lty=c(1,1),
        col=c("lightblue","lightblue"), ylim=c(200000, 650000),
        ylab="Sum daily demand (in MWh)", xlab="Date")
polygon(c(df$date, rev(df$date)), c(df$lo95, rev(df$hi95)),
        col = "lightblue", border=F)
polygon(c(df$date, rev(df$date)), c(df$lo80, rev(df$hi80)),
        col = "cyan", border=F)
lines(df$date, df$observed,col="purple")
legend("topright", legend=c("95% CI", "80% CI", "Observed"), lty=1, 
       col=c("lightblue"," cyan", "purple"), lwd=c(5, 5, 1)
)

cat("Report on the prediction interval for Expanding:")
# Calculate the % of the observed values fall into the CI
rep.tbats_test <- rbind(cbind(sum(df$in.CI80), nrow(df), 
                   sum(df$in.CI80)/nrow(df)*100),
             cbind(sum(df$in.CI95), nrow(df), 
                   sum(df$in.CI95)/nrow(df)*100))
rownames(rep.tbats_test) <- make.names(c("Prediction interval 80%",
                              "Prediction interval 95%"))
colnames(rep.tbats_test) <- make.names(c("Observed",
                                          "Total","Percentage"))
print(rep.tbats_test)

# exposure by quarters
rep.tbats_test_quarter <- rbind(
cbind(sum(df$in.CI80[c(1:90,366:455)]), 180,
sum(df$in.CI80[c(1:90,366:455)])/180*100),
cbind(sum(df$in.CI80[c(91:181,456:546)]), 180,
sum(df$in.CI80[c(91:181,456:546)])/180*100),
cbind(sum(df$in.CI80[c(182:273,547:638)]), 180,
sum(df$in.CI80[c(182:273,547:638)])/180*100),
cbind(sum(df$in.CI80[c(274:365,639:730)]), 180,
sum(df$in.CI80[c(274:365,639:730)])/180*100),
cbind(sum(df$in.CI95[c(1:90,366:455)]), 180,
sum(df$in.CI95[c(1:90,366:455)])/180*100),
cbind(sum(df$in.CI95[c(91:181,456:546)]), 180,
sum(df$in.CI95[c(91:181,456:546)])/180*100),
cbind(sum(df$in.CI95[c(182:273,547:638)]), 180,
sum(df$in.CI95[c(182:273,547:638)])/180*100),
cbind(sum(df$in.CI95[c(274:365,639:730)]), 180,
sum(df$in.CI95[c(274:365,639:730)])/180*100))


rownames(rep.tbats_test_quarter) <- make.names(c("CI 80% Q1",
                                         "CI 80% Q2",
                                         "CI 80% Q3", 
                                         "CI 80% Q4", 
                                         "CI 95% Q1",
                                         "CI 95% Q2",
                                         "CI 95% Q3", 
                                         "CI 95% Q4"))
colnames(rep.tbats_test_quarter) <- make.names(c("Observed",
                                          "Total","Percentage"))
print(rep.tbats_test_quarter)



### SUMMARY TEST TBATS (RETRAINED TBATS WITH MAPE at 5.34) 
## ANALYSIS TEST

#           ME  RMSE   MAE    MPE MAPE   ACF1 Theil's U
# Test.Set 616 27824 19786 -0.369 5.16 0.0624     0.879

#                    Q1   Q2   Q3   Q4
# Test.Performance 5.48 5.77 4.74 4.67

#                  X2020 X2021
# Test.Performance  5.27  5.06

#             Q1.2020 Q2.2020 Q3.2020 Q4.2020 Q1.2021 Q2.2021 Q3.2021
# Test.Perfore 5.24    5.95     4.7     5.2    5.72    5.59    4.79
#                 Q4.2021
# Test.Perfonce  4.14

#                         Observed Total Percentage
# Prediction.interval.80.      618   730       84.7
# Prediction.interval.95.      683   730       93.6

#           Observed Total Percentage
# CI.80..Q1      154   180       85.6
# CI.80..Q2      147   180       81.7
# CI.80..Q3      163   180       90.6
# CI.80..Q4      154   180       85.6
# CI.95..Q1      163   180       90.6
# CI.95..Q2      169   180       93.9
# CI.95..Q3      178   180       98.9
# CI.95..Q4      173   180       96.1