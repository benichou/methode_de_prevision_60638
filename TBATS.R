# Program: TBATS.R
#
# Purpose: Adjusts the TBATS model between moving and expanding 
# window with and without retraining
# 
#
# Written by: Team G, March 12 2022
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
source('dataAggregation.r')

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

rownames(Q.eval) <- make.names(c("Q1_2017","Q2_2017","Q3_2017",
                                 "Q4_2017","Q1_2018","Q2_2018",
                                 "Q3_2018","Q4_2018"))
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


# ==================================================================
#   Re-train after 1 year
# ==================================================================
in.sample.r  <- msts(window(SOMME.ts, begTrain, endTrain+365),
                     seasonal.periods=c(7,365.25), start=c(2012,1))
out.sample.r <- msts(window(SOMME.ts, endTrain+366, endValid),
                     seasonal.periods=c(7,365.25), start=c(2019,1))

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
}

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

# ==================================================================
# Overall evaluation (with/without annual re-training)
# ==================================================================
cat("Overall Performance of TBATS:","\n")
all.eval <- rbind(accuracy(fc.exp, out.sample)[,5],
                   accuracy(c(fc.exp[1:365], fcasts.r[1:365]),
                            out.sample)[,5])
rownames(all.eval) <- make.names(c("No_retrain", "Retrain"))
colnames(all.eval) <- make.names("Overall_MAPE")
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

# Confidence interval
df <- data.frame(CI)
df$date <-as.Date(data$DATE[(endTrain+1):endValid])

matplot(df$date, cbind(df$lo95,df$hi95), type="l", lty=c(1,1),
        col=c("lightblue","lightblue"), ylim=c(700, 2000),
        ylab="WFEC daily peak demand (in MWh)", xlab="Date")
polygon(c(df$date, rev(df$date)), c(df$lo95, rev(df$hi95)),
        col = "lightblue", border=F)
polygon(c(df$date, rev(df$date)), c(df$lo80, rev(df$hi80)),
        col = "cyan", border=F)
lines(df$date, df$observed,col="purple")
legend("topright", legend=c("95% CI", "80% CI", "Observed"), lty=1, 
       col=c("lightblue"," cyan", "purple"), lwd=c(5, 5, 1)
)

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


dev.off(dev.cur())