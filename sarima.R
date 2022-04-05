#
# Program: sarima.R
#
# Purpose: exploring SARIMA models for one day ahead forecasting
# Written by: Team G, April 2nd 2022
#
# Updated: April 8th 2022
#          
#         
#          
#         
#          
# ------------------------------------------------------

library(astsa)

pdf("visual_output/sarima_exploration_1.pdf")

# our target variable, yt, is a time series built in the 
# timeseries_x.R script

# our train is yt_train from timeseries_x.R script

# our validation is yt_validation from timeseries_x.R script

plot(yt_train,type="l",
     ylab="Texas Daily Demand in Region North-Central(in MW)")
print("We observe a trend and seasonality in the time series")
print("We can discard ARIMA models then")
print("The time series is not stationary")

# acf on the target variable does not show stationarity
acf2(yt_train)
# we observe slight bumps at lag 7, 14, 21, etc 
# it shows there might be a weekly seasonal effect

# is there a linear relationship between Yt and Yt-1
lag1.plot(yt_train)
print("There is a strong linear dependence between Yt
    and Yt-1 since the correlation is at 0.9")

# looks like it's stationary after first differencing
plot(diff(yt_train,1),type="l",
     ylab="lag 1 difference Daily Texas Daily Demand 
     in Region North-Central(in MW)")
# looks like it's also stationary after differencing at lag 7 
plot(diff(yt_train,7),type="l",
     ylab="lag 7 difference Daily Texas Daily Demand 
     in Region North-Central(in MW)")


# With differencing Yt vs Yt-1 --> no major dependence 
## since corr at 0.09
lag1.plot(diff(yt_train,1)[-1])

# weekly seasonality; it shows we certainly need a 
# weekly differencing in a SARIMA even once we applied a first
# differencing
acf2(diff(yt_train))
print("a simple differencing of order 1 is not enough")

# let us apply a differencing of order 7
acf2(diff(yt_train, 7))



# Does the variance of series vary with the mean level?
# I.e. are data more variable when they take on larger values?
# For example, we can compute means and stdevs by month to check.
YMt.sim.mean <- applySeries(yt_train,by="monthly",FUN=colMeans)
YMt.sim.sd   <- applySeries(yt_train,by="monthly",FUN=colSds)
plot(series(YMt.sim.mean),series(YMt.sim.sd),
     main="Before Box-Cox transformation")
print("We conclude that the higher the level the higher
the variance")
print("A Box Cox transformation might prove to be useful")

# Compute Box-Cox parameter lambda that stabilizes the variance
lambda <- BoxCox.lambda(yt_train)
YMt.yt_train.mean.bc <- applySeries(BoxCox(yt_train,lambda),
                               by="monthly",FUN=colMeans)
YMt.yt_train.sd.bc   <- applySeries(BoxCox(yt_train,lambda),
                               by="monthly",FUN=colSds)

# Has the situation improved?
plot(series(YMt.yt_train.mean.bc),series(YMt.yt_train.sd.bc),
     main=paste("After Box-Cox transformation, lambda=",
     round(lambda,2)))
cat("training data - Box Cox lambda:",lambda,"\n")
acf2(BoxCox(yt_train,lambda))
hist(yt_train,main="Before Box-Cox")
hist(BoxCox(yt_train,lambda),main="After Box-Cox")

print("The Box Cox transformation is useful since it
     transformed the distribution to a more normal shape")

# Box-Cox transformation to stabilize the variance
# lambda computed = -0.649
Yt.bc <- BoxCox(yt_train,lambda)

# do we still have stationarity with the box cox transformed 
# data

# looks like it's stationary after first differencing
plot(diff(Yt.bc,1),type="l",
     ylab="Box Cox Transformed
      lag 1 difference Daily Texas Daily Demand 
     in Region North-Central(in MW)")
# looks like it's also stationary after differencing at lag 7 
plot(diff(Yt.bc,7),type="l",
     ylab="Box Cox Transformed
     lag 7 difference Daily Texas Daily Demand 
     in Region North-Central(in MW)")


# with seasonality = 7; both seasonal acf/pacf decaying: SARIMA 
# model with D =1 and seasonality of 7 could be considered
acf2(diff(Yt.bc))
acf2(diff(yt_train, 7))
print("SARIMA model could be considered given both ACF and 
      PACF are decaying and the multiple of lags 7 are non zero
      and non multiple of 7 tend to be closer to 0")
print("A similar pattern is observed with target transformed
      by Box Cox")
diff7 = diff(Yt.bc,7)
acf2(diff7)
acf2(diff(diff7,1))


# # just try arma and arima to see how it goes
# # Try #1: ARIMA(1,0,0) with no seasonality --> discarded
# # none of the conditions on standardized residuals
# # acf of residuals qq plot or ljung box are respected
# # AIC: -18.6  BIC: -18.6
# f1 <- sarima(Yt.bc, 2,0,2)
# cat("ARIMA(1,0,0) - AIC:",f1$AIC," BIC:",f1$BIC,"\n")

# ## qq plot and standardized residuals do not respect conditions
# # as well as the ACF of Residuals when p = 1, q = 0, d = 0

# # Try #2: ARIMA(1,1,0) with no seasonality --> discarded
# # none of the conditions on standardized residuals
# # acf of residuals qq plot or ljung box are respected
# # AIC: -18.6  BIC: -18.6
# f2 <- sarima(Yt.bc, 1,1,0)
# cat("ARIMA(1,1,0) - AIC:",f2$AIC," BIC:",f2$BIC,"\n")

# we then also discard ARIMA (1, 1, 1) and further
# versions of ARIMA and directly jump into SARIMA


# # Try 1# SARIMA with Box Cox transformed target
# SARIMA(1, 1, 2, 0, 1, 1)[7]
# # AIC: -18.9  BIC: -18.9
s1 <- sarima(diff(Yt.bc, 7),1,1,2,0,0,1,7)
cat("SARIMA(1,1,2,0,1,1)[7] Box Cox
           - AIC:",s1$AIC," BIC:",s1$BIC,"\n")

# Try #2: SARIMA(1,1,2,1,0,1)[7]
# AIC: -18.9  BIC: -18.9
s2 <- sarima(Yt.bc,1,1,2,1,0,1,7)
cat("SARIMA(1,1,2,1,0,1)[7] Box Cox
                          - AIC:",s2$AIC," BIC:",s2$BIC,"\n")

# # Try #3 SARIMA with NON Box Cox transformed target
# # AIC: -18.9  BIC: -18.9
s1_nobc <- sarima(diff(yt_train, 7),1,1,2,0,0,1,7)
cat("SARIMA(1,1,2,0,1,1)[7] No Box Cox - AIC:",s1_nobc$AIC,
          " BIC:",s1_nobc$BIC,"\n")

# Try # 4: SARMA(1,1,2,1,0,7)[7]
# AIC: -18.9  BIC: -18.9
s2_nobc  <- sarima(yt_train,1,1,2,1,0,1,7)
cat("SARIMA(1,1,2,1,0,1)[7] No Box Cox - AIC:",s2_nobc$AIC,
                              " BIC:",s2_nobc$BIC,"\n")

# Try #4
# s4 <- sarima(diff(Yt.bc, 7),0,0,2,0,0,1,7)
# cat("SARIMA(0,0,2,0,1,1)[7] Box Cox
#            - AIC:",s4$AIC," BIC:",s4$BIC,"\n")
# Try #5
# s5 <- sarima(diff(Yt.bc, 7),0,0,1,0,0,1,7)
# cat("SARIMA(0,0,1,0,1,1)[7] Box Cox
#            - AIC:",s5$AIC," BIC:",s5$BIC,"\n")
# Try #6
# s6 <- sarima(diff(Yt.bc, 7),0,0,0,0,0,1,7)
# cat("SARIMA(0,0,0,0,1,1)[7] Box Cox
#            - AIC:",s6$AIC," BIC:",s6$BIC,"\n")
# Try #7
# s7 <- sarima(diff(Yt.bc, 7),1,0,0,0,0,1,7)
# cat("SARIMA(1,0,0,0,1,1)[7] Box Cox
#            - AIC:",s7$AIC," BIC:",s7$BIC,"\n")
# Try #8
# s8 <- sarima(diff(Yt.bc, 7),2,0,0,0,0,1,7)
# cat("SARIMA(2,0,0,0,1,1)[7] Box Cox
#            - AIC:",s8$AIC," BIC:",s8$BIC,"\n")
# Try #9
# s9 <- sarima(diff(Yt.bc, 7),1,1,2,0,0,1,7)
# cat("SARIMA(2,0,1,0,1,1)[7] Box Cox
#            - AIC:",s9$AIC," BIC:",s9$BIC,"\n")


# Our model of choice will be SARIMA(1,1,2,0,1,1)[7] with box cox
# (lambda computed = -0.649)
# because it has the lowest AIC/BIC and the diagnostic plots
# show that all conditions for SARIMA to work are fulfilled

#graphics.off()
dev.off(dev.cur())

### NO RETRAIN ###
n <- length(yt_valid)
fc1_no_retrain_exp <- ts(numeric(n))
fc2_no_retrain_mov <- ts(numeric(n))

CI.exp_no_ret <- matrix(nrow=n, ncol=5)
colnames(CI.exp_no_ret) <- c("lo95","hi95",
                              "forecast","observed","in CI95")

CI.mov_no_ret <- matrix(nrow=n, ncol=5)
colnames(CI.mov_no_ret) <- c("lo95","hi95",
                              "forecast","observed","in CI95")

### NO RETRAIN ###
for(i in 1:n) {
  s1.for <- sarima.for(window(SOMME.ts, begTrain, (endTrain+i-1)),
                       n.ahead=1,
                       p=1,d=1,q=2,P=0,D=1,Q=1,S=7,
                       fixed=c(s1$fit$coef[1],
                               s1$fit$coef[2],
                               s1$fit$coef[3],
                               s1$fit$coef[4]
                               ))
  fc1_no_retrain_exp[i] <- ts(s1.for$pred)
  # prediction interval expanding window
  lo.s <- s1.for$pred-1.96*s1.for$se
  hi.s <- s1.for$pred+1.96*s1.for$se
  in95.s <- (yt_valid[i] >= lo.s && yt_valid[i] <= hi.s)
  CI.exp_no_ret[i,] <- c(lo.s, hi.s, 
                         s1.for$pred, yt_valid[i], in95.s)
  ### moving window ####
  s2.for <- sarima.for(window(SOMME.ts, begTrain+i-1, 
                                             (endTrain+i-1)),
                       n.ahead=1,
                       p=1,d=1,q=2,P=0,D=1,Q=1,S=7,
                       fixed=c(s1$fit$coef[1],
                               s1$fit$coef[2],
                               s1$fit$coef[3],
                               s1$fit$coef[4]
                               ))
  fc2_no_retrain_mov[i] <- ts(s2.for$pred)
  # prediction interval expanding window
  lo.s2 <- s2.for$pred-1.96*s2.for$se
  hi.s2 <- s2.for$pred+1.96*s2.for$se
  in95.s2 <- (yt_valid[i] >= lo.s2 && yt_valid[i] <= hi.s2)
  CI.mov_no_ret[i,] <- c(lo.s2, hi.s2, 
                         s2.for$pred, yt_valid[i], in95.s2)

}

# Performance evaluation
cat("Performance of SARIMA models expanding and moving windows:
     ","\n")

sarima.eval <- rbind(accuracy(fc1_no_retrain_exp, 
                              yt_valid)[,1:5],
                     accuracy(fc2_no_retrain_mov, 
                              yt_valid)[,1:5])

rownames(sarima.eval) <- make.names(
     c("SARIMA(1,1,2,0,1,1)[7] Expanding Window No retrain",
       "SARIMA(1,1,2,0,1,1)[7] Moving Window No Retrain"))
print(sarima.eval)


cat("Quarterly Performance:","\n")
q.eval <- rbind(
  #SARIMA(1,1,2,0,1,1)[7] Expanding Window No retrain
  cbind(
    accuracy(fc1_no_retrain_exp[c(1:90,366:455)], 
             yt_valid[c(1:90,366:455)])[,5],
    accuracy(fc1_no_retrain_exp[c(91:181,456:546)], 
             yt_valid[c(91:181,456:546)])[,5],
    accuracy(fc1_no_retrain_exp[c(182:273,547:638)], 
             yt_valid[c(182:273,547:638)])[,5],
    accuracy(fc1_no_retrain_exp[c(274:365,639:730)], 
             yt_valid[c(274:365,639:730)])[,5]),
  #SARIMA(1,1,2,0,1,1)[7] Moving Window No Retrain
  cbind(
    accuracy(fc2_no_retrain_mov[c(1:90,366:455)], 
             yt_valid[c(1:90,366:455)])[,5],
    accuracy(fc2_no_retrain_mov[c(91:181,456:546)], 
             yt_valid[c(91:181,456:546)])[,5],
    accuracy(fc2_no_retrain_mov[c(182:273,547:638)], 
             yt_valid[c(182:273,547:638)])[,5],
    accuracy(fc2_no_retrain_mov[c(274:365,639:730)], 
             yt_valid[c(274:365,639:730)])[,5]))
rownames(q.eval) <- make.names(c("ARMA(1,1,2)",
                                    "SARIMA(1,1,2,2,0,0)[7]"))
rownames(q.eval) <- make.names(
     c("SARIMA(1,1,2,0,1,1)[7] Expanding Window No retrain",
       "SARIMA(1,1,2,0,1,1)[7] Moving Window No Retrain"))

colnames(q.eval) <- make.names(c("Q1","Q2","Q3","Q4"))
print(q.eval)

## The quarterly performance is exactly the same across quarters


# Prediction interval for SARIMA(1,1,2)(2,0,0) [7] Expanding
#
CI <- CI.exp_no_ret
df <- data.frame(CI)
df$date <-as.Date(data$DATE[(endTrain+1):endValid])

matplot(df$date, cbind(df$lo95,df$hi95), type="l", lty=c(1,1),
        col=c("lightblue","lightblue"), ylim=c(200000, 600000),
        ylab="WFEC daily peak demand (in MWh)", xlab="Date")
polygon(c(df$date, rev(df$date)), c(df$lo95, rev(df$hi95)),
        col = "lightblue", border=F)
lines(df$date, df$observed,col="purple")
legend("topright", legend=c("95% Pred. Interval", "Observed"), lty=1, 
       col=c("lightblue","purple"), lwd=c(5, 5, 1))

cat("Report on the prediction interval:")
# Calculate the % of the observed values fall into the CI
rep <- rbind(cbind(sum(df$in.CI95), nrow(df), 
                   sum(df$in.CI95)/nrow(df)*100))
rownames(rep) <- make.names(c("Prediction interval 95% 
                              Expanding Window No retrain"))
colnames(rep) <- make.names(c("Observed","Total","Percentage"))
print(rep)


# Prediction interval for SARIMA(1,1,2)(2,0,0) [7] Moving
#
CI <- CI.mov_no_ret
df <- data.frame(CI)
df$date <-as.Date(data$DATE[(endTrain+1):endValid])

matplot(df$date, cbind(df$lo95,df$hi95), type="l", lty=c(1,1),
        col=c("lightblue","lightblue"), ylim=c(200000, 600000),
        ylab="WFEC daily peak demand (in MWh)", xlab="Date")
polygon(c(df$date, rev(df$date)), c(df$lo95, rev(df$hi95)),
        col = "lightblue", border=F)
lines(df$date, df$observed,col="purple")
legend("topright", legend=c("95% Pred. Interval", "Observed"), lty=1, 
       col=c("lightblue","purple"), lwd=c(5, 5, 1))

cat("Report on the prediction interval:")
# Calculate the % of the observed values fall into the CI
rep <- rbind(cbind(sum(df$in.CI95), nrow(df), 
                   sum(df$in.CI95)/nrow(df)*100))
rownames(rep) <- make.names(c("Prediction interval 95% Moving 
                               Window No retrain"))
colnames(rep) <- make.names(c("Observed","Total","Percentage"))
print(rep)


## WITH DAILY RETRAIN

