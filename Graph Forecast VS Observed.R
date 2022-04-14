#Visualisation des forecast VS observed pour TBATS et Auto.arima


load("plot_final.Rdata")

f.r.tbats_valid = msts(tail(f.r.tbat$fitted, length(out.sample)),
                       seasonal.periods=c(7,365.25), start=c(2018,1))
f.auto.arima_valid = msts(joe_df$fit, seasonal.periods=c(7,365.25),
                          start=c(2018,1))

#Needed for PI analysis
#TBATS
out.CI = c()

for (i in 1:nrow(CI)) {
  if (CI[i,7] == 0) {
  out.CI[i] <- CI[i,1]
  }}

CI2 = cbind(CI, out.CI)

#ARX
out.CI.ARX = c()

for (i in 1:length(cv_exp_daily)) {
  if (cv_exp_daily[i] == 0) {
    out.CI.ARX[i] <- yt_valid[i]
  }}

out.CI.ARX = c(rep(NA,7), out.CI.ARX)

#TBATS Q1
par(mfrow=c(2,1))

plot(CI2[,1][1:91], 
     type="l", lwd=2, xlab="Jours depuis 1er janvier 2018", 
     ylab = "Somme de la demande (MWh)",
     main="Observations VS intervalles de pr??dictions 95% -Q1 2018")
lines(CI2[,5][1:91],
      type="l" ,col="blue", lwd=1, lty=2)
lines(CI2[,6][1:91],
      type="l" ,col="blue", lwd=1, lty=2)
points(CI2[,9][1:91],
      type="o" ,col="red", lwd=1)
legend(x="topright", legend=c("Observations",
              "TBATS 95% intervalle pred."), 
       col=c("black", "blue"), 
       lty=1, bg="light blue", cex=0.8)

#TBATS Q2
plot(CI2[,1][92:183], 
     type="l", lwd=2, xlab="Jours depuis 1er avril 2018", 
     ylab = "Somme de la demande (MWh)",
     main="Observations VS intervalles de pr??dictions 95% -Q2 2018")
lines(CI2[,5][92:183],
      type="l" ,col="blue", lwd=1, lty=2)
lines(CI2[,6][92:183],
      type="l" ,col="blue", lwd=1, lty=2)
lines(CI2[,9][92:183],
      type="o" ,col="red", lwd=1)
legend(x="bottomright", legend=c("Observations",
                              "TBATS 95% intervalle pred."), 
       col=c("black", "blue"), 
       lty=1, bg="light blue", cex=0.8)

#ARX Q3
par(mfrow=c(2,1))
plot(CI2[,1][182:273], 
     type="l", lwd=2, xlab="Jours depuis 1er juillet 2018", 
     ylab = "Somme de la demande (MWh)",
     main="Observations VS intervalles de pr??dictions 95% -Q3 2018")
lines(pred_exp_lwr[182:273],
      type="l" ,col="blue", lwd=1, lty=2)
lines(pred_exp_upr[182:273],
      type="l" ,col="blue", lwd=1, lty=2)
legend(x="bottomleft", legend=c("Obs.",
                                 "95%"), 
       col=c("black", "blue"), 
       lty=1, bg="light blue", cex=0.8)

#ARX Q4
plot(CI2[,1][274:365], 
     type="l", lwd=2, xlab="Jours depuis 1er octobre 2018", 
     ylab = "Somme de la demande (MWh)",
     main="Observations VS intervalles de pr??dictions 95% -Q4 2018")
lines(pred_exp_lwr[274:365],
      type="l" ,col="blue", lwd=1, lty=2)
lines(pred_exp_upr[274:365],
      type="l" ,col="blue", lwd=1, lty=2)
legend(x="bottomleft", legend=c("Obs.",
                                "95%"), 
       col=c("black", "blue"), 
       lty=1, bg="light blue", cex=0.8)
