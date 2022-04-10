#Visualisation des forecast VS observed pour TBATS et Auto.arima

<<<<<<< HEAD
=======
load("plot_final.Rdata")

>>>>>>> dev
f.r.tbats_valid = msts(tail(f.r.tbat$fitted, length(out.sample)),
                       seasonal.periods=c(7,365.25), start=c(2018,1))
f.auto.arima_valid = msts(joe_df$fit, seasonal.periods=c(7,365.25),
                          start=c(2018,1))

plot(out.sample[1:183], 
     type="l", lwd=2, xlab="Jours depuis 1er janvier 2018", 
     ylab = "Somme de la demande (MW/h)",
     main="Previsions VS observations pour TBATS, auto.arima")
#lines(naive_next_day$fitted[1:183],
#     type="l" , col="palegreen3", lwd=2)
lines(f.r.tbats_valid[1:183],
      type="l" ,col="red", lwd=2, lty=2)
lines(f.auto.arima_valid [1:183],
      type="l" ,col="lightsteelblue3", lwd=2, lty = 2)
legend(x="topright", legend=c("Observations",
              "TBATS - Retrained Once","auto.arima - Daily Retrain"), 
       col=c("black", "red", "lightsteelblue3"), 
       lty=1, bg="light blue", cex=0.8)
  
  