source("timeseries_x.R")

library(dynlm)

#Function taken from arx.R on ZoneCours for plots analysis

plot.diag <- function(o) { 
  
  par(mfrow=c(2,2))
  plot(o$fitted, o$residuals)
  for (k in 2:dim(o$x)[2]) { 
    plot(o$x[,k], o$residuals, xlab=dimnames(o$x)[2][[1]][k]) }
  qqnorm(o$residuals); abline(a=0, b=1, col="blue")
  acf(o$residuals)
  
}

#Creating model

arx = dynlm(yt ~  + L(yt,1), data=ddis, x=T)