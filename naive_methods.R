# upload necessary packages
library(forecast)
library(timeSeries)

# launch the data_transformation.R module
source("./data_transformation.R")

# Naive forecast draft

#Creating timeseries
sdate = data$Date[1]
edate = data$Date[3652]
data_agg_ts = ts(data_agg$Somme, start=sdate, end = edate, 
                 frequency=365.25)

ffcast = data$Date[2]
naive_next_day = naive(data_agg_ts, h=1)

forecast <- window(naive_next_day$fitted, start=ffcast) 
observed <- window(naive_next_day$x, start=ffcast)

#naif, naive seasonal 7 jours , moyenne mobile, random walk.
