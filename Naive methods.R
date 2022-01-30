library(forecast)
library(timeSeries)

# Naive forecast

#Creating timeseries
sdate = data_agg$Date[1]
edate = data_agg$Date[3652]
data_agg_ts = ts(data_agg$Somme, start=sdate, end = edate, frequency=365.25)

ffcast = data_agg$Date[2]
naive_next_day = naive(data_agg_ts, h=1)

forecast <- window(naive_next_day$fitted, start=ffcast) 
observed <- window(naive_next_day$x, start=ffcast)