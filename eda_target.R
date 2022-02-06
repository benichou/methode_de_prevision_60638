# Program: eda_target.R
#
# Purpose: exploration of the target to check for trends, seasonality
#  and special effects of week ends, stats holidays etc on 
# daily electricity demand
#
# Written by: Team G, January 30 2021
#
# Updated: NA
#          
#         
#          
#         
#          
# ------------------------------------------------------
# upload necessary packages
library(timeSeries)

# launch the data_transformation.R module
source("./exploratory_vars.R")
source("./functions.R")


## Visualize Each Year on a monthly basis with seasonplot

eda_df = data
eda_df["EDA_DATE"] = as.Date(eda_df$DATE, format =  "%Y-%m-%d")
eda_df["YEAR"] = format(eda_df["EDA_DATE"], format = "%Y")
eda_df["MONTH"] = format(eda_df["EDA_DATE"], format = "%m")

agg_eda_df = eda_df[c("NORTH", "EAST", "NCENT", "SOMME", 
                      "YEAR", "MONTH")]

agg_eda_df = aggregate(.~YEAR+MONTH,
                   agg_eda_df,
                   FUN=sum, na.rm = FALSE)

north_vector <- c()
north_cen_vector <- c()
east_vector <- c()
somme_vector <- c()



year_list_2012_17 = list("2012", "2013", "2014", "2015", 
                        "2016", "2017")
year_list = list("2012", "2013", "2014", "2015", "2016", "2017", 
              "2018", "2019", "2020", "2021")
month_list = list("01", "02", "03", "04", "05", "06", "07", "08", 
              "09", "10", "11", "12")


## entire period 2012-21

results_2012_21 = stack_years(agg_eda_df, year_list, month_list, 
                        "YEAR", "MONTH", "NORTH", "NCENT", "EAST",
                        "SOMME",
                        north_vector,
                        north_cen_vector,
                        east_vector,
                        somme_vector)

north_vector_2012_21 = results[1][[1]]
north_cen_vector_2012_21 = results[2][[1]]
east_vector_2012_21 = results[3][[1]]
somme_vector_2012_21 = results[4][[1]]

ts_somme = ts(somme_vector_2012_21, start=2012, frequency=12)
ts_north = ts(north_vector_2012_21, start=2012, frequency=12)
ts_north_cen = ts(north_cen_vector_2012_21, start=2012, frequency=12)
ts_east = ts(east_vector_2012_21, start=2012, frequency=12)

seasonplot(ts_somme, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north_cen, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_east, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

## 2012 -2017

results_2012_17 = stack_years(agg_eda_df, year_list_2012_17, 
                              month_list, 
                        "YEAR", "MONTH", "NORTH", "NCENT", "EAST",
                        "SOMME",
                        north_vector,
                        north_cen_vector,
                        east_vector,
                        somme_vector)

north_vector_2012_17 = results_2012_17[1][[1]]
north_cen_vector_2012_17 = results_2012_17[2][[1]]
east_vector_2012_17 = results_2012_17[3][[1]]
somme_vector_2012_17 = results_2012_17[4][[1]]

ts_somme = ts(somme_vector_2012_17, start=2012, frequency=12)
ts_north = ts(north_vector_2012_17, start=2012, frequency=12)
ts_north_cen = ts(north_cen_vector_2012_17, start=2012, frequency=12)
ts_east = ts(east_vector_2012_17, start=2012, frequency=12)

seasonplot(ts_somme, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north_cen, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_east, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")











for (i in year_list) {
     df1 = agg_eda_df[agg_eda_df["YEAR"] == i, ] 
     for (j in month_list) {
          north_value = df1[df1["MONTH"] == j, "NORTH"]
          north_cen_value = df1[df1["MONTH"] == j, "NCENT"]
          east_value = df1[df1["MONTH"] == j, "EAST"]
          somme_value = df1[df1["MONTH"] == j, "SOMME"]
          north_vector = c(north_vector, north_value)
          north_cen_vector = c(north_cen_vector, north_cen_value)
          east_vector = c(east_vector, east_value)
          somme_vector = c(somme_vector, somme_value)  
     }
     }
ts_somme = ts(somme_vector, start=2012, frequency=12)
ts_north = ts(north_vector, start=2012, frequency=12)
ts_north_cen = ts(north_cen_vector, start=2012, frequency=12)
ts_east = ts(east_vector, start=2012, frequency=12)

seasonplot(ts_somme, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north_cen, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_east, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")
## continue the seasonplot but on shorter periods!

# Create the for statement



seasonplot(AirPassengers, col=rainbow(12), year.labels=TRUE)


#Creating timeseries
sdate = c(2012,1)
data_ts = ts(data$SOMME, start=sdate, frequency=365.25)

#grahpique de la demande totale jounralière d'électricité 01-01-2012 
#au 30-12-2021 pour la région totale
plot(data_ts, 
     ylab="Texas Daily Demand (in MW)")

#il y a bien une saisonnalité annuelle de la demande jounralière 
#d'électricité dans nos 3 régions du texas qui reste relativement 
#constante. Les plus hauts pics ont l'air de se dérouler en éte
#les plus bas pics ont quant à aux l'air d'être en hiver quand il y 
#en a (e.g. : hiver 2016 très doux ? Voir avec données méteo)


#pour la région North
data_ts_n = ts(data$NORTH, start=sdate, frequency=365.25)
plot(data_ts_n, 
     ylab="Texas Daily Demand in Region North(in MW)")
#fort drop en début 2020
#pas de tendance à la hausse, constante voir même début de tendance 
# à la baisse ?

#pour la région Nort-Central
data_ts_nc = ts(data$NCENT, start=sdate, frequency=365.25)
plot(data_ts_nc, 
     ylab="Texas Daily Demand in Region North-Central(in MW)")

#pour le région East
data_ts_e = ts(data$EAST, start=sdate, frequency=365.25)
plot(data_ts_e, 
     ylab="Texas Daily Demand in Region East (in MW)")

#pic hiver 2021 et 2018, faible demande en 2013, beaucoup plus de
#variations pour le demande hivernale que d'été
#legère tendance à la hausse ces dernière années, essayer de voir ça 
#via le delta ? A cause de quoi ? Cette tendance ne se reflète pas 
#dans les autres régions

# check out analysis per weather type events 
t_med = ((data$TMAX + data$TMIN)/2)
sdate_met = c(2012,1)
t_med_ts = ts(t_med, start=sdate_met, frequency=365.25)

plot(t_med_ts, 
     ylab="Texas Daily Temperature (in Celsius)")

#hiver 2021 a été le plus froid ce qui corrobore avec la forte demande 
#et avec l'hiver rude qui a mené à la panne générale (Ted Cruz)
#l'hiver 2016 et 2020 ont été les plus doux en effet
# max été constant alors que min hiver non-constant

#Snow Depth Time Series
snwd_ts = ts(data$SNWD, start=sdate_met, frequency=365.25)
plot(snwd_ts, 
     ylab="Texas Daily Snow Depth (in Celsius)")

#ont eus de la neige au texas seulement en 2013, 2015 et février 2021


## check weekends with combined effect of stats holidays



# check for outlier values annual, monthly basis



# check for seasonality annual monthly and weekly
### all regions, separate regions
# --> done annually but not monthly for all, monthly by region 
#necessary ? 

## check for delta (growth or not) of electricity demand over time

## check for overall trend over the overall period, 
### all regions and each separate regions



## analyze days when the military go out of their 3 bases in 
### our region,
#haven't found when they go out yet, doesn't seem like we 
#have weeks where the daily demand of electricity is impacted by 
#military trainings 


## check out analysis per week end --> need help for coding that one, 
## statutory holidays, 





## do the analysis to make the choice of traning, validation, and
## test











