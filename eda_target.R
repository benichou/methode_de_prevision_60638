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
library(forecast)

# launch the data_transformation.R module
source("./exploratory_vars.R")
source("./functions.R")

pdf("./visual_output/eda_target_visual_output.pdf")
## Visualize Each Year on a monthly basis with seasonplot

#Creating timeseries
sdate = c(2012,1)
data_ts = ts(data$SOMME, start=sdate, frequency=365.25)

#grahpique de la demande totale jounralière d'électricité 01-01-2012 
#au 30-12-2021 pour la région totale
plot(data_ts, col = "blue", 
     main = "Evolution of Daily Electricity Demand over time",
     ylab="Texas Daily Demand (in MW)")

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


eda_df = data
eda_df["EDA_DATE"] = as.Date(eda_df$DATE, format =  "%Y-%m-%d")
eda_df["YEAR"] = format(eda_df["EDA_DATE"], format = "%Y")
eda_df["MONTH"] = format(eda_df["EDA_DATE"], format = "%m")

agg_eda_df = eda_df[c("NORTH", "EAST", "NCENT", "SOMME", 
                      "YEAR", "MONTH")]

agg_eda_df = aggregate(.~YEAR+MONTH,
                   agg_eda_df,
                   FUN=sum, na.rm = FALSE)
eda_df["SEASON"] = "0"
eda_df[eda_df["MONTH"] == "12", "SEASON"] = "WINTER"
eda_df[eda_df["MONTH"] == "01", "SEASON"] = "WINTER"                         
eda_df[eda_df["MONTH"] == "02", "SEASON"] = "WINTER" 
eda_df[eda_df["MONTH"] == "03", "SEASON"] = "SPRING"
eda_df[eda_df["MONTH"] == "04", "SEASON"] = "SPRING"
eda_df[eda_df["MONTH"] == "05", "SEASON"] = "SPRING"
eda_df[eda_df["MONTH"] == "06", "SEASON"] = "SUMMER"
eda_df[eda_df["MONTH"] == "07", "SEASON"] = "SUMMER"
eda_df[eda_df["MONTH"] == "08", "SEASON"] = "SUMMER"
eda_df[eda_df["MONTH"] == "09", "SEASON"] = "FALL"
eda_df[eda_df["MONTH"] == "10", "SEASON"] = "FALL"
eda_df[eda_df["MONTH"] == "11", "SEASON"] = "FALL"

agg_eda_season_df = eda_df[c("NORTH", "EAST", "NCENT", "SOMME", 
                      "YEAR", "SEASON")]

agg_eda_season_df = aggregate(.~YEAR+SEASON,
                   agg_eda_season_df,
                   FUN=mean, na.rm = FALSE)

## Fall season :
fall_df = agg_eda_season_df[
        agg_eda_season_df["SEASON"] == "FALL", -c(1, 2)]
fall_df_summary = summary(fall_df)
std_fall = colSds(as.matrix(fall_df[
       sapply(fall_df, is.numeric)]))
mean_fall = colMeans(as.matrix(fall_df[
            sapply(fall_df, is.numeric)]))

# winter season:
winter_df = agg_eda_season_df[
        agg_eda_season_df["SEASON"] == "WINTER", -c(1, 2)]
winter_df_summary = summary(winter_df)
std_winter = colSds(as.matrix(winter_df[
       sapply(winter_df, is.numeric)]))
mean_winter = colMeans(as.matrix(winter_df[
            sapply(fall_df, is.numeric)]))
# spring season:
spring_df = agg_eda_season_df[
        agg_eda_season_df["SEASON"] == "SPRING", -c(1, 2)]
spring_df_summary = summary(spring_df)
std_spring = colSds(as.matrix(spring_df[
       sapply(spring_df, is.numeric)]))
mean_spring = colMeans(as.matrix(spring_df[
            sapply(fall_df, is.numeric)]))
# summer season:
summer_df = agg_eda_season_df[
        agg_eda_season_df["SEASON"] == "SUMMER", -c(1, 2)]
summer_df_summary = summary(summer_df)
std_summer = colSds(as.matrix(summer_df[
       sapply(summer_df, is.numeric)]))
mean_summer = colMeans(as.matrix(summer_df[
            sapply(fall_df, is.numeric)]))

## what is the standard deviation and mean of the daily 
## electric demand 
## for each region in each season?


std_summary_seasons <- data.frame(REGION = c("NORTH", "EAST", 
                                             "NCENT", "SOMME"),
                                  FALL = c("", "", "", ""), 
                                  WINTER =c("", "", "", ""),
                                  SPRING = c("", "", "", ""), 
                                  SUMMER = c("", "", "", ""))
std_summary_seasons["FALL"] = as.matrix(t(t(std_fall)))
std_summary_seasons["WINTER"] = as.matrix(t(t(std_winter)))
std_summary_seasons["SPRING"] = as.matrix(t(t(std_spring)))
std_summary_seasons["SUMMER"] = as.matrix(t(t(std_summer)))

# mean

mean_summary_seasons <- data.frame(REGION = c("NORTH", "EAST", 
                                             "NCENT", "SOMME"),
                                  FALL = c("", "", "", ""), 
                                  WINTER =c("", "", "", ""),
                                  SPRING = c("", "", "", ""), 
                                  SUMMER = c("", "", "", ""))
mean_summary_seasons["FALL"] = as.matrix(t(t(mean_fall)))
mean_summary_seasons["WINTER"] = as.matrix(t(t(mean_winter)))
mean_summary_seasons["SPRING"] = as.matrix(t(t(mean_spring)))
mean_summary_seasons["SUMMER"] = as.matrix(t(t(mean_summer)))


## By How much has the daily demand increased from 2012 to 2021?
## do the aggregation only with year

agg_eda_df1 = eda_df[c("NORTH", "EAST", "NCENT", "SOMME", 
                      "YEAR")]

year_agg = aggregate(.~YEAR,
                   agg_eda_df1,
                   FUN=mean, na.rm = FALSE)
print("hello again")

## ploting electricity in stacked years per month/season

north_vector <- c()
north_cen_vector <- c()
east_vector <- c()
somme_vector <- c()

year_list = list("2012", "2013", "2014", "2015", "2016", "2017", 
              "2018", "2019", "2020", "2021")
month_list = list("01", "02", "03", "04", "05", "06", "07", "08", 
              "09", "10", "11", "12")
season_list = list("FALL", "WINTER", "SPRING", "SUMMER")

## entire period 2012-21

results_2012_21 = stack_years(agg_eda_df, year_list, month_list, 
                        "YEAR", "MONTH", "NORTH", "NCENT", "EAST",
                        "SOMME",
                        north_vector,
                        north_cen_vector,
                        east_vector,
                        somme_vector)

north_vector_2012_21 = results_2012_21[1][[1]]
north_cen_vector_2012_21 = results_2012_21[2][[1]]
east_vector_2012_21 = results_2012_21[3][[1]]
somme_vector_2012_21 = results_2012_21[4][[1]]

ts_somme = ts(somme_vector_2012_21, start=2012, frequency=12)
ts_north = ts(north_vector_2012_21, start=2012, frequency=12)
ts_north_cen = ts(north_cen_vector_2012_21, start=2012, frequency=12)
ts_east = ts(east_vector_2012_21, start=2012, frequency=12)

## seasonal plot of stacked years, month after month

seasonplot(ts_somme, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand Somme in MW/h")

seasonplot(ts_north, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand North in MW/h")

seasonplot(ts_north_cen, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand North Cen in MW/h")

seasonplot(ts_east, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand East in MW/h")

## stl decomposition to find trend, season, and remainder useful
## to check for outliers too

plot(stl(ts_somme, "periodic"), main="stl decomposition Somme
                                      2012-21")
plot(stl(ts_north, "periodic"), main="stl decomposition
                                     North 2012-21")
plot(stl(ts_north_cen, "periodic"), main="stl decomposition 
                                     North Central 2012-21")
plot(stl(ts_east, "periodic"), main="stl decomposition 
                                     East 2012-21")

## check for acf and pacf to check for stationarity or not
## there is no stationarity of course

tsdisplay(ts_somme, main="All regions Somme 2012-21")
tsdisplay(ts_north, main="All regions North 2012-21")
tsdisplay(ts_north_cen, main="All regions North Central 2012-21")
tsdisplay(ts_east, main="All regions East 2012-21")

# season

north_vector <- c()
north_cen_vector <- c()
east_vector <- c()
somme_vector <- c()


results = stack_seasons(agg_eda_season_df, year_list, 
                        season_list, "YEAR", 
              "SEASON", "NORTH", "NCENT", "EAST",
                        "SOMME", north_vector, north_cen_vector, 
                        east_vector,somme_vector)

north_vector_2012_21 = results[1][[1]]
north_cen_vector_2012_21 = results[2][[1]]
east_vector_2012_21 = results[3][[1]]
somme_vector_2012_21 = results[4][[1]]

ts_somme = ts(somme_vector_2012_21, start=2012, frequency=4)
ts_north = ts(north_vector_2012_21, start=2012, frequency=4)
ts_north_cen = ts(north_cen_vector_2012_21, start=2012, frequency=4)
ts_east = ts(east_vector_2012_21, start=2012, frequency=4)

## season plot of each season frequency 4 of time series above

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


## holidays?
## week end?
## combined holidays and week end?


## outliers?


dev.off(dev.cur())
print("visual output in pdf created")

## CONCLUSION EDA_TARGET ##

## Data is not stationary because we can see a trend and several
## seasonality
## 2 climatic seasons in winter and summer
## an overall increasing trend in all regions and the whole
## interesting insight about yearly variability that has is stable
## at the year level may help with planning














#il y a bien une saisonnalité annuelle de la demande jounralière 
#d'électricité dans nos 3 régions du texas qui reste relativement 
#constante. Les plus hauts pics ont l'air de se dérouler en éte
#les plus bas pics ont quant à aux l'air d'être en hiver quand il y 
#en a (e.g. : hiver 2016 très doux ? Voir avec données méteo)




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











