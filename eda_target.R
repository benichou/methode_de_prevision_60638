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
agg_eda_df["SEASON"] = "0"
agg_eda_df[agg_eda_df["MONTH"] == "12", "SEASON"] = "WINTER"
agg_eda_df[agg_eda_df["MONTH"] == "01", "SEASON"] = "WINTER"                         
agg_eda_df[agg_eda_df["MONTH"] == "02", "SEASON"] = "WINTER" 
agg_eda_df[agg_eda_df["MONTH"] == "03", "SEASON"] = "SPRING"
agg_eda_df[agg_eda_df["MONTH"] == "04", "SEASON"] = "SPRING"
agg_eda_df[agg_eda_df["MONTH"] == "05", "SEASON"] = "SPRING"
agg_eda_df[agg_eda_df["MONTH"] == "06", "SEASON"] = "SUMMER"
agg_eda_df[agg_eda_df["MONTH"] == "07", "SEASON"] = "SUMMER"
agg_eda_df[agg_eda_df["MONTH"] == "08", "SEASON"] = "SUMMER"
agg_eda_df[agg_eda_df["MONTH"] == "09", "SEASON"] = "FALL"
agg_eda_df[agg_eda_df["MONTH"] == "10", "SEASON"] = "FALL"
agg_eda_df[agg_eda_df["MONTH"] == "11", "SEASON"] = "FALL"

agg_eda_season_df = agg_eda_df[,-2]

agg_eda_season_df = aggregate(.~YEAR+SEASON,
                   agg_eda_season_df,
                   FUN=sum, na.rm = FALSE)

## calculate descriptive statistics for each region
## : avg, std, median, mode, Q1, Q3, IQR

## Fall season :
fall_df = agg_eda_season_df[
        agg_eda_season_df["SEASON"] == "FALL", -c(1, 2)]
fall_df_summary = summary(fall_df)
std_fall = colSds(as.matrix(fall_df[
       sapply(fall_df, is.numeric)]))

# winter season:
winter_df = agg_eda_season_df[
        agg_eda_season_df["SEASON"] == "WINTER", -c(1, 2)]
winter_df_summary = summary(winter_df)
std_winter = colSds(as.matrix(winter_df[
       sapply(winter_df, is.numeric)]))
# spring season:
spring_df = agg_eda_season_df[
        agg_eda_season_df["SEASON"] == "SPRING", -c(1, 2)]
spring_df_summary = summary(spring_df)
std_spring = colSds(as.matrix(spring_df[
       sapply(spring_df, is.numeric)]))
# summer season:
summer_df = agg_eda_season_df[
        agg_eda_season_df["SEASON"] == "SUMMER", -c(1, 2)]
summer_df_summary = summary(summer_df)
std_summer = colSds(as.matrix(summer_df[
       sapply(summer_df, is.numeric)]))

## what is the standard deviation of the daily electric demand 
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

## what is the standard deviation of the daily electric demand 
## for each region in each year?

demand_2012 = agg_eda_df[agg_eda_df["YEAR"] == "2012", ]
std_2012 = colSds(as.matrix(demand_2012[
       sapply(demand_2012, is.numeric)]))
demand_2013 = agg_eda_df[agg_eda_df["YEAR"] == "2013", ]
std_2013 = colSds(as.matrix(demand_2013[
       sapply(demand_2013, is.numeric)]))
demand_2014 = agg_eda_df[agg_eda_df["YEAR"] == "2014", ]
std_2014 = colSds(as.matrix(demand_2014[
       sapply(demand_2014, is.numeric)]))
demand_2015 = agg_eda_df[agg_eda_df["YEAR"] == "2015", ]
std_2015 = colSds(as.matrix(demand_2015[
       sapply(demand_2015, is.numeric)]))
demand_2016 = agg_eda_df[agg_eda_df["YEAR"] == "2016", ]
std_2016 = colSds(as.matrix(demand_2016[
       sapply(demand_2016, is.numeric)]))
demand_2017 = agg_eda_df[agg_eda_df["YEAR"] == "2017", ]
std_2017 = colSds(as.matrix(demand_2017[
       sapply(demand_2017, is.numeric)]))
demand_2017 = agg_eda_df[agg_eda_df["YEAR"] == "2017", ]
std_2017 = colSds(as.matrix(demand_2017[
       sapply(demand_2017, is.numeric)]))
demand_2018 = agg_eda_df[agg_eda_df["YEAR"] == "2018", ]
std_2018 = colSds(as.matrix(demand_2018[
       sapply(demand_2018, is.numeric)]))
demand_2019 = agg_eda_df[agg_eda_df["YEAR"] == "2019", ]
std_2019 = colSds(as.matrix(demand_2019[
       sapply(demand_2019, is.numeric)]))
demand_2020 = agg_eda_df[agg_eda_df["YEAR"] == "2020", ]
std_2020 = colSds(as.matrix(demand_2020[
       sapply(demand_2020, is.numeric)]))
demand_2021 = agg_eda_df[agg_eda_df["YEAR"] == "2021", ]
std_2021 = colSds(as.matrix(demand_2021[
       sapply(demand_2021, is.numeric)]))

std_summary_year <- data.frame(YEAR = c("2012", "2013", 
                                           "2014", "2015", 
                                           "2016", "2017", 
                                           "2018", "2019", 
                                           "2020", "2021"),
                                  NORTH = c("", "", "", "", 
                                            "", "", "", "",
                                            "", ""), 
                                  EAST =c("", "", "", "", 
                                            "", "", "", "",
                                            "", ""),
                                  NCENT = c("", "", "", "", 
                                            "", "", "", "",
                                            "", ""), 
                                  SOMME = c("", "", "", "", 
                                            "", "", "", "",
                                            "", ""))
std_summary_year[std_summary_year["YEAR"] == "2012", 
                                             -c(1)] = std_2012
std_summary_year[std_summary_year["YEAR"] == "2013", 
                                             -c(1)] = std_2013
std_summary_year[std_summary_year["YEAR"] == "2014", 
                                             -c(1)] = std_2014
std_summary_year[std_summary_year["YEAR"] == "2015", 
                                             -c(1)] = std_2015
std_summary_year[std_summary_year["YEAR"] == "2016", 
                                             -c(1)] = std_2016
std_summary_year[std_summary_year["YEAR"] == "2017", 
                                             -c(1)] = std_2017
std_summary_year[std_summary_year["YEAR"] == "2018", 
                                             -c(1)] = std_2018
std_summary_year[std_summary_year["YEAR"] == "2019", 
                                             -c(1)] = std_2019
std_summary_year[std_summary_year["YEAR"] == "2020", 
                                             -c(1)] = std_2020
std_summary_year[std_summary_year["YEAR"] == "2021", 
                                             -c(1)] = std_2021

## By How much has the demand increased from 2012 to 2021?
## do the aggregation only with year



## ploting electricity in stacked years per month/season

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
season_list = list("FALL", "WINTER", "SPRING", "SUMMER")


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

plot(stl(ts_somme, "periodic"), main="stl decomposition 2012-21")
plot(stl(ts_north, "periodic"), main="stl decomposition 2012-21")
plot(stl(ts_north_cen, "periodic"), main="stl decomposition 2012-21")
plot(stl(ts_east, "periodic"), main="stl decomposition 2012-21")

tsdisplay(ts_somme, main="All regions 2012-21")
tsdisplay(ts_north, main="All regions 2012-21")
tsdisplay(ts_north_cen, main="All regions 2012-21")
tsdisplay(ts_east, main="All regions 2012-21")

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
           col=rainbow(5), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north, 
           col=rainbow(5), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north_cen, 
           col=rainbow(5), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_east, 
           col=rainbow(5), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")


plot(stl(ts_somme, "periodic"), main="stl decomposition 2012-17")
plot(stl(ts_north, "periodic"), main="stl decomposition 2012-17")
plot(stl(ts_north_cen, "periodic"), main="stl decomposition 2012-17")
plot(stl(ts_east, "periodic"), main="stl decomposition 2012-17")

tsdisplay(ts_somme, main="All regions 2012-17")
tsdisplay(ts_north, main="All regions 2012-17")
tsdisplay(ts_north_cen, main="All regions 2012-17")
tsdisplay(ts_east, main="All regions 2012-17")

# season

north_vector <- c()
north_cen_vector <- c()
east_vector <- c()
somme_vector <- c()


results = stack_seasons(agg_eda_season_df, year_list_2012_17, 
                        season_list, "YEAR", 
              "SEASON", "NORTH", "NCENT", "EAST",
                        "SOMME", north_vector, north_cen_vector, 
                        east_vector,somme_vector)

north_vector = results[1][[1]]
north_cen_vector = results[2][[1]]
east_vector = results[3][[1]]
somme_vector = results[4][[1]]

ts_somme = ts(somme_vector, start=2012, frequency=4)
ts_north = ts(north_vector, start=2012, frequency=4)
ts_north_cen = ts(north_cen_vector, start=2012, frequency=4)
ts_east = ts(east_vector, start=2012, frequency=4)


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

# 2012 -2015

north_vector <- c()
north_cen_vector <- c()
east_vector <- c()
somme_vector <- c()

year_list_2012_15 = list("2012", "2013", "2014", "2015")

results_2012_15 = stack_years(agg_eda_df, year_list_2012_15, 
                              month_list, 
                        "YEAR", "MONTH", "NORTH", "NCENT", "EAST",
                        "SOMME",
                        north_vector,
                        north_cen_vector,
                        east_vector,
                        somme_vector)

north_vector_2012_15 = results_2012_15[1][[1]]
north_cen_vector_2012_15 = results_2012_15[2][[1]]
east_vector_2012_15 = results_2012_15[3][[1]]
somme_vector_2012_15 = results_2012_15[4][[1]]

ts_somme = ts(somme_vector_2012_15, start=2012, frequency=12)
ts_north = ts(north_vector_2012_15, start=2012, frequency=12)
ts_north_cen = ts(north_cen_vector_2012_15, start=2012, frequency=12)
ts_east = ts(east_vector_2012_15, start=2012, frequency=12)

seasonplot(ts_somme, 
           col=rainbow(4), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north, 
           col=rainbow(4), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north_cen, 
           col=rainbow(4), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_east, 
           col=rainbow(4), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

plot(stl(ts_somme, "periodic"), main="stl decomposition 2012-15")
plot(stl(ts_north, "periodic"), main="stl decomposition 2012-15")
plot(stl(ts_north_cen, "periodic"), main="stl decomposition 2012-15")
plot(stl(ts_east, "periodic"), main="stl decomposition 2012-15")

tsdisplay(ts_somme, main="All regions 2012-15")
tsdisplay(ts_north, main="All regions 2012-15")
tsdisplay(ts_north_cen, main="All regions 2012-15")
tsdisplay(ts_east, main="All regions 2012-15")


# season

north_vector <- c()
north_cen_vector <- c()
east_vector <- c()
somme_vector <- c()

results = stack_seasons(agg_eda_season_df, year_list_2012_15, 
                        season_list, "YEAR", 
              "SEASON", "NORTH", "NCENT", "EAST",
                        "SOMME", north_vector, north_cen_vector, 
                        east_vector,somme_vector)

north_vector = results[1][[1]]
north_cen_vector = results[2][[1]]
east_vector = results[3][[1]]
somme_vector = results[4][[1]]

ts_somme = ts(somme_vector, start=2012, frequency=4)
ts_north = ts(north_vector, start=2012, frequency=4)
ts_north_cen = ts(north_cen_vector, start=2012, frequency=4)
ts_east = ts(east_vector, start=2012, frequency=4)

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

## 2016 - 2021

north_vector <- c()
north_cen_vector <- c()
east_vector <- c()
somme_vector <- c()

year_list_2016_21 = list("2016", "2017", "2018", "2019", "2020", 
                         "2021")

results_2016_21 = stack_years(agg_eda_df, year_list_2016_21, 
                              month_list, 
                        "YEAR", "MONTH", "NORTH", "NCENT", "EAST",
                        "SOMME",
                        north_vector,
                        north_cen_vector,
                        east_vector,
                        somme_vector)

north_vector_2016_21 = results_2016_21[1][[1]]
north_cen_vector_2016_21 = results_2016_21[2][[1]]
east_vector_2016_21 = results_2016_21[3][[1]]
somme_vector_2016_21 = results_2016_21[4][[1]]

ts_somme = ts(somme_vector_2016_21, start=2016, frequency=12)
ts_north = ts(north_vector_2016_21, start=2016, frequency=12)
ts_north_cen = ts(north_cen_vector_2016_21, start=2016, frequency=12)
ts_east = ts(east_vector_2016_21, start=2016, frequency=12)

seasonplot(ts_somme, 
           col=rainbow(6), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north, 
           col=rainbow(6), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north_cen, 
           col=rainbow(6), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_east, 
           col=rainbow(6), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

plot(stl(ts_somme, "periodic"), main="stl decomposition 2016-21")
plot(stl(ts_north, "periodic"), main="stl decomposition 2016-21")
plot(stl(ts_north_cen, "periodic"), main="stl decomposition 2016-21")
plot(stl(ts_east, "periodic"), main="stl decomposition 2016-21")

tsdisplay(ts_somme, main="All regions 2016-21")
tsdisplay(ts_north, main="All regions 2016-21")
tsdisplay(ts_north_cen, main="All regions 2016-21")
tsdisplay(ts_east, main="All regions 2016-21")

# season

north_vector <- c()
north_cen_vector <- c()
east_vector <- c()
somme_vector <- c()

results = stack_seasons(agg_eda_season_df, year_list_2016_21, 
                        season_list, "YEAR", 
              "SEASON", "NORTH", "NCENT", "EAST",
                        "SOMME", north_vector, north_cen_vector, 
                        east_vector,somme_vector)

north_vector = results[1][[1]]
north_cen_vector = results[2][[1]]
east_vector = results[3][[1]]
somme_vector = results[4][[1]]

ts_somme = ts(somme_vector, start=2016, frequency=4)
ts_north = ts(north_vector, start=2016, frequency=4)
ts_north_cen = ts(north_cen_vector, start=2016, frequency=4)
ts_east = ts(east_vector, start=2016, frequency=4)

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

## 2012 - 2013 14

north_vector <- c()
north_cen_vector <- c()
east_vector <- c()
somme_vector <- c()

year_list_2012_14 = list("2012", "2013", "2014")

results_2012_14 = stack_years(agg_eda_df, year_list_2012_14, 
                              month_list, 
                        "YEAR", "MONTH", "NORTH", "NCENT", "EAST",
                        "SOMME",
                        north_vector,
                        north_cen_vector,
                        east_vector,
                        somme_vector)

north_vector_2012_14 = results_2012_14[1][[1]]
north_cen_vector_2012_14 = results_2012_14[2][[1]]
east_vector_2012_14 = results_2012_14[3][[1]]
somme_vector_2012_14 = results_2012_14[4][[1]]

ts_somme = ts(somme_vector_2012_14, start=2012, frequency=12)
ts_north = ts(north_vector_2012_14, start=2012, frequency=12)
ts_north_cen = ts(north_cen_vector_2012_14, start=2012, frequency=12)
ts_east = ts(east_vector_2012_14, start=2012, frequency=12)

seasonplot(ts_somme, 
           col=rainbow(3), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north, 
           col=rainbow(3), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north_cen, 
           col=rainbow(3), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_east, 
           col=rainbow(3), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

plot(stl(ts_somme, "periodic"), main="stl decomposition 2012-14")
plot(stl(ts_north, "periodic"), main="stl decomposition 2012-14")
plot(stl(ts_north_cen, "periodic"), main="stl decomposition 2012-14")
plot(stl(ts_east, "periodic"), main="stl decomposition 2012-14")

tsdisplay(ts_somme, main="All regions 2012-14")
tsdisplay(ts_north, main="All regions 2012-14")
tsdisplay(ts_north_cen, main="All regions 2012-14")
tsdisplay(ts_east, main="All regions 2012-14")

# season

north_vector <- c()
north_cen_vector <- c()
east_vector <- c()
somme_vector <- c()

results = stack_seasons(agg_eda_season_df, year_list_2012_14, 
                        season_list, "YEAR", 
              "SEASON", "NORTH", "NCENT", "EAST",
                        "SOMME", north_vector, north_cen_vector, 
                        east_vector,somme_vector)

north_vector = results[1][[1]]
north_cen_vector = results[2][[1]]
east_vector = results[3][[1]]
somme_vector = results[4][[1]]

ts_somme = ts(somme_vector, start=2012, frequency=4)
ts_north = ts(north_vector, start=2012, frequency=4)
ts_north_cen = ts(north_cen_vector, start=2012, frequency=4)
ts_east = ts(east_vector, start=2012, frequency=4)

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

## 2019/20/21

north_vector <- c()
north_cen_vector <- c()
east_vector <- c()
somme_vector <- c()

year_list_2019_21 = list("2019", "2020", "2021")

results_2019_21 = stack_years(agg_eda_df, year_list_2019_21, 
                              month_list, 
                        "YEAR", "MONTH", "NORTH", "NCENT", "EAST",
                        "SOMME",
                        north_vector,
                        north_cen_vector,
                        east_vector,
                        somme_vector)

north_vector_2019_21 = results_2019_21[1][[1]]
north_cen_vector_2019_21 = results_2019_21[2][[1]]
east_vector_2019_21 = results_2019_21[3][[1]]
somme_vector_2019_21 = results_2019_21[4][[1]]

ts_somme = ts(somme_vector_2019_21, start=2019, frequency=12)
ts_north = ts(north_vector_2019_21, start=2019, frequency=12)
ts_north_cen = ts(north_cen_vector_2019_21, start=2019, frequency=12)
ts_east = ts(east_vector_2019_21, start=2019, frequency=12)

seasonplot(ts_somme, 
           col=rainbow(3), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north, 
           col=rainbow(3), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north_cen, 
           col=rainbow(3), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_east, 
           col=rainbow(3), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

plot(stl(ts_somme, "periodic"), main="stl decomposition 2019-21")
plot(stl(ts_north, "periodic"), main="stl decomposition 2019-21")
plot(stl(ts_north_cen, "periodic"), main="stl decomposition 2019-21")
plot(stl(ts_east, "periodic"), main="stl decomposition 2019-21")

tsdisplay(ts_somme, main="All regions 2019-21")
tsdisplay(ts_north, main="All regions 2019-21")
tsdisplay(ts_north_cen, main="All regions 2019-21")
tsdisplay(ts_east, main="All regions 2019-21")

# season 

north_vector <- c()
north_cen_vector <- c()
east_vector <- c()
somme_vector <- c()

results = stack_seasons(agg_eda_season_df, year_list_2016_21, 
                        season_list, "YEAR", 
              "SEASON", "NORTH", "NCENT", "EAST",
                        "SOMME", north_vector, north_cen_vector, 
                        east_vector,somme_vector)

north_vector = results[1][[1]]
north_cen_vector = results[2][[1]]
east_vector = results[3][[1]]
somme_vector = results[4][[1]]

ts_somme = ts(somme_vector, start=2016, frequency=4)
ts_north = ts(north_vector, start=2016, frequency=4)
ts_north_cen = ts(north_cen_vector, start=2016, frequency=4)
ts_east = ts(east_vector, start=2016, frequency=4)

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


## 2014/15/16/17/18

north_vector <- c()
north_cen_vector <- c()
east_vector <- c()
somme_vector <- c()

year_list_2014_18 = list("2014", "2015", "2016", "2017", "2018")

results_2014_18 = stack_years(agg_eda_df, year_list_2014_18, 
                              month_list, 
                        "YEAR", "MONTH", "NORTH", "NCENT", "EAST",
                        "SOMME",
                        north_vector,
                        north_cen_vector,
                        east_vector,
                        somme_vector)

north_vector_2014_18 = results_2014_18[1][[1]]
north_cen_vector_2014_18 = results_2014_18[2][[1]]
east_vector_2014_18 = results_2014_18[3][[1]]
somme_vector_2014_18 = results_2014_18[4][[1]]

ts_somme = ts(somme_vector_2014_18, start=2014, frequency=12)
ts_north = ts(north_vector_2014_18, start=2014, frequency=12)
ts_north_cen = ts(north_cen_vector_2014_18, start=2014, frequency=12)
ts_east = ts(east_vector_2014_18, start=2014, frequency=12)

seasonplot(ts_somme, 
           col=rainbow(5), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north, 
           col=rainbow(5), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_north_cen, 
           col=rainbow(5), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

seasonplot(ts_east, 
           col=rainbow(5), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Energy Demand in MW/h")

plot(stl(ts_somme, "periodic"), main="stl decomposition 2014-18")
plot(stl(ts_north, "periodic"), main="stl decomposition 2014-18")
plot(stl(ts_north_cen, "periodic"), main="stl decomposition 2014-18")
plot(stl(ts_east, "periodic"), main="stl decomposition 2014-18")

tsdisplay(ts_somme, main="All regions 2014-18")
tsdisplay(ts_north, main="All regions 2014-18")
tsdisplay(ts_north_cen, main="All regions 2014-18")
tsdisplay(ts_east, main="All regions 2014-18")


dev.off(dev.cur())


## CONCLUSION EDA_TARGET ##

## Data is not stationary because we can see a trend and several
## seasonality
## 2 climatic seasons in winter and summer
## an overall increasing trend in all regions and the whole
## interesting insight about yearly variability that has is stable
## at the year level may help with planning
##
##  holidays?
## week end?
## combined holidays and week end?
## outliers?












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











