# Program: eda_target.R
#
# Purpose: exploration of the target to check for trends, seasonality
#  and special effects of week ends, stats holidays etc on 
# daily Electricity demand
#
# Written by: Team G, January 30 2022
#
# Updated: Team G, Feb 6th 2022
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


eda_df_october_2017 = eda_df[((eda_df["MONTH"] == 10) | 
                            (eda_df["MONTH"] ==11)) & 
                             eda_df["YEAR"] == 2017,]
week_vector <- c()
for (j in as.vector(eda_df_october_2017["DATE"])) {
        week_of_day = strftime(j, format = "%V")
        
          week_vector = c(week_vector, week_of_day)  
      }
eda_df_october_2017["WEEK"] = week_vector

week_list = c(40, 41, 42, 43, 43, 44, 45, 46, 47) 
year_list = c("2017")
north_vector <- c()
north_cen_vector <- c()
east_vector <- c()
somme_vector <- c()

results_october_2017 <- stack_weeks(eda_df_october_2017, 
                        year_list, week_list, "YEAR", 
                        "WEEK", "NORTH", "NCENT", "EAST",
                        "SOMME", north_vector, north_cen_vector, 
                        east_vector,somme_vector)

north_vector_2017 = results_october_2017[1][[1]]
north_cen_vector_2017 = results_october_2017[2][[1]]
east_vector_2017 = results_october_2017[3][[1]]
somme_vector_2017 = results_october_2017[4][[1]]

ts_somme = ts(somme_vector_2017, frequency=7)
ts_north = ts(north_vector_2017, frequency=7)
ts_north_cen = ts(north_cen_vector_2017, frequency=7)
ts_east = ts(east_vector_2017, frequency=7)

seasonplot(ts_somme, 
           col=rainbow(9), year.labels=TRUE,year.labels.left=TRUE,
continuous=TRUE, ylab= "Electricity Demand Somme in MW/h Weekly")

seasonplot(ts_north, 
           col=rainbow(9), year.labels=TRUE,year.labels.left=TRUE,
continuous=TRUE, ylab= "Electricity Demand North in MW/h Weekly")

seasonplot(ts_north_cen, 
           col=rainbow(9), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, 
           ylab= "Electricity Demand North Cen in MW/h Weekly")

seasonplot(ts_east, 
        col=rainbow(19), year.labels=TRUE,year.labels.left=TRUE,
    continuous=TRUE, ylab= "Electricity Demand East in MW/h Weekly")




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

mean_summary_seasons <- data.frame(REGION = c("FALL", "WINTER", 
                                             "SPRING", "SUMMER"),
                                  NORTH = c("", "", "", ""), 
                                  EAST =c("", "", "", ""),
                                  NCENT = c("", "", "", ""), 
                                  SOMME = c("", "", "", ""))
mean_summary_seasons["NORTH"] = mean_fall
mean_summary_seasons["EAST"] = mean_winter
mean_summary_seasons["NCENT"] = mean_spring
mean_summary_seasons["SOMME"] = mean_summer

fall <- c(north=mean_fall[1], east=mean_fall[2], 
         ncent=mean_fall[3], somme=mean_fall[4])
winter <- c(north=mean_winter[1], east=mean_winter[2], 
         ncent=mean_winter[3], somme=mean_winter[4])
spring <-c(north=mean_spring[1], east=mean_spring[2], 
         ncent=mean_spring[3], somme=mean_spring[4])
summer <- c(north=mean_summer[1], east=mean_summer[2], 
         ncent=mean_summer[3], somme=mean_summer[4])
summary_seasons = rbind(fall,winter,spring, summer)

colnames(summary_seasons) = c("NORTH", "EAST", "NCENT", "SOMME")
options(scipen=10000)
barplot(summary_seasons,
        col=c("dodgerblue3", "skyblue1", "green", "red"),
        legend.text = rownames(summary_seasons),
        args.legend=list(cex=0.95,x = "topleft"), 
        horiz=FALSE, beside=TRUE, 
        main= "Average Daily Electricity Demand MW/H per Season")

## By How much has the daily demand increased from 2012 to 2021?
## do the aggregation only with year

agg_eda_df1 = eda_df[c("NORTH", "EAST", "NCENT", "SOMME", 
                      "YEAR")]

year_agg = aggregate(.~YEAR,
                   agg_eda_df1,
                   FUN=mean, na.rm = FALSE)
## TODO:
options(scipen=10000)
plot(year_agg$YEAR, year_agg$NORTH, col = "red", type = "l", 
     lty = 1, ylim = c(0, 400000), xlab="YEAR", 
     ylab = "Average Daily Electricity Demand MW/H", 
     main= "Average Daily Electricity Demand MW/H")
lines(year_agg$YEAR, year_agg$EAST, col = "blue",
      lty=2, lwd=1.8, type="l")
lines(year_agg$YEAR, year_agg$NCENT, col = "green",
      lty=3, lwd=1.8, type="l")
lines(year_agg$YEAR, year_agg$SOMME, col = "black",
      lty=4, lwd=1.8, type="l")
legend("topleft", legend=c("NORTH","EAST", 
                           "NORTH CENTRAL", "SOMME"),
       col=c("red", "blue", "green", "black"),
       cex=0.5,
       lty=c(1,2,3,4))

p_north = year_agg[1:9, c("NORTH")]
p_east = year_agg[1:9, c("EAST")]
p_ncent = year_agg[1:9, c("NCENT")]
p_somme = year_agg[1:9, c("SOMME")]

year_agg_delta = year_agg
year_agg_delta[year_agg_delta$YEAR == 2012,c("NORTH", "EAST", 
                                            "NCENT", "SOMME")] = 0

year_agg_delta[year_agg_delta$YEAR > 2012,
 c("NORTH")] = 100 *(year_agg_delta[year_agg_delta$YEAR > 2012,
 c("NORTH")] - p_north) / p_north

year_agg_delta[year_agg_delta$YEAR > 2012,
 c("EAST")] = 100 *(year_agg_delta[year_agg_delta$YEAR > 2012,
 c("EAST")] - p_east) / p_east

year_agg_delta[year_agg_delta$YEAR > 2012,
 c("NCENT")] = 100 *(year_agg_delta[year_agg_delta$YEAR > 2012,
 c("NCENT")] - p_ncent) / p_ncent


year_agg_delta[year_agg_delta$YEAR > 2012,
 c("SOMME")] = 100 *(year_agg_delta[year_agg_delta$YEAR > 2012,
 c("SOMME")] - p_somme) / p_somme

options(scipen=10000)
plot(year_agg_delta$YEAR, year_agg_delta$NORTH, 
     col = "red", type = "l", 
     lty = 1, ylim = c(-10, 20), xlab="YEAR", 
     ylab = "% Evolution of Average Daily Demand MW/H",
     main= "% Evolution of Electricity Demand MW/H")
lines(year_agg_delta$YEAR, year_agg_delta$EAST, col = "blue",
      lty=2, lwd=1.8, type="l")
lines(year_agg_delta$YEAR, year_agg_delta$NCENT, col = "green",
      lty=3, lwd=1.8, type="l")
lines(year_agg_delta$YEAR, year_agg_delta$SOMME, col = "black",
      lty=4, lwd=1.8, type="l")
legend("topleft", legend=c("NORTH","EAST", 
                           "NORTH CENTRAL", "SOMME"),
       col=c("red", "blue", "green", "black"),
       cex=1.0,
       lty=c(1,2,3,4))



daily_avg_dmd_2012 = year_agg[year_agg$YEAR == 2012, c("NORTH", 
                                                        "EAST", 
                                                         "NCENT", 
                                                         "SOMME")] 
daily_avg_dmd_2021 = year_agg[year_agg$YEAR == 2021, c("NORTH", 
                                                        "EAST", 
                                                        "NCENT", 
                                                        "SOMME")]


delta_2012_21 = 100 * (daily_avg_dmd_2021 - 
daily_avg_dmd_2012) /daily_avg_dmd_2012

print("The % evolution between 2012 to 2021 of the daily demand is")
print(delta_2012_21)

## ploting Electricity in stacked years per month/season

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
options(scipen=10000)
seasonplot(ts_somme, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Electricity Demand Somme in MW/h")
options(scipen=10000)
seasonplot(ts_north, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Electricity Demand North in MW/h")
options(scipen=10000)
seasonplot(ts_north_cen, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, 
           ylab= "Electricity Demand North Cen in MW/h")
options(scipen=10000)
seasonplot(ts_east, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Electricity Demand East in MW/h")

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
options(scipen=10000)
seasonplot(ts_somme, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Electricity Demand in MW/h")
options(scipen=10000)
seasonplot(ts_north, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Electricity Demand in MW/h")
options(scipen=10000)
seasonplot(ts_north_cen, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Electricity Demand in MW/h")
options(scipen=10000)
seasonplot(ts_east, 
           col=rainbow(12), year.labels=TRUE,year.labels.left=TRUE,
           continuous=TRUE, ylab= "Electricity Demand in MW/h")

## Outlier analysis plot the daily data

## somme --> one outlier on 2021
daily_2012_somme = eda_df[eda_df$YEAR == "2012",c("SOMME")]
daily_2013_somme = eda_df[eda_df$YEAR == "2013",c("SOMME")]
daily_2014_somme = eda_df[eda_df$YEAR == "2014",c("SOMME")]
daily_2015_somme = eda_df[eda_df$YEAR == "2015",c("SOMME")]
daily_2016_somme = eda_df[eda_df$YEAR == "2016",c("SOMME")]
daily_2017_somme = eda_df[eda_df$YEAR == "2017",c("SOMME")]
daily_2018_somme = eda_df[eda_df$YEAR == "2018",c("SOMME")]
daily_2019_somme = eda_df[eda_df$YEAR == "2019",c("SOMME")]
daily_2020_somme = eda_df[eda_df$YEAR == "2020",c("SOMME")]
daily_2021_somme = eda_df[eda_df$YEAR == "2021",c("SOMME")]
# no outliers in 2012
boxplot(daily_2012_somme, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis Somme 2012")
# no outliers in 2013
boxplot(daily_2013_somme, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis Somme 2013")
# no outliers 2014
boxplot(daily_2014_somme, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis Somme 2014")
# no outliers in 2015
boxplot(daily_2015_somme, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis Somme 2015")
# no outliers in 2016
boxplot(daily_2016_somme, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis Somme 2016")
# no outliers in 2017
boxplot(daily_2017_somme, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis Somme 2017")
# no outliers in 2018
boxplot(daily_2018_somme, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis Somme 2018")
# no outliers in 2019
boxplot(daily_2019_somme, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis Somme 2019")
# no outliers in 2020
boxplot(daily_2020_somme, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis Somme 2020")
# One outlier in 2021
boxplot(daily_2021_somme, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis Somme 2021")

## north --> one outlier in 2017 and one in 2021
daily_2012_north = eda_df[eda_df$YEAR == "2012",c("NORTH")]
daily_2013_north = eda_df[eda_df$YEAR == "2013",c("NORTH")]
daily_2014_north = eda_df[eda_df$YEAR == "2014",c("NORTH")]
daily_2015_north = eda_df[eda_df$YEAR == "2015",c("NORTH")]
daily_2016_north = eda_df[eda_df$YEAR == "2016",c("NORTH")]
daily_2017_north = eda_df[eda_df$YEAR == "2017",c("NORTH")]
daily_2018_north = eda_df[eda_df$YEAR == "2018",c("NORTH")]
daily_2019_north = eda_df[eda_df$YEAR == "2019",c("NORTH")]
daily_2020_north = eda_df[eda_df$YEAR == "2020",c("NORTH")]
daily_2021_north = eda_df[eda_df$YEAR == "2021",c("NORTH")]
# no outliers in 2012
boxplot(daily_2012_north, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North 2012")
# no outliers in 2013
boxplot(daily_2013_north, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North 2013")
# no outliers in 2014
boxplot(daily_2014_north, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North 2014")
# no outliers in 2015
boxplot(daily_2015_north, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North 2015")
# no outliers in 2016
boxplot(daily_2016_north, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North 2016")
# One outlier in 2017
boxplot(daily_2017_north, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North 2017")
# no outliers in 2018
boxplot(daily_2018_north, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North 2018")
# no outliers in 2019
boxplot(daily_2019_north, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North 2019")
# no outliers in 2020
boxplot(daily_2020_north, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North 2020")
# one outlier in 2021
boxplot(daily_2021_north, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North 2021")


## north central --> one outlier in 2021
daily_2012_north_c = eda_df[eda_df$YEAR == "2012",c("NCENT")]
daily_2013_north_c = eda_df[eda_df$YEAR == "2013",c("NCENT")]
daily_2014_north_c = eda_df[eda_df$YEAR == "2014",c("NCENT")]
daily_2015_north_c = eda_df[eda_df$YEAR == "2015",c("NCENT")]
daily_2016_north_c = eda_df[eda_df$YEAR == "2016",c("NCENT")]
daily_2017_north_c = eda_df[eda_df$YEAR == "2017",c("NCENT")]
daily_2018_north_c = eda_df[eda_df$YEAR == "2018",c("NCENT")]
daily_2019_north_c = eda_df[eda_df$YEAR == "2019",c("NCENT")]
daily_2020_north_c = eda_df[eda_df$YEAR == "2020",c("NCENT")]
daily_2021_north_c = eda_df[eda_df$YEAR == "2021",c("NCENT")]
# no outliers in 2012
boxplot(daily_2012_north_c, 
ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North Central 2012")
# no outliers in 2013
boxplot(daily_2013_north_c, 
ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North Central 2013")
# no outliers in 2014
boxplot(daily_2014_north_c, 
ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North Central 2014")
# no outliers in 2015
boxplot(daily_2015_north_c, 
ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North Central 2015")
# no outliers in 2016
boxplot(daily_2016_north_c, 
ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North Central 2016")
# No outlier in 2017
boxplot(daily_2017_north_c, 
ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North Central 2017")
# no outliers in 2018
boxplot(daily_2018_north_c, 
ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North Central 2018")
# no outliers in 2019
boxplot(daily_2019_north_c, 
ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North Central 2019")
# no outliers in 2020
boxplot(daily_2020_north_c, 
ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North Central 2020")
# one outlier in 2021
boxplot(daily_2021_north_c, 
ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis North Central 2021")

## east --> 2 outliers in 2021
daily_2012_east = eda_df[eda_df$YEAR == "2012",c("EAST")]
daily_2013_east = eda_df[eda_df$YEAR == "2013",c("EAST")]
daily_2014_east = eda_df[eda_df$YEAR == "2014",c("EAST")]
daily_2015_east = eda_df[eda_df$YEAR == "2015",c("EAST")]
daily_2016_east = eda_df[eda_df$YEAR == "2016",c("EAST")]
daily_2017_east = eda_df[eda_df$YEAR == "2017",c("EAST")]
daily_2018_east = eda_df[eda_df$YEAR == "2018",c("EAST")]
daily_2019_east = eda_df[eda_df$YEAR == "2019",c("EAST")]
daily_2020_east = eda_df[eda_df$YEAR == "2020",c("EAST")]
daily_2021_east = eda_df[eda_df$YEAR == "2021",c("EAST")]
# no outliers in 2012
boxplot(daily_2012_east, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis East 2012")
# no outliers in 2013
boxplot(daily_2013_east, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis East 2013")
# no outliers in 2014
boxplot(daily_2014_east, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis East 2014")
# no outliers in 2015
boxplot(daily_2015_east, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis East 2015")
# no outliers in 2016
boxplot(daily_2016_east, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis East 2016")
# No outlier in 2017
boxplot(daily_2017_east, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis East 2017")
# no outliers in 2018
boxplot(daily_2018_east, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis East 2018")
# no outliers in 2019
boxplot(daily_2019_east, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis East 2019")
# no outliers in 2020
boxplot(daily_2020_east, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis East 2020")
# 2 outliera in 2021
boxplot(daily_2021_east, ylab = "Daily Electricity Demand in MW/h", 
main="Outlier Analysis East 2021")

## week day effect


## holidays?
### somme
boxplot(data$SOMME~data$Holiday, data=data, notch=TRUE,
col=(c("gold","darkgreen")),
main="Effet Jour Ferie SOMME", xlab="Holidays (1) or No H (0)",
ylab="Daily Electricity Demand in MW/h")
### north cen
boxplot(data$NCENT~data$Holiday, data=data, notch=TRUE,
col=(c("gold","darkgreen")),
main="Effet Jour Ferie NORTH CENTRAL", 
xlab="Holidays (1) or No H (0)",
ylab="Daily Electricity Demand in MW/h")
### north
boxplot(data$NORTH~data$Holiday, data=data, notch=TRUE,
col=(c("gold","darkgreen")),
main="Effet Jour Ferie NORTH", xlab="Holidays (1) or No H (0)",
ylab="Daily Electricity Demand in MW/h")
### east
boxplot(data$EAST~data$Holiday, data=data, notch=TRUE,
col=(c("gold","darkgreen")),
main="Effet Jours Ferie EAST", xlab="Holidays (1) or No H (0)",
ylab="Daily Electricity Demand in MW/h")

## week end vs week day?
boxplot(data$SOMME~data$weekend, data=data, notch=TRUE,
col=(c("gold","darkgreen")),
main="Effet Fin de Semaine Somme",
 xlab="Weekend (1) or Week Day(0)",
ylab="Daily Electricity Demand in MW/h")
### north cen
boxplot(data$NCENT~data$weekend, data=data, notch=TRUE,
col=(c("gold","darkgreen")),
main="Effet Fin de Semaine NORTH CENTRAL", 
xlab="Weekend (1) or Week Day(0)",
ylab="Daily Electricity Demand in MW/h")
### north
boxplot(data$NORTH~data$weekend, data=data, notch=TRUE,
col=(c("gold","darkgreen")),
main="Effet Fin de Semaine NORTH", 
xlab="Weekend (1) or Week Day(0)",
ylab="Daily Electricity Demand in MW/h")
### east
boxplot(data$EAST~data$weekend, data=data, notch=TRUE,
col=(c("gold","darkgreen")),
main="Effet Fin de Semaine EAST", 
xlab="Weekend (1) or Week Day(0)",
ylab="Daily Electricity Demand in MW/h")

## combined holidays and week end?
boxplot(data$SOMME~data$inter_holi_weekend, data=data, notch=TRUE,
col=(c("gold","darkgreen")),
main="Jour Ferie Week end Somme", 
xlab="Interaction Holiday Week end (1) or not (0)",
ylab="Daily Electricity Demand in MW/h")
### north cen
boxplot(data$NCENT~data$inter_holi_weekend, data=data, notch=TRUE,
col=(c("gold","darkgreen")),
main="Jour Ferie Week end NORTH CENTRAL", 
xlab="Interaction Holiday Week end or not",
ylab="Daily Electricity Demand in MW/h")
### north
boxplot(data$NORTH~data$inter_holi_weekend, data=data, notch=TRUE,
col=(c("gold","darkgreen")),
main="Jour Ferie Week end NORTH", 
xlab="Interaction Holiday Week end or not",
ylab="Daily Electricity Demand in MW/h")
### east
boxplot(data$EAST~data$inter_holi_weekend, data=data, notch=TRUE,
col=(c("gold","darkgreen")),
main="Jour Ferie Week end EAST", 
xlab="Interaction Holiday Week end or not)",
ylab="Daily Electricity Demand in MW/h")


dev.off(dev.cur())
print("visual output in pdf created")


