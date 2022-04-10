# Program: functions.R
#
# Purpose: script that contain all utility functions used 
# throughout the script
# Written by: Team G, January 30 2022
#
# Updated: Feb 10th 2022
#          
#         
#          
#         
#          
# ------------------------------------------------------

#Satterplot de deux series chrono en fonction d'une start date
# et d'une end date
scatter_period <- function(tsx , tsy, start, end ,
                             xlab , ylab , main) {
  
  #tsx : time series en x doit etre un objet timeSeries
  #tsy : time series en y doit etre aussi un objet timeSeries
  
  #les deux series chronos doivent etre des timeSeries object pour
  #que ca fonctionne. Utilise med_t et total dem
  ts1_wind = window(tsx , start = start , end = end)
  ts2_wind = window(tsy , start =start , end = end)
  
  df <- data.frame(ts1_wind$TS.1 , ts2_wind$TS.1)
  
  plot(x = df$ts1_wind , y = df$ts2_wind , main = main,
       xlab = xlab ,
       ylab = ylab ,
       pch = 19,
       frame = FALSE)
}

#Boxplot d'une serie chrono par groupe pour une periode donnee
boxplot_group_time <- function(ts , grp , start , end , ylab,
                               xlab , main , data ,
                               add_filter = NULL) {
  
  #ts : le nome de la colonne qui contient la timeSeries
  #grp : le nom de la variable qualitative pour regrouper
  # les donnees
  
  if (is.null(add_filter)) {
    filter <- which(data$DATE >= start & data$DATE <= end)
  } else {
    filter <- which(data$DATE >= start & data$DATE <= end &
                      add_filter)
  }
  
  
  if (grp == 'weekday' || grp == 'month') {
    group <- factor(data[filter , grp] , 
                    levels = unique(data[filter , grp]))
  } else {
    group <- data[filter , grp]
  }
  
  boxplot( data[filter ,ts] ~ group,
           xlab = xlab ,
           ylab = ylab ,
           main = main ,
           ylim = c(min(data[ts]) , max(data[ts])))
}

#function to plot a single time series over a given period of time
plot_period <- function(time_series , start , end , ylab ,
                        main) {
  sub_series <- window(time_series , start = start , end = end)
  plot(sub_series , ylab = ylab , main = main)
}

#year over year exploration
plot_year <- function(series , years_seq , ylabel , main) {
  
  for ( i in seq(1,length(years_seq)-1)) {
    plot_period(series , years_seq[i] , years_seq[i+1] , 
    ylab=ylabel ,
                main = main)
  }
}

stack_years <- function(df, year_list, month_list, year_col, 
                        month_col, north_col, n_cent_col, east_col,
                        somme_col, north_vector, north_cen_vector, 
                        east_vector,somme_vector) {
  for (i in year_list) {
      df1 = df[df[year_col] == i, ] 
      for (j in month_list) {
            north_value = df1[df1[month_col] == j, north_col]
            north_cen_value = df1[df1[month_col] == j, n_cent_col]
            east_value = df1[df1[month_col] == j, east_col]
            somme_value = df1[df1[month_col] == j, somme_col]
            north_vector = c(north_vector, north_value)
            north_cen_vector = c(north_cen_vector, north_cen_value)
            east_vector = c(east_vector, east_value)
            somme_vector = c(somme_vector, somme_value)  
      }
      }
      return(list(north_vector, north_cen_vector, east_vector, 
             somme_vector))
                        }

stack_seasons <- function(df, year_list, season_list, year_col, 
                        season_col, north_col, n_cent_col, east_col,
                        somme_col, north_vector, north_cen_vector, 
                        east_vector,somme_vector) {
  for (i in year_list) {
      df1 = df[df[year_col] == i, ] 
      for (j in season_list) {
            north_value = df1[df1[season_col] == j, north_col]
            north_cen_value = df1[df1[season_col] == j, n_cent_col]
            east_value = df1[df1[season_col] == j, east_col]
            somme_value = df1[df1[season_col] == j, somme_col]
            north_vector = c(north_vector, north_value)
            north_cen_vector = c(north_cen_vector, north_cen_value)
            east_vector = c(east_vector, east_value)
            somme_vector = c(somme_vector, somme_value)  
      }
      }
      return(list(north_vector, north_cen_vector, east_vector, 
             somme_vector))
                        }

stack_weeks <- function(df, year_list, week_list, year_col, 
                        week_col, north_col, n_cent_col, east_col,
                        somme_col, north_vector, north_cen_vector, 
                        east_vector,somme_vector) {
  for (i in year_list) {
      df1 = df[df[year_col] == i, ] 
      for (j in week_list) {
            north_value = df1[df1[week_col] == j, north_col]
            north_cen_value = df1[df1[week_col] == j, n_cent_col]
            east_value = df1[df1[week_col] == j, east_col]
            somme_value = df1[df1[week_col] == j, somme_col]
            north_vector = c(north_vector, north_value)
            north_cen_vector = c(north_cen_vector, north_cen_value)
            east_vector = c(east_vector, east_value)
            somme_vector = c(somme_vector, somme_value)  
      }
      }
      return(list(north_vector, north_cen_vector, east_vector, 
             somme_vector))
}

#function to create validation and training sets
wind_t <- function(ts , set) {
  
  ts <- timeSeries(ts , final_data$DATE)
  
  if (set == 't') {
    return(window(ts , start = '2012-01-01' , end ='2017-12-31'))
  } else {
    return(window(ts , start = '2018-01-01' , end = '2019-12-31'))
  }
}

