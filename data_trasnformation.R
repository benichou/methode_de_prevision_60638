#
# Program: data_exploration.R
#
# Purpose: Exploring the ercot data
#
# Written by: Team G

# install the packages
Sys.setlocale("LC_TIME", "C")
library(forecast)
library(timeDate)
library(timeSeries)
library(astsa)

# load the R ercot data

filePath = 'data/ercotdata.rdata'
load(filePath)
head(ercotdata)
names(ercotdata)
# "COAST" "EAST"  "FWEST" "NORTH" "NCENT" "SOUTH" "SCENT" "WEST"  
# "ERCOT"

class(ercotdata)
# ts
start(ercotdata)
end(ercotdata)
frequency(ercotdata)
summary(ercotdata)

df = data.frame(Y=as.matrix(ercotdata), date=time(ercotdata))
# get the name of the columns in the dataframe
column_names_list = c(colnames(df))
# check the first 5 rows
head(df)

# Y.ERCOT and the time series are at position 9 and 10
our_columns = column_names_list[9:10]
# limit the sppData dataframe to WFEC (team WFEC) and the time series
ercotdata_df = df[c(our_columns[1], our_columns[2])]
head(ercotdata_df)

# check whether there are missing values
if(any(is.na(ercotdata_df))){
  print("There is missing data in the DataFrame")
}

identifyMissingRows <- function(df) {
  missingRows = df[is.na(df$Y.ERCOT),]
  return(missingRows)
}

identifyMissingRows(ercotdata_df)

typeof(ercotdata_df$GMT.x..i..)

# convert it to a datetime type with CST time zone
ercotdata_df$GMT.x..i.. = as.POSIXct(ercotdata_df$GMT.x..i..)

ercotdata_df$CSTTime = format(ercotdata_df$GMT.x..i.., 
                         tz="US/Central",usetz=TRUE)


# Create Year, Month, Day, Hour Variable
ercotdata_df$Year = as.integer(strftime(ercotdata_df$CSTTime, "%Y"))
ercotdata_df$Month = as.integer(strftime(ercotdata_df$CSTTime, "%m"))
ercotdata_df$Day = as.integer(strftime(ercotdata_df$CSTTime, "%d"))
ercotdata_df$Hour = as.integer(strftime(ercotdata_df$CSTTime, "%H"))

# ensure there are no missing values in the hourly data
## otherwise make sure to explore them and have an imputation 
## strategy


# transform the CSTTime as a date 
# before doing the grouping by max for daily peak 
ercotdata_df$Date = as.Date(ercotdata_df$CSTTime)

## aggregate the data by day and then get:
### somme de la demande horaire par jour
sum_demand_day_df = aggregate(Y.ERCOT ~ ercotdata_df$Date, 
                                     ercotdata_df, 
                                     sum) 

# export as a csv
yourPath = 'data/sum_demand_csv.csv'
write.csv(sum_demand_day_df, yourPath, row.names = FALSE)








