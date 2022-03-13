# methode_de_prevision_60638 TEXT VERSION IN CASE MARKDOWN DOES NOT 
# WORK
# Purpose
- The purpose is to forecast the sum of the hourly demand for the day 
t + 1
- This is a short term forecast with h = 1, with available data for 
day t, and this for each day of the year. 

# Data
The dataset is time series of hourly electricity demand from Ercot
for the period of January 1st 2012 to December 31st 2021.

Meteorological data from the National Centers for Environmental
Information is used and merged with the Ercot data.

Texas mesonet data that include relative humidity is also used

The output is a dataframe called data

# Running Instructions
1. Make sure your terminal is at the root of the project, that is
at methode_de_prevision_60638
2. Enter a R interactive console
3. Launch the following command
> source("main.R")
