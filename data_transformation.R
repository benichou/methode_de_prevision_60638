#
# Program: data_transformation.R
#
# Purpose: appropriate data transformation and data aggregattion
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

## load ercotdata

load("./data/ercotdata.RData")

colnames(ercotdata)

#Nouveau df avec nos regions
date = time(ercotdata)
date = as.POSIXct(date, tzone = "CST") #Changement de fuseau horaire
date = format(date, tz="US/Central",usetz=TRUE)
data = data.frame(date, ercotdata$NORTH, ercotdata$EAST, 
                  ercotdata$NCENT)
data = data[-c(1:5),] # Retirer les premieres obs qui étaient en GMT

data = head(data, - 18) # Retirer les dernieres obs 
#qui ne complete pas la journee

row.names(data) <- NULL #retirer les index

#Renommer les colonnes
names(data)[names(data) == "date"] <- "DATE"
names(data)[names(data) == "ercotdata.NORTH"] <- "NORTH"
names(data)[names(data) == "ercotdata.EAST"] <- "EAST"
names(data)[names(data) == "ercotdata.NCENT"] <- "NCENT"
colnames(data)

#Valeurs manquantes
print(sum(is.na(data))) # 3 valeurs manquantes
NonNAindex = which(is.na(data), arr.ind=TRUE) #Ligne 42523
print(data[42523,]) # Ligne 42523 = 6 novembre 2016 18h

#Remplacer valeurs manquantes avec la moy de la ligne avant et après
valeurs_remplacement = c((data[42522,2]+ data[42524,2])/2,
                         (data[42522,3]+ data[42524,3])/2,
                         (data[42522,4]+ data[42524,4])/2)

data[is.na(data)] <- valeurs_remplacement

print(data[42523,]) 
print(sum(is.na(data))) # 0 valeurs manquantes maintenant

#Bouger toutes les observations par 1 ligne vers le haut pour 
#faciliter l'aggregation

data[,-1] <- data[seq_len(nrow(data)) + 1, -1]
data = head(data, - 1) #enlever la derniere ligne qui est vide

#Aggrégation des données
data$DATE = as.Date(data$DATE)
data = aggregate(cbind(data$NORTH, data$EAST, data$NCENT) ~ 
                      data$DATE, FUN=sum, na.rm = FALSE)

data$SOMME <- data$V1 + data$V2 + data$V3

names(data)[names(data) == "data$DATE"] <- "DATE"
names(data)[names(data) == "V1"] <- "NORTH"
names(data)[names(data) == "V2"] <- "EAST"
names(data)[names(data) == "V3"] <- "NCENT"

print(head(data))
print("The Ercot data is now ready appropriately transformed
and aggregated to forecast the target at h = t+1")

# read time series data from NOAA

noaa_meteo <- read.csv("./meteo_data/time_series_meteo_data.csv")

# read texas mesonet data to extract relative humidity metrix (in %)

read.texas_mesonet <- function(year, 
                      path="./meteo_data/TxMeso_Timeseries_KDFW_") {
      
      
            texas_mesonet <- read.csv(paste(path, year, ".csv", 
            sep = ""))

            return(texas_mesonet)
                          }

mesonet_2012 <- read.texas_mesonet(2012)
mesonet_2013 <- read.texas_mesonet(2013)
mesonet_2014 <- read.texas_mesonet(2014)
mesonet_2015 <- read.texas_mesonet(2015)
mesonet_2016 <- read.texas_mesonet(2016)
mesonet_2017 <- read.texas_mesonet(2017)
mesonet_2018 <- read.texas_mesonet(2018)
mesonet_2019 <- read.texas_mesonet(2019)
mesonet_2020 <- read.texas_mesonet(2020)
mesonet_2021 <- read.texas_mesonet(2021)

mesonet_2012_13 <- rbind(mesonet_2012, mesonet_2013)
mesonet_2014_15 <- rbind(mesonet_2014, mesonet_2015)
mesonet_2016_17 <- rbind(mesonet_2016, mesonet_2017)
mesonet_2018_19 <- rbind(mesonet_2018, mesonet_2019)
mesonet_2020_21 <- rbind(mesonet_2020, mesonet_2021)

mesonet_2012_15 <- rbind(mesonet_2012_13, mesonet_2014_15)
mesonet_2016_19 <- rbind(mesonet_2016_17, mesonet_2018_19)
mesonet_2012_19 <- rbind(mesonet_2012_15, mesonet_2016_19)
mesonet_hum_all <- rbind(mesonet_2012_19, mesonet_2020_21)

# get the average relative humidity for each day 
# note the relative humidity data is tracked hourly so we will
# average it on a daily basis

hum = aggregate(cbind(mesonet_hum_all$Relative.Humidity) ~ 
                      mesonet_hum_all$Date_Time, FUN=mean, 
                      na.rm = FALSE)

# rename all date columns as "DATE" to enable proper merging on
# DATE
colnames(data)[1] <- "DATE"
colnames(noaa_meteo)[3] <- "DATE"
colnames(hum)[1] <- "DATE"
colnames(hum)[2] <- "RELATIVE_HUM_PERCENT"
# convert to characters to allow for merging
# same datatype is required
data$DATE <- as.character(data$DATE)
hum$DATE <- as.character(hum$DATE)
# merge data with 
data <- merge(data,noaa_meteo,by="DATE") # meteo data
data <- merge(data,hum,by="DATE") # with humidity data
# check the first rows of the dataframe
head(data)
# check the dimensions are still the same
dim(data)
print("The data is appropriately transformed, aggregated
      and appended with the meterological data from
      noaa and the humidity data from Texas Mesonet")