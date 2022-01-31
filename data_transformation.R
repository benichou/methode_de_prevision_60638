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
#qui ne complete pas la jour

row.names(data) <- NULL #retirer les index

#Renommer les colonnes
names(data)[names(data) == "GMT.x..i.."] <- "Date"
names(data)[names(data) == "ercotdata.NORTH"] <- "NORTH"
names(data)[names(data) == "ercotdata.EAST"] <- "EAST"
names(data)[names(data) == "ercotdata.NCENT"] <- "NCENT"
colnames(data)

#Valeurs manquantes
print(sum(is.na(data))) # 3 valeurs manquantes
NonNAindex = which(is.na(data), arr.ind=TRUE) #Ligne 42522
print(data[42522,]) # Ligne 42522 = 6 novembre 2016 18h

#Remplacer valeurs manquantes avec la moy de la ligne avant et après
valeurs_remplacement = c((data[42521,2]+ data[42523,2])/2,
                         (data[42521,3]+ data[42523,3])/2,
                         (data[42521,4]+ data[42523,4])/2)

data[is.na(data)] <- valeurs_remplacement

print(data[42522,]) 
print(sum(is.na(data))) # 0 valeurs manquantes maintenant

#Bouger toutes les observations par 1 ligne vers le haut pour 
#faciliter l'aggregation

data[,-1] <- data[seq_len(nrow(data)) + 1, -1]
data = head(data, - 1) #enlever la derniere ligne qui est vide

#Aggrégation des données
data$date = as.Date(data$date)
data = aggregate(cbind(data$NORTH, data$EAST, data$NCENT) ~ 
                      data$date, FUN=sum, na.rm = FALSE)

data$Somme <- data$V1 + data$V2 + data$V3

names(data)[names(data) == "data$date"] <- "Date"
names(data)[names(data) == "V1"] <- "NORTH"
names(data)[names(data) == "V2"] <- "EAST"
names(data)[names(data) == "V3"] <- "NCENT"

print(head(data))
print("The Ercot data is now ready appropriately transformed
and aggregated to forecast the target at h = t+1")


