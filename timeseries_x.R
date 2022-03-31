#Loading the right packages
library(timeSeries)
library(forecast)
library(car)



#creating time series variables for last phase
#Importing our user defined functions
source('functions.R')



#Importing explanatory variables
load('regression_df.Rdata')
load('master_df.Rdata')



#Les variables doivent etre exprimees en time series on va toutes les
#transformer ici pour un usage future

#1 - demande
yt <- timeSeries(final_data$SOMME , final_data$DATE)
yt_train <- window(yt , start = '2012-01-01' , end = '2017-12-31')
yt_valid <- window(yt , start = '2018-01-01' , end = '2019-12-31')

# - CDD et HDD avec leur bruit
cddt <- timeSeries(final_data$noisy_CDD , final_data$DATE)
cddt_train <- window(cddt , start = '2012-01-01' , end = '2017-12-31')
cddt_valid <- window(cddt , start = '2018-01-01' , end = '2019-12-31')

hddt <- timeSeries(final_data$noisy_HDD , final_data$DATE)
hddt_train <- window(hddt , start = '2012-01-01' , end = '2017-12-31')
hddt_valid <- window(hddt , start = '2018-01-01' , end = '2019-12-31')

# Refroidissement avec son bruit
cpt <- timeSeries(final_data$noisy_cp, final_data$DATE)
cpt_train <- window(cpt , start = '2012-01-01' , end = '2017-12-31')
cpt_valid <- window(cpt , start = '2018-01-01' , end = '2019-12-31')

#humidex avec son bruit egalement
humt <- timeSeries(final_data$noisy_humidex, final_data$DATE)
humt_train <- window(humt , start = '2012-01-01' , end = '2017-12-31')
humt_valid <- window(humt , start = '2018-01-01' , end = '2019-12-31')

#befor holiday
befholyt <- timeSeries(final_data$before_holi, final_data$DATE)
befholy_train <- wind_t(final_data$before_holi , set= 't')
befholy_valid <- wind_t(final_data$before_holi , set= 'v')

#after holiday
aftholyt <- timeSeries(final_data$after_holiday, final_data$DATE)
aftholy_train <- wind_t(final_data$after_holiday , set= 't')
aftholy_valid <- wind_t(final_data$after_holiday , set= 'v')

#holiday 
holyt <- timeSeries(final_data$Holiday, final_data$DATE)
holy_train <- wind_t(final_data$Holiday , set= 't')
holy_valid <- wind_t(final_data$Holiday , set= 'v')

#Dummy pour la journee de la semaine vendredi est dans 
#la reference
DMon <- ifelse(factor(final_data$weekday)=="Monday", 1, 0)
DTue <- ifelse(factor(final_data$weekday)=="Tuesday", 1, 0)
DWed <- ifelse(factor(final_data$weekday)=="Wednesday", 1, 0)
DThu <- ifelse(factor(final_data$weekday)=="Thursday", 1, 0)
DSat <- ifelse(factor(final_data$weekday)=="Saturday", 1, 0)
DSun <- ifelse(factor(final_data$weekday)=="Sunday", 1, 0)

DMont <- timeSeries(DMon , final_data$DATE)
DMont_train<-window(DMont , start = '2012-01-01' , end = '2017-12-31')
DMont_valid<-window(DMont , start = '2018-01-01' , end = '2019-12-31')

DTuet <- timeSeries(DTue , final_data$DATE)
DTuet_train<-window(DTuet , start = '2012-01-01' , end = '2017-12-31')
DTuet_valid<-window(DTuet , start = '2018-01-01' , end = '2019-12-31')

DWedt <- timeSeries(DWed , final_data$DATE)
DWedt_train<-window(DWedt , start = '2012-01-01' , end = '2017-12-31')
DWedt_valid<-window(DWedt , start = '2018-01-01' , end = '2019-12-31')

DThut <- timeSeries(DThu , final_data$DATE)
DThut_train<-window(DThut , start = '2012-01-01' , end = '2017-12-31')
DThut_valid<-window(DThut , start = '2018-01-01' , end = '2019-12-31')

DSatt <- timeSeries(DSat , final_data$DATE)
DSatt_train<-window(DSatt , start = '2012-01-01' , end = '2017-12-31')
DSatt_valid<-window(DSatt , start = '2018-01-01' , end = '2019-12-31')

DSunt <- timeSeries(DSun , final_data$DATE)
DSunt_train<-window(DSunt , start = '2012-01-01' , end = '2017-12-31')
DSunt_valid<-window(DSunt , start = '2018-01-01' , end = '2019-12-31')

#Dummy pour le mois, decembre est en reference
Djan <- ifelse(factor(data$month)=="January", 1, 0)
Djant_train <- wind_t(Djan , set ='t')
Djant_valid <- wind_t(Djan , set ='v')

Dfeb <- ifelse(factor(data$month)=="February", 1, 0)
Dfebt_train <- wind_t(Dfeb , set = 't')
Dfebt_valid <- wind_t(Dfeb , set = 'v')

Dmar <- ifelse(factor(data$month)=="March", 1, 0)
Dmart_train <- wind_t(Dmar , set = 't')
Dmart_valid <- wind_t(Dmar , set = 'v')

Dapr <- ifelse(factor(data$month)=="April", 1, 0)
Daprt_train <- wind_t(Dapr , set = 't')
Daprt_valid <- wind_t(Dapr , set = 'v')

Dmay <- ifelse(factor(data$month)=="May", 1, 0)
Dmayt_train <- wind_t(Dmay , set = 't')
Dmayt_valid <- wind_t(Dmay , set = 'v')

Djun <- ifelse(factor(data$month)=="June", 1, 0)
Djunt_train <- wind_t(Djun , set = 't')
Djunt_valid <- wind_t(Djun , set = 'v')

Djul <- ifelse(factor(data$month)=="July", 1, 0)
Djult_train <- wind_t(Djul , set = 't')
Djult_valid <- wind_t(Djul , set = 'v')

Daug <- ifelse(factor(data$month)=="August", 1, 0)
Daugt_train <- wind_t(Daug , set = 't')
Daugt_valid <- wind_t(Daug , set = 'v')

Dsep <- ifelse(factor(data$month)=="September", 1, 0)
Dsept_train <- wind_t(Dsep , set = 't')
Dsept_valid <- wind_t(Dsep , set = 'v')

Doct <- ifelse(factor(data$month)=="October", 1, 0)
Doctt_train <- wind_t(Doct , set = 't')
Doctt_valid <- wind_t(Doct , set = 'v')

Dnov <- ifelse(factor(data$month)=="December", 1, 0)
Dnovt_train <- wind_t(Dnov , set = 't')
Dnovt_valid <- wind_t(Dnov , set = 'v')

q1 <- ifelse(factor(final_data$quarter)=="Q1", 1, 0)
q1t <- timeSeries(q1 , final_data$DATE)

q2 <- ifelse(factor(final_data$quarter)=="Q2", 1, 0)
q2t <- timeSeries(q2 , final_data$DATE)

q3 <- ifelse(factor(final_data$quarter)=="Q3", 1, 0)
q3t <- timeSeries(q3 , final_data$DATE)

q4 <- ifelse(factor(final_data$quarter)=="Q4", 1, 0)
q4t <- timeSeries(q4 , final_data$DATE)

quartt <- timeSeries(final_data$quarter , final_data$DATE)
quartt_train <- wind_t(quartt , set='t')
quartt_valid <- wind_t(quartt , set='v')

