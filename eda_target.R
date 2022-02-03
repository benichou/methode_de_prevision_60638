#
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
source("./data_transformation.R")

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


# check for outlier values annual, monthly basis

#2012
plot(data_ts[1:366], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2012-01-01 to 2012-12-31")
#RAs si ce n'est mi-février

#2012-01
plot(data_ts[1:31], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2012-01-01 to 2012-01-31")

#2012-02
plot(data_ts[32:60], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2012-02-01 to 2012-02-29")
#assez haut aux alentours du 10-15 février

#2012-03
plot(data_ts[61:91], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2012-03-01 to 2012-03-31")

#2012-04
plot(data_ts[92:121], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2012-04-01 to 2012-04-30")

#2012-05
plot(data_ts[122:152], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2012-05-01 to 2012-05-31")

#2012-06
plot(data_ts[153:182], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2012-06-01 to 2012-06-30")

#2012-07
plot(data_ts[183:213], 
ylab="Texas Daily Demand (in MW)",
xlab="2012-07-01 to 2012-07-31")

#2012-08
plot(data_ts[214:244], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2012-08-01 to 2012-08-31")

#2012-09
plot(data_ts[245:274], 
ylab="Texas Daily Demand (in MW)",
xlab="2012-09-01 to 2012-09-30")

#2012-10
plot(data_ts[275:305], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2012-10-01 to 2012-10-31")

#2012-11
plot(data_ts[306:335], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2012-11-01 to 2012-11-30")

#2012-12
plot(data_ts[336:366], 
ylab="Texas Daily Demand (in MW)",
xlab="2012-12-01 to 2012-12-31")
#il y a l'air d'avoir une certaine saisonnalité mensuelle, 
# quelques trait sinusoïdaux?

#2013
plot(data_ts[367:731], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2013-01-01 to 2013-12-31")
#grosse montée à la fin voir si lié avec les chutes de neige observées
#plus tôt

#2013-01
plot(data_ts[367:397], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2013-01-01 to 2013-01-31")

#2013-02
plot(data_ts[398:425], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2013-02-01 to 2013-02-28")

#2013-03
plot(data_ts[426:456], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2013-03-01 to 2013-03-31")
#2013-04
plot(data_ts[457:486], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2013-04-01 to 2013-04-30")

#2013-05
plot(data_ts[487:517], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2013-05-01 to 2013-05-31")

#2013-06
plot(data_ts[518:546], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2013-06-01 to 2013-06-30")

#2013-07
plot(data_ts[547:578], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2013-07-01 to 2013-07-31")

#2013-08
plot(data_ts[579:608], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2013-08-01 to 2013-08-31")

#2013-09
plot(data_ts[609:639], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2013-09-01 to 2013-09-30")

#2013-10
plot(data_ts[640:670], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2013-10-01 to 2013-10-31")

#2013-11
plot(data_ts[671:700], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2013-11-01 to 2013-11-30")

#2013-12
plot(data_ts[701:731], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2013-12-01 to 2013-12-31")


#2014
plot(data_ts[732:1096], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2014-01-01 to 2014-12-31")

#2014-01
plot(data_ts[732:762], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2014-01-01 to 2014-01-31")

#2014-02
plot(data_ts[763:790], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2014-02-01 to 2014-02-28")

#2014-03
plot(data_ts[791:821], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2014-03-01 to 2014-03-31")

#2014-04
plot(data_ts[822:851], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2014-04-01 to 2014-04-30")

#2014-05
plot(data_ts[852:882], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2014-05-01 to 2014-05-31")

#2014-06
plot(data_ts[883:912], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2014-06-01 to 2014-06-30")

#2014-07
plot(data_ts[913:943], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2014-07-01 to 2014-07-31")

#2014-08
plot(data_ts[944:974], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2014-08-01 to 2014-08-31")

#2014-09
plot(data_ts[975:1004], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2014-09-01 to 2014-09-30")

#2014-10
plot(data_ts[1005:1035], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2014-10-01 to 2014-10-31")

#2014-11
plot(data_ts[1036:1065], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2014-11-01 to 2014-11-30")

#2014-12
plot(data_ts[1066:1096], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2014-12-01 to 2014-12-31")

#2015
plot(data_ts[1097:1461], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2015-01-01 to 2015-12-31")

#2015-01
plot(data_ts[1097:1127], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2015-01-01 to 2015-01-31")

#2015-02
plot(data_ts[1128:1155], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2015-02-01 to 2015-02-28")

#2015-03
plot(data_ts[1156:1186], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2015-03-01 to 2015-03-31")

#2015-04
plot(data_ts[1187:1216], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2015-04-01 to 2015-04-30")

#2015-05
plot(data_ts[1217:1247], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2015-05-01 to 2015-05-31")

#2015-06
plot(data_ts[1248:1277], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2015-06-01 to 2015-06-30")

#2015-07
plot(data_ts[1278:1308], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2015-07-01 to 2015-07-31")

#2015-08
plot(data_ts[1309:1339], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2015-08-01 to 2015-08-31")

#2015-09
plot(data_ts[1340:1369], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2015-09-01 to 2015-09-30")

#2015-10
plot(data_ts[1370:1400], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2015-10-01 to 2015-10-31")

#2015-11
plot(data_ts[1401:1430], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2015-11-01 to 2015-11-30")

#2015-12
plot(data_ts[1431:1461], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2015-12-01 to 2015-12-31")

#2016
plot(data_ts[1462:1827], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2016-01-01 to 2016-12-31")

#2016-01
plot(data_ts[1462:1492], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2016-01-01 to 2016-01-31")

#2016-02
plot(data_ts[1493:1521], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2016-02-01 to 2016-02-29")

#2016-03
plot(data_ts[1522:1552], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2016-03-01 to 2016-03-31")

#2016-04
plot(data_ts[1553:1582], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2016-04-01 to 2016-04-30")

#2016-05
plot(data_ts[1583:1613], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2016-05-01 to 2016-05-31")

#2016-06
plot(data_ts[1614:1643], 
    ylab="Texas Daily Demand (in MW)",
    xlab="2016-06-01 to 2016-06-30")

#2016-07
plot(data_ts[1644:1674], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2016-07-01 to 2016-07-31")

#2016-08
plot(data_ts[1675:1705], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2016-08-01 to 2016-08-31")

#2016-09
plot(data_ts[1706:1735], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2016-09-01 to 2016-09-30")

#2016-10
plot(data_ts[1736:1766], 
    ylab="Texas Daily Demand (in MW)",
    xlab="2016-10-01 to 2016-10-31")

#2016-11
plot(data_ts[1767:1796], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2016-11-01 to 2016-11-30")

#2016-12
plot(data_ts[1797:1827], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2016-12-01 to 2016-12-31")

#2017
plot(data_ts[1828:2192], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2017-01-01 to 2017-12-31")

#2017-01
plot(data_ts[1828:2192], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2017-01-01 to 2017-01-31")

#2017-02
plot(data_ts[1828:2192], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2017-02-01 to 2017-02-28")

#2017-03
plot(data_ts[1828:2192], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2017-03-01 to 2017-03-31")

#2017-04
plot(data_ts[1828:2192], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2017-04-01 to 2017-04-30")

#2017-05
plot(data_ts[1828:2192], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2017-05-01 to 2017-05-31")

#2017-06
plot(data_ts[1828:2192], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2017-06-01 to 2017-06-30")

#2017-07
plot(data_ts[1828:2192], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2017-07-01 to 2017-07-31")

#2017-08
plot(data_ts[1828:2192], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2017-08-01 to 2017-08-31")

#2017-09
plot(data_ts[1828:2192], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2017-09-01 to 2017-09-30")

#2017-10
plot(data_ts[1828:2192], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2017-10-01 to 2017-10-31")

#2017-11
plot(data_ts[1828:2192], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2017-11-01 to 2017-11-31")

#2017-12
plot(data_ts[1828:2192], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2017-12-01 to 2017-12-31")

#2018
plot(data_ts[2193:2557], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2018-01-01 to 2018-12-31")

#2018-01
plot(data_ts[2193:2557], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2018-01-01 to 2018-01-31")

#2018-02
plot(data_ts[2193:2557], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2018-02-01 to 2018-02-28")

#2018-03
plot(data_ts[2193:2557], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2018-03-01 to 2018-03-31")

#2018-04
plot(data_ts[2193:2557], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2018-04-01 to 2018-04-30")

#2018-05
plot(data_ts[2193:2557], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2018-05-01 to 2018-05-31")

#2018-06
plot(data_ts[2193:2557], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2018-06-01 to 2018-06-30")

#2018-07
plot(data_ts[2193:2557], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2018-07-01 to 2018-07-31")

#2018-08
plot(data_ts[2193:2557], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2018-08-01 to 2018-08-31")

#2018-09
plot(data_ts[2193:2557], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2018-09-01 to 2018-09-30")

#2018-10
plot(data_ts[2193:2557], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2018-10-01 to 2018-10-31")

#2018-11
plot(data_ts[2193:2557], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2018-11-01 to 2018-11-30")

#2018-12
plot(data_ts[2193:2557], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2018-12-01 to 2018-12-31")


#2019
plot(data_ts[2558:2922], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2019-01-01 to 2019-12-31")


#2019-01
plot(data_ts[2558:2922], 
    ylab="Texas Daily Demand (in MW)",
    xlab="2019-01-01 to 2019-01-31")

#2019-02
plot(data_ts[2558:2922], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2019-02-01 to 2019-02-28")

#2019-03
plot(data_ts[2558:2922], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2019-03-01 to 2019-03-31")

#2019-04
plot(data_ts[2558:2922], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2019-04-01 to 2019-04-30")

#2019-05
plot(data_ts[2558:2922], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2019-05-01 to 2019-05-31")

#2019-06
plot(data_ts[2558:2922], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2019-06-01 to 2019-06-30")

#2019-07
plot(data_ts[2558:2922], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2019-07-01 to 2019-07-31")

#2019-08
plot(data_ts[2558:2922], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2019-08-01 to 2019-08-31")

#2019-09
plot(data_ts[2558:2922], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2019-09-01 to 2019-09-30")

#2019-10
plot(data_ts[2558:2922], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2019-10-01 to 2019-10-31")

#2019-11
plot(data_ts[2558:2922], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2019-11-01 to 2019-11-30")

#2019-12
plot(data_ts[2558:2922], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2019-12-01 to 2019-12-31")

#2020
plot(data_ts[2923:3288], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2020-01-01 to 2020-12-31")

#2020-01
plot(data_ts[2923:3288], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2020-01-01 to 2020-01-31")

#2020-02
plot(data_ts[2923:3288], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2020-02-01 to 2020-02-29")

#2020-03
plot(data_ts[2923:3288], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2020-03-01 to 2020-03-31")

#2020-04
plot(data_ts[2923:3288], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2020-04-01 to 2020-04-30")

#2020-05
plot(data_ts[2923:3288], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2020-05-01 to 2020-05-31")

#2020-06
plot(data_ts[2923:3288], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2020-06-01 to 2020-06-30")

#2020-07
plot(data_ts[2923:3288], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2020-07-01 to 2020-07-31")

#2020-08
plot(data_ts[2923:3288], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2020-08-01 to 2020-08-31")

#2020-09
plot(data_ts[2923:3288], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2020-09-01 to 2020-09-30")

#2020-10
plot(data_ts[2923:3288], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2020-10-01 to 2020-10-31")

#2020-11
plot(data_ts[2923:3288], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2020-11-01 to 2020-11-30")

#2020-12
plot(data_ts[2923:3288], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2020-12-01 to 2020-12-31")

#2021
plot(data_ts[3289:3652], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2021-01-01 to 2021-12-30")

plot(snwd_ts[3289:3652], 
     ylab="Texas Daily Snow Depth (in Celsius)")

#2021-01
plot(data_ts[3289:3319], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2021-01-01 to 2021-01-31")

#2021-02
plot(data_ts[3320:3347], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2021-02-01 to 2021-02-28")

#2021-03
plot(data_ts[3348:3378], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2021-03-01 to 2021-03-31")

#2021-04
plot(data_ts[3379:3408], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2021-04-01 to 2021-04-30")

#2021-05
plot(data_ts[3409:3439], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2021-05-01 to 2021-05-31")

#2021-06
plot(data_ts[3440:3469], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2021-06-01 to 2021-06-30")

#2021-07
plot(data_ts[3470:3500], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2021-07-01 to 2021-07-31")

#2021-08
plot(data_ts[3501:3531], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2021-08-01 to 2021-08-31")

#2021-09
plot(data_ts[3532:3561], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2021-09-01 to 2021-09-30")

#2021-10
plot(data_ts[3562:3592], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2021-10-01 to 2021-10-31")

#2021-11
plot(data_ts[3593:3622], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2021-11-01 to 2021-11-30")

#2021-12
plot(data_ts[3622:3652], 
     ylab="Texas Daily Demand (in MW)",
     xlab="2021-12-01 to 2021-12-30")


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











