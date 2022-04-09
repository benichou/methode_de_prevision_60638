source('timeseries_x.R')

#Ce script a pour but de corriger les erreurs en lien avec le
#mod??le de regression de la phase 2

#La performance ??tait tr??s mauvais ??tant donn??e qu'on n'avait pas
#Inclut la variable temps on va essayer avec et voir si ca fait 
#une diff??rence


#The arima models seem over estimate the demand and with
#quit an important marging
#our startegy to correct this is to make a model that retrains
#at each iteration for the whole validation period
#it will only take a portion of the training


