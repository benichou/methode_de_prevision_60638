#??tape 1, corriger les probl??mes de la phase 1.


# Importer le master df b??ti dans la premi??re phase
load('master_df.Rdata')


#Arranger l'effet de refroidissement pour ??liminer la portion
#non lin??aire
temp_range <- seq(10 ,25 , 0.5)

for (temp in temp_range) {
  cp_test <- c()
  for (i in seq(1 , length(data$DATE))) {
    if (data$med_t[i] < temp){
      cp_test[i] = round(((data$AWND[i]**(1/2)) * (temp - data$med_t[i])),
                    digits = 4)
    } else {
      cp_test[i] = 0
    }
  }
  plot( x = cp_test , y = data$SOMME , main = temp , 
        xlab = 'Refroidissement' , 
        ylab = 'Demande' ,
        frame = FALSE)
}

#En observantle nuage de points, on peut remarquer que la meilleur 
#linearite est obtenue avec une temperature de 16.
#L'effet de refroidissement sera alors modifie en consequence

cp_final <- c()
for (i in seq(1 , length(data$DATE))) {
  if (data$med_t[i] < 16.0) {
    cp_final[i] = round(((data$AWND[i]**(1/2)) * (16.00 - data$med_t[i])),
                        digits = 4)
  } else {
    cp_final[i] = 0
  }
}

data['CP'] <- cp_final 

#Humidit?? relative : Durant la premi??re phase, nous avons utilis?? 
#l'humidit?? en fonction des diff??rent mois et nous avons obs??rv?? une 
#am??lioration du nuage de point. Nous allons essayer de modifi??


dewpoint <- data$med_t-((100-data$RELATIVE_HUM_PERCENT)/5)+273.15

humidex <- data$med_t+
  0.5555*(6.11*exp(5417.7530 *((1/273.16)-(1/dewpoint)))- 10)


#En observant le nuage de points, on remarque une forte 
#ressemblance avec celui de la demande vs temp??rature.
plot( x = humidex , y = data$SOMME , main = temp , 
      xlab = 'humidex' , 
      ylab = 'Demande' ,
      frame = FALSE)

#La relation est trop lin??aire ce qui signifie que l'humidit?? seule
#n'a pas d'apport avec la demande
plot( x = humidex , y = data$med_t , main = temp , 
      xlab = 'humidex' , 
      ylab = 'Demande' ,
      frame = FALSE)


#En utilisant une m??thode similaire ?? celle du refroidissement
temp_range <- seq(10 ,30 , 0.5)

for (temp in temp_range) {
  hum_test <- c()
  for (i in seq(1 , length(data$DATE))) {
    if (data$med_t[i] > temp){
      hum_test[i] = round(( (data$RELATIVE_HUM_PERCENT[i]**(1/2)) * 
                             (data$med_t[i] - temp)),
                         digits = 4)
    } else {
      hum_test[i] = 0
    }
  }
  plot( x = hum_test , y = data$SOMME , main = temp , 
        xlab = 'Humidity' , 
        ylab = 'Demande' ,
        frame = FALSE)
}

#Le nuage de point indique que la meilleure relation est avec 22

hum_final <- c()
for (i in seq(1 , length(data$DATE))) {
  if (data$med_t[i] > 22){
    hum_final[i] = round(( (data$RELATIVE_HUM_PERCENT[i]**(1/2)) * 
                           (data$med_t[i] - 22) ),
                        digits = 4)
  } else {
    hum_final[i] = 0
  }
}

data['hum_final'] <- hum_final


#marking the days arround holidays
before_holiday <- rep(0 , length(data$DATE))
after_holiday <- rep(0 , length(data$DATE))

for (i in seq(2 , length(data$DATE))) {
  if(data$Holiday[i] == 1){
    before_holiday[i-1] = 1
    after_holiday[i+1] = 1
  } 
}

#noticed that I forgot to adjust 
after_holiday[2] = 1

data['before_holi'] <- before_holiday
data['after_holiday'] <- after_holiday

#Enregistrer le df avec les variables utiles seulement
colonnes_utiles <- c("DATE" , "SOMME" , "CDD" , "HDD" , "weekday" ,
                     "CP" , "Holiday" , "hum_final" , "before_holi",
                     "after_holiday")
# subset
final_data <- data[colonnes_utiles]

save(final_data , file='regression_df.Rdata')