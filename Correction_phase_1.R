#etape 1, corriger les problemes de la phase 1.


# Importer le master df b??ti dans la premi??re phase
load('master_df.Rdata')

#ajouter du bruit dans les donn??es de validation et test pour 
#les variables explicatives

#data separation
#beg_train <- 1 #2012/01/01
#end_train <- 2192 #2017/12/31
#end_valid <- 2922 #2019/12/31


pdf("visual_output/correction_phase1.pdf")

#https://www.nrel.gov/docs/fy12osti/56130.pdf
set.seed(123)
noise_wind <- c()
for (i in seq(1 , length(data$AWND))) {
  if (i < 2192) {
    noise_wind[i] = 0
  } else {
    noise_wind[i] = rnorm(1 , mean = 0 , sd = 0.1187)
  }
}

#https://www.forecastadvisor.com/detail/Texas/Dallas/75235/
set.seed(123)
noise_temp <- c()
for (i in seq(1 , length(data$AWND))) {
  if (i < 2192) {
    noise_temp[i] = 0
  } else {
    noise_temp[i] = rnorm(1 , mean = 0 , sd = 1.5)
  }
}

#didn't find any sources regarding that matter so we will
#assume good prediction on relative humidity
set.seed(123)
noise_hum <- c()
for (i in seq(1 , length(data$AWND))) {
  if (i < 2192) {
    noise_hum[i] = 0
  } else {
    noise_hum[i] = rnorm(1 , mean = 0 , sd = 2)
  }
}

# adding the noise to our variables
noisy_temp <- data$med_t + noise_temp
noisy_hum <- data$RELATIVE_HUM_PERCENT + noise_hum
noisy_wind <- data$AWND + noise_wind

# creating the covariates with the noisy vars

noisy_CDD <- c()
for (i in seq(1 , length(noisy_temp))) {
  noisy_CDD[i] <- max(noisy_temp[i] - 21 , 0 )
}

noisy_HDD <- c()
for (i in seq(1 , length(noisy_temp))) {
  noisy_HDD[i] <- max(16 - noisy_temp[i] , 0)
} 

data['noisy_CDD'] <- noisy_CDD

data['noisy_HDD'] <- noisy_HDD



#Arranger l'effet de refroidissement pour eliminer la portion
#non lineaire
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
  plot( x = cp_test , y = data$SOMME , 
        main = paste('Tref: ' ,temp) , 
        xlab = 'Effet de refroidissement' , 
        ylab = "Somme de la demande d'??lectricit??" ,
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

#Plot final cp against temps
plot(x = cp_final , y = data$SOMME ,
main = 
paste("La demande contre l'effet de refroissiement Tref: ",16), 
     xlab = 'Effet de refroidissement' , 
     ylab = "Somme de la demande d'??lectricit??" ,
     frame = FALSE)

data['CP'] <- cp_final 

noisy_cp <- c()
for (i in seq(1 , length(noisy_wind))) {
  if (noisy_temp[i] < 16.0) {
    noisy_cp[i]=round(((noisy_wind[i]**(1/2))*(16.00-noisy_temp[i])),
                        digits = 4)
  } else {
    noisy_cp[i] = 0
  }
}

data['noisy_cp'] <- noisy_cp


#Humidite relative : Durant la premiere phase, nous avons utilise
#l'humidite en fonction des different mois et nous avons observe une 
#amelioration du nuage de point. Nous allons essayer de modifier


dewpoint <- data$med_t-((100-data$RELATIVE_HUM_PERCENT)/5)+273.15

humidex <- data$med_t+
  0.5555*(6.11*exp(5417.7530 *((1/273.16)-(1/dewpoint)))- 10)


#En observant le nuage de points, on remarque une forte 
#ressemblance avec celui de la demande vs temperature.
plot( x = humidex , y = data$SOMME , main = temp , 
      xlab = 'humidex' , 
      ylab = 'Demande' ,
      frame = FALSE)

#La relation est trop lineaire ce qui signifie que l'humidite seule
#n'a pas d'apport avec la demande
plot( x = humidex , y = data$med_t , main = temp , 
      xlab = 'humidex' , 
      ylab = 'Demande' ,
      frame = FALSE)


#En utilisant une methode similaire a celle du refroidissement
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

humidex_final <- c()
for (i in seq(1 , length(data$DATE))) {
  if (data$med_t[i] > 22){
    humidex_final[i] = round(( (data$RELATIVE_HUM_PERCENT[i]**(1/2)) * 
                           (data$med_t[i] - 22) ),
                        digits = 4)
  } else {
    humidex_final[i] = 0
  }
}

#Plot final Humidit??
plot(x = humidex_final , y = data$SOMME ,
main = 
paste("La demande contre l'effet de refroissiement Tref: ",22), 
     xlab = 'Humidit?? transform??e' , 
     ylab = "Somme de la demande d'??lectricit??" ,
     frame = FALSE)

data['humidex_final'] <- humidex_final


noisy_humidex <- c()
for (i in seq(1 , length(data$DATE))) {
  if (noisy_temp[i] > 22){
    noisy_humidex[i] = round(( (noisy_hum[i]**(1/2)) * 
                                 (noisy_temp[i] - 22) ),
                             digits = 4)
  } else {
    noisy_humidex[i] = 0
  }
}

data['noisy_humidex'] <- noisy_humidex

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

#Before and after holiday
boxplot( data$SOMME 
         ~ data$before_holi ,
         main = 'Effet des journ??es avant un cong??' ,
         xlab = 'Avant une journ??e f??ri??e' ,
         ylab = "Demande d'??lectricit??")

#Before and after holiday
boxplot( data$SOMME 
         ~ data$after_holiday ,
         main = 'Effet des journ??es apr??s un cong??' ,
         xlab = 'Apr??s une Journ??e f??ri??e' ,
         ylab = "Demande d'??lectricit??")


#Express the time with t starting at 1 to length of data to
#make coding easier
final_data['t'] <- seq(1 , length(final_data$DATE) , 1)

#errors by quarter
quarter <- c()
for (i in seq(1 , 365)) {
  if ( i <= 90) {
    quarter[i] = 'Q1'
  } else if ((i >= 91) & (i<=181) ) {
    quarter[i] = 'Q2'
  } else if ( (i>=182) & (i<=273)) {
    quarter[i] = 'Q3'
  } else {
    quarter [i] = 'Q4'
  }
}

quarter_2012 <- c('Q1' , quarter )
quarter_2016 <- c('Q1' , quarter )
quarter_2020 <- c('Q1' , quarter )
quarter_2021 <- quarter[1:364]

quarter <- c(quarter_2012 , quarter , quarter , quarter ,
             quarter_2016 , quarter , quarter , quarter ,
             quarter_2020 , quarter_2021) 

data['quarter'] <- quarter

#Enregistrer le df avec les variables utiles seulement
colonnes_utiles <- c("DATE" , "SOMME" , "CDD" , "HDD" , "weekday" ,
                     "CP" , "Holiday" , "humidex_final" , "before_holi",
                     "after_holiday" , "noisy_humidex" , "noisy_cp",
                     "noisy_HDD" , "noisy_CDD" , "quarter")

# subset
final_data <- data[colonnes_utiles]

save(final_data , file='regression_df.Rdata')
dev.off(dev.cur())