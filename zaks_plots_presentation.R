#loading the data
load('regression_df.Rdata')
load('master_df.Rdata')

#exploratory variables

#figure 1 , cdd and hdd
plot(x = data$CDD , 
     y = data$SOMME/1000 , 
     pch = 1 ,
     col = "darkblue",
     xlab = "HDD et CDD" ,
     ylab = "Demande quotidienne (MW/h)")
points( x = data$HDD , y = data$SOMME/1000 ,
        col = "darkred" , pch = 1 , lty = 2)
legend("topleft" , legend = c("CDD" , "HDD") ,
       pch = c(19,19) , col = c("darkblue" , "darkred"))


#figure 2 , humidex
plot(x = final_data$humidex_final , 
     y = final_data$SOMME/1000 , 
     pch = 1 ,
     col = "midnightblue",
     xlab = "Humidex" ,
     ylab = "Demande quotidienne (MW/h)")

#figure 3 , froid ressenti
plot(x = final_data$CP , 
     y = final_data$SOMME/1000 , 
     pch = 1 ,
     col = "midnightblue",
     xlab = "Refroidissement éolien" ,
     ylab = "Demande quotidienne (MW/h)")

#figure 4 , boxplot de type of day vs demande
plot.new()


# Light gray background
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
     col = "#ebebeb")

# Add white grid
grid(nx = NULL, ny = NULL, col = "white", lty = 1,
     lwd = par("lwd"), equilogs = TRUE)

# Boxplot
par(new = TRUE )
boxplot(data$SOMME/1000 ~ data$type_of_day, # Data
        horizontal = FALSE, # Horizontal or vertical plot
        lwd = 2, # Lines width
        col = c("orange" , "skyblue" , "palegreen" , "yellow"),
        xlab = "Type de journée",  # X-axis label
        ylab = "Demande quotidienne (MW/h)",  # Y-axis label
        border = "black",  # Boxplot border color
        outpch = 25,       # Outliers symbol
        outbg = "red",   # Outliers color
        whisklty = 1,      # Whisker line type
        lty = 1) # Line type (box and median)

# Add a legend
par(xpd=TRUE)
legend("topright", 
       legend = c("1: Semaine" , "2: Weekend" , "3: Férié" ,
                  "4: Férié et Weekend"), 
       fill = c("orange" , "skyblue" , "palegreen" , "yellow"), 
       inset = c(-0.00, 0.0), # Modify margins
       bg = "white" ,
       cex = 0.8) # Legend background color

#figure 5 :

data$month <- factor(data$month , 
levels = c("January" ,"February", "March","April" ,"May","June" 
           ,"July","August" ,"September","October","November",
           "December"))
#Demande par mois
plot.new()


# Light gray background
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
     col = "#ebebeb")

# Add white grid
grid(nx = NULL, ny = NULL, col = "white", lty = 1,
     lwd = par("lwd"), equilogs = TRUE)

# Boxplot
par(new = TRUE )
boxplot(data$SOMME/1000 ~ data$month, # Data
        horizontal = FALSE, # Horizontal or vertical plot
        lwd = 2, # Lines width
        col = rainbow(12),
        xlab = "Mois",
        ylab = "Demande quotidienne (MW/h)",  # Y-axis label
        border = "black",  # Boxplot border color
        outpch = 25,       # Outliers symbol
        outbg = "red",   # Outliers color
        whisklty = 1,      # Whisker line type
        lty = 1) # Line type (box and median)



