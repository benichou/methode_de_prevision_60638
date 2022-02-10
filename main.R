#
# Program: main.R
#
# Purpose: running the whole pipeline at once and modular view for
# ease of debugging
#
# Written by: Team G, January 30 2022
#
# Updated: Feb 10th 2022
#          
#         
#          
#         
#          
# ------------------------------------------------------
## load all necessary packages

library(forecast)
library(timeSeries)
library(zoo)

source("./functions.R") # load the functions
# below the data transformation is also done
source("./exploratory_vars.R") # load the explor analysis
source("./eda_target.R") # eda target analysis + pdf reporting
source("./report_explanatory_vars.R") # report into pdf the 
# exploratory_vars.R output
source("./naive_methods.R") # naive method evaluation for
# baseline performance + pdf reporting
print("Double Check successful completion")
