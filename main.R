#
# Program: main.R
#
# Purpose: running the whole pipeline at once and modular view for
# ease of debugging
#
# Written by: Team G, January 30 2022
#
# Updated: March 17th 2022
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
# below the data transformation is also done in explor_vars.R
source("./exploratory_vars.R") # load the explor analysis
source("./eda_target.R") # eda target analysis + pdf reporting
source("./report_explanatory_vars.R") # report into pdf the 
# exploratory_vars.R output
source("./naive_methods.R") # naive method evaluation for
# baseline performance + pdf reporting
print("Double Check successful completion of part 1")
# running the smoothing methods
print("RUNNING SMOOTHING METHODS")
source("./smoothing_methods.R")
# running the ets models
print("RUNNING ETS")
source("./state_space_models.R")
# # running TBATS
print("RUNNING TBATS")
source("./TBATS.R")
# running corrections
print("Run corrections of phase 1")
source("./correction_phase_1.R")
print("timeseries_x for regression and phase 3 launches")
source("./timeseries_x.R")
print("timeseries_x script completed successfully")
# print("Run regressions section")
# source("./regression.R")
print("Launching section for phase 3")
source("./sarima.R")
print("Run regressions section - correction for phase 2")
source("./regression_correction.R")
print("Run ARX section")
source("./arx_model.R")
print("Double Check successful completion of part 3")