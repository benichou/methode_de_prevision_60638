#
#
#
# Program: global_comparison_diebold.R 
#
# Purpose: Comparing whether the validation predcition vectors
# for linear regression and arx are different than naive benchmark
# and sarima and tbats and then different than one another
#
#
# Written by: Team G, April 10 2022
#
# Updated: April 10th 2022
#          
#         
#          
#         
#          
# ------------------------------------------------------


# load the best model arx lag 7 val and test vectors

load("arxmodel_test.RData")
load("arxmodel_valid.RData")
# arxmodel_valid
# arxmodel_test
# load the best linear regression model with ARMA error validation
# and test vectors for diebold mariano comparison
load("armamodel_test.RData")
load("armamodel_valid.RData")
# armamodel_valid
# armamodel_test


# compare naive no change vs linear regression with
# arma error

print(dm.test((c(forecast_next_day)-c(out.sample)), 
(armamodel_valid-out.sample)))

# data: (forecast_next_day - out.sample)(armamodel_valid -out.sample)
# DM = 10, Forecast horizon = 1, Loss function power = 2, p-value
# <0.0000000000000002
# alternative hypothesis: two.sided

## Significant difference between naive no change and linear 
## regression with arma error


# compare naive no change vs arx model with lag 7

print(dm.test((c(forecast_next_day)-c(out.sample)), 
(arxmodel_valid-c(out.sample))))

#         Diebold-Mariano Test

# data:(c(forecast_next_day) - c(out.sample))
# (arxmodel_valid - c(out.sample))
# DM = -6, Forecast horizon = 1, Loss function power = 2, p-value =
# 0.000000007
# alternative hypothesis: two.sided

# ARX model forecast are statistically significantly different
## than the naive no change

## sarima vs linear regression with arma error

print(dm.test((c(fc1_no_retrain_exp)-c(out.sample)), 
(armamodel_valid-c(out.sample))))

#         Diebold-Mariano Test

# data:  (c(fc1_no_retrain_exp) - c(out.sample))
# (armamodel_valid - c(out.sample))
# DM = 9, Forecast horizon = 1, Loss function power = 2, p-value
# <0.0000000000000002
# alternative hypothesis: two.sided

# SARIMA vs linear regression with arma errors are statistically
# significantly different

## sarima vs arx with arma error

print(dm.test((c(fc1_no_retrain_exp)-c(out.sample)), 
(arxmodel_valid-c(out.sample))))


# data:  (c(fc1_no_retrain_exp) 
# - c(out.sample))(arxmodel_valid - c(out.sample))
# DM = 9, Forecast horizon = 1, Loss function power = 2, p-value
# <0.0000000000000002
# alternative hypothesis: two.sided

# SARIMA vs ARX model lag 7 are statistically
# significantly different


## tbats with retrain vs linear regression with arma errors

print(dm.test((c(fc.exp[1:365],fcasts.r)-c(out.sample)), 
(armamodel_valid-c(out.sample))))

# data:  (c(fc.exp[1:365], fcasts.r) -
#  c(out.sample))(armamodel_valid - c(out.sample))
# DM = 9, Forecast horizon = 1, Loss function power = 2, p-value
# <0.0000000000000002
# alternative hypothesis: two.sided

# TBATS vs linear regression with ARMA errors are statistically
# significantly sifferent at 5% in the quadratic average of their 
# errors

## tbats with retrain vs arx

print(dm.test((c(fc.exp[1:365],fcasts.r)-c(out.sample)), 
(arxmodel_valid-c(out.sample))))

# data:  (c(fc.exp[1:365], fcasts.r) - 
# c(out.sample))(arxmodel_valid - c(out.sample))
# DM = 9, Forecast horizon = 1, Loss function power = 2, p-value
# <0.0000000000000002
# alternative hypothesis: two.sided

# TBATS vs arx model are statistically
# significantly sifferent at 5% in the quadratic average of their 
# errors

# Winning ARX vs linear regression with ARMA error model

print(dm.test((arxmodel_valid-c(out.sample)), 
(armamodel_valid-c(out.sample))))

#         Diebold-Mariano Test

# data:  (arxmodel_valid - c(out.sample))
# (armamodel_valid - c(out.sample))
# DM = -0.007, Forecast horizon = 1, 
# Loss function power = 2, p-value = 1
# alternative hypothesis: two.sided

## We can not reject the null hypothesis H0 
# that the average of the difference 
# of the quadratic errors of arx and linear regression 
# with arma errors is equal to 0 and thus that they are equal
# We conclude at critical threshold of 5% that there is no
# advantage of choosing one vs the other
# However, because we could observe that the ME is much lower
# with ARX, we shall select ARX as the winning model



