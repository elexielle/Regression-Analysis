#regression analysis activity

#loading packages
library(car)
library(carData)
library(zoo)
library(olsrr)
library(tseries)

#data import
library(readxl)
SAMPLE_DATA <- read_excel("school/regression analysis/spss & excel stuff/SAMPLE DATA.xlsx")
View(SAMPLE_DATA)

#creating a model
licen_model_orig <- lm(Licensure ~ Major_grade + `Year of Graduation`,
                       data = SAMPLE_DATA)

###--- TESTING ASSUMPTION 3 ---###
#durbin watson
durbinWatsonTest(licen_model_orig)

#ljung-box q test
orig_model_resid <- resid(licen_model_orig)
Box.test(orig_model_resid, lag = 1, type = "Ljung")

###--- TESTING ASSUMPTION 4 ---###
orig_model_outlier <- cooks.distance(licen_model_orig)
n <- nrow(SAMPLE_DATA)

plot(orig_model_outlier, main = "cook's distance")
abline(h = 4/n, lty = 2, col = "red")

influential_obs <- as.numeric(names(orig_model_outlier)[orig_model_outlier>(4/n)])
influential_obs

influential_obs <- as.numeric(names(outlier)[outlier>(4/n)])
influential_obs

outlier_removed <- SAMPLE_DATA[-influential_obs, ]
View(outlier_removed)

###--- TESTING ASSUMPTION 5 ---###
hist(orig_model_resid, main = "histogram") 
ols_test_normality(licen_model_orig)
jarque.bera.test(orig_model_resid)



