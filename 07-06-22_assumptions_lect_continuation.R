#regression #assumptions

library(readxl)
SAMPLE_DATA <- read_excel("school/regression analysis/spss & excel stuff/SAMPLE DATA.xlsx")
View(SAMPLE_DATA)


library(car)
library(carData)

model1 <- lm(Licensure ~ Compre_grade + Major_grade, SAMPLE_DATA)
summary(model1)


#assumption 3
durbinWatsonTest(model1)


#install.packages"zoo(
library(zoo)
Box.test(resid(model1), lag = 1, type = "Ljung")

#assumption 4
outlier <- cooks.distance(model1) #identify the outliers
outlier

n <- nrow(SAMPLE_DATA)
n
plot(outlier, main = "Cooks Distance")
abline(h = 4/n, lty = 2, col = "red")


influential_obs <- as.numeric(names(outlier)[outlier>(4/n)])
influential_obs

outlier_removed <- SAMPLE_DATA[-influential_obs, ] #inside sample data removed influential_obs per row
                                #[col, row]
View(outlier_removed)

#assumption 5 normality test
hist(resid(model1), col = "blue", main = "histogram")
#install.packages("olsrr")  
library(olsrr)  
ols_test_normality(model1)  #resid of mdel1 violated the assumption
#lomongorov sensitive to equal values
shapiro.test(resid1)
#install.packages("nortest")
library(nortest)  
normtest::jb.norm.test(resid1)




