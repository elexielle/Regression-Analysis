#regression analysis
#component-plus residual plots

#import data
library(readxl)
SAMPLE_DATA <- read_excel("~/school/regression analysis/spss & excel stuff/SAMPLE DATA.xlsx")
View(SAMPLE_DATA)

attach(SAMPLE_DATA) #active excel file 

#install needed packages
#install.packages("https://cran.r-project.org/src/contrib/Archive/het.test/het.test_0.1.tar.gz", repos = NULL, type="source")
    #for the het.test package #white test
#install.packages("lmtest")
#install.packages("carData")
#install.packages("car")
library(lmtest)
library(carData)
library(car)

###--- TESTING ASSUMPTION 1 ---###
#lm test is used to create regression model
model1 <- lm(Licensure ~ Compre_grade + Major_grade, SAMPLE_DATA)
summary(model1)
crPlots(model1)
#major grade is linear in parameters
#compre grade is not 
    #transform using log (commonly used)

#using ramsey reset test
resettest(model1)
  # p-value = 0.001233 => reject ho (not linear in parameters) 

#transformation of data
logCompre_grade <- log(Compre_grade)
SAMPLE_DATA <- data.frame(SAMPLE_DATA, logCompre_grade)

#testing of model 
model2 <- lm(Licensure ~ logCompre_grade + Major_grade, SAMPLE_DATA)
crPlots(model2)
resettest(model2)
  #p-value = 0.002629 => still reject ho; think of other transformation

#breusch-pagan test
lmtest::bptest(model1)

#install.packages("vars)
#install.packages("het.test)
library(vars)
library(het.test)
data11 <- data.frame(Licensure = SAMPLE_DATA$Licensure, 
                     Compre_grade = SAMPLE_DATA$Compre_grade,
                     Major_grade = SAMPLE_DATA$Major_grade)
v <- VAR(data11, p=1)
whites.htest(v)

#---remedial measures---#
#weighted least squares
#define weights to use
w <- 1/lm(abs(model1$residuals)~model1$fitted.values)$fitted.values^2
wls_model <- lm(Licensure~Compre_grade + Major_grade, SAMPLE_DATA, weights = w)
summary(wls_model)

#original model
summary(model1)

#breusch-pagan test
lmtest::bptest(wls_model)

#white's remedial measures
#install.packages("sandwich")
library(sandwich)
coeftest(model1, vcov. = vcovHC(model1, type = "HC1"))

#boxcox transformation
BCmodel <- caret::BoxCoxTrans(SAMPLE_DATA$Licensure)

print(logCompre_grade)
print(BCmodel)

SAMPLE_DATA <- cbind(SAMPLE_DATA,
                     BClicensure = predict(BCmodel, SAMPLE_DATA$Licensure))

model3 <- lm(BClicensure~Compre_grade+Major_grade, SAMPLE_DATA)
summary(model3)
lmtest::bptest(model3)
crPlots(model3)
