#regression analysis

#import data
library(readxl)
SAMPLE_DATA <- read_excel("~/school/regression analysis/spss & excel stuff/SAMPLE DATA.xlsx")
View(SAMPLE_DATA)

attach(SAMPLE_DATA) #active excel file 

#install.packages("lmtest")
#install.packages("carData")
#install.packages("car")
library(lmtest)
library(carData)
library(car)

#creating a model
licen_model_orig <- lm(Licensure ~ Major_grade + `Year of Graduation`,
                       data = SAMPLE_DATA)

###--- TESTING ASSUMPTION 1 ---###
#1. component-plus residual plots
crPlots(licen_model_orig)
#2. using ramsey reset test
resettest(licen_model_orig)
#p-value = 0.2702: fail to reject ho

summary(licen_model_orig)

###--- TESTING ASSUMPTION 2 ---###
#3. breusch-pagan-godfrey test
lmtest::bptest(licen_model_orig)
#p-value = 0.00102

#3. white's general heteroscedasticity test

#install.packages("vars")
#install.packages("het.test")
library(vars)
library(het.test)

#create a dataframe
year_mgrade_df <- data.frame(Licensure = SAMPLE_DATA$Licensure, 
                             Compre_grade = SAMPLE_DATA$Major_grade,
                             Year_of_Graduation = SAMPLE_DATA$`Year of Graduation`)
View(year_mgrade_df)

var_format <- VAR(year_mgrade_df, p=1)
whites.htest(var_format)

###--- TRANSFORMATION ---###
#4. box-cox transformation
boxcox_model <- caret::BoxCoxTrans(SAMPLE_DATA$Licensure)
print(boxcox_model)

SAMPLE_DATA <- cbind(SAMPLE_DATA, bclicensure = predict(boxcox_model,SAMPLE_DATA$Licensure))

licen_model_bc <- lm(bclicensure ~ Major_grade + `Year of Graduation`, SAMPLE_DATA)
summary(licen_model_bc)

#assumptions check
#using ramsey reset test
resettest(licen_model_bc)
# using breusch-pagan-godfrey test
lmtest::bptest(licen_model_bc)

#5. log transformation
log_major_grade <- log(Major_grade)
SAMPLE_DATA <- data.frame(SAMPLE_DATA, `Year of Graduation`, log_major_grade)
licen_model_log <- lm(Licensure ~ log_major_grade + `Year of Graduation`, SAMPLE_DATA)
summary(licen_model_log)
#assumptions check
#using ramsey reset test
resettest(licen_model_log)
# using breusch-pagan-godfrey test
lmtest::bptest(licen_model_log)

#6. weighted least squares
licen_weight <- 1/lm(abs(licen_model_orig$residuals) ~ 
                       licen_model_orig$fitted.values)$fitted.values^2
licen_model_wls <- lm(Licensure ~ Major_grade + `Year of Graduation`,
                      SAMPLE_DATA, weights = licen_weight)
summary(licen_model_wls)

#assumptions check
#using ramsey reset test
resettest(licen_model_wls)
# using breusch-pagan-godfrey test
lmtest::bptest(licen_model_wls)

