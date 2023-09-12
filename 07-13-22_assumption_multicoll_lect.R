#regression analysis  #assumptions
#multicollinearity: IVs are independent from e/o
    #when IVs are correlated, can be considered as one factor

# vif=1/(1-r^2)
  #r^2 => 0.7 & up IV can

# a. 5x, b.3x, c. 

#loading packages
library(car)
library(carData)

#data import
library(readxl)
SAMPLE_DATA <- read_excel("school/regression analysis/spss & excel stuff/SAMPLE DATA.xlsx")
View(SAMPLE_DATA)
attach(SAMPLE_DATA)

#creating a model
model1 <- lm(Licensure ~ Compre_grade + Major_grade, SAMPLE_DATA)

vif(model1)
dependent <- SAMPLE_DATA$Licensure
compre <- SAMPLE_DATA$Compre_grade
compre5x <- 5*compre 
set1 <- data.frame(dependent, compre, compre5x)
modelA <- lm(dependent~compre+compre5x, set1)
vif(modelA)
summary(model1)

##
compre3x <- compre
set2 <- data.frame(dependent, compre, )
modelB <- lm()



#to det best model
  #look for best combinations
  #ex. 10 run: 5 sig & 5 not sig
      #5 significant => run again; possible not all are sig again
      #4 sig => run again 
  #magbawas isa isa
  #look for lev of accu; small std error, small mse, 
  #standardized beta coeff: higher val higher contribution
  #normality, 












