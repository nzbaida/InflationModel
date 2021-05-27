rm(list = ls())
library(sandwich)
setwd("C:/Users/natty/OneDrive/Documents/Econ 143")
data <- read.csv("Final Data.csv")
head(data)
#Base Regression
firstreg <- lm(Inflation.Percentage ~ Unemployment.Difference + TB3MS, data = data)
firstregsum <- summary(firstreg)
prediction1 <- (firstregsum$coefficients[1,1] + firstregsum$coefficients[2,1]*data$Unemployment.Difference 
+ firstregsum$coefficients[3,1]*data$TB3MS)
data$error1 <- (data$Inflation.Percentage - prediction1)
firstregsum
#White Test for Heteroskedasticity
white <- lm(I(error1^2) ~ Unemployment.Difference + TB3MS, data = data)
whitesum <- summary(white)
whitesum
plot(as.Date(data$ï..Date), data$error1,xlab = "Dates", ylab = "Residuals")
#HAC Estimators First Regression
vcovmat <- vcovHAC(firstreg)
vcovmat
beta1p <- firstregsum$coefficients[1,1]/vcovmat[1,1]
beta1p
beta1SE <- sqrt(vcovmat[1,1])
beta1SE
beta2p <- firstregsum$coefficients[2,1]/vcovmat[2,2]
beta2p
beta2SE <- sqrt(vcovmat[2,2])
beta2SE
beta3p <- firstregsum$coefficients[3,1]/vcovmat[3,3]
beta3p
beta3SE <- sqrt(vcovmat[3,3])
beta3SE
#RESET Test (second regression)
secondreg <- lm(Inflation.Percentage ~ Unemployment.Difference + TB3MS + I(Unemployment.Difference^2) 
                + Unemployment.Difference*TB3MS + I(TB3MS^2) , data = data)
secondregsum <- summary(secondreg)
secondregsum
#HAC estimators second regression
vcovmat2 <- vcovHAC(secondreg)
vcovmat2
beta1p2 <- secondregsum$coefficients[1,1]/vcovmat2[1,1]
beta1p2
beta1SE2 <- sqrt(vcovmat2[1,1])
beta1SE2
beta2p2 <- secondregsum$coefficients[2,1]/vcovmat2[2,2]
beta2p2
beta2SE2 <- sqrt(vcovmat2[2,2])
beta2SE2
beta3p2 <- secondregsum$coefficients[3,1]/vcovmat2[3,3]
beta3p2
beta3SE2 <- sqrt(vcovmat2[3,3])
beta3SE2
beta4p2 <- secondregsum$coefficients[4,1]/vcovmat2[4,4]
beta4p2
beta4SE2 <- sqrt(vcovmat2[4,4])
beta4SE2
beta5p2 <- secondregsum$coefficients[5,1]/vcovmat2[5,5]
beta5p2
beta5SE2 <- sqrt(vcovmat2[5,5])
beta5SE2