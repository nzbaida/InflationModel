rm(list = ls())
library(sandwich)
library(lmtest)
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
#Durbin-Watson Test for Autocorrelation
dwtest(firstreg)
#HAC Estimators First Regression
vcovmat <- vcovHAC(firstreg)
vcovmat
beta1t <- firstregsum$coefficients[1,1]/vcovmat[1,1]
beta1t
beta1p <- 1 - pnorm(beta1t)
beta1p
beta1SE <- sqrt(vcovmat[1,1])
beta1SE
beta2t <- firstregsum$coefficients[2,1]/vcovmat[2,2]
beta2t
beta2p <- pnorm(beta2t)
beta2p
beta2SE <- sqrt(vcovmat[2,2])
beta2SE
beta3t <- firstregsum$coefficients[3,1]/vcovmat[3,3]
beta3t
beta3p <- 1 - pnorm(beta3t)
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
beta1t2 <- secondregsum$coefficients[1,1]/vcovmat2[1,1]
beta1t2
beta1p2 <- 1 - pnorm(beta1t2)
beta1p2
beta1SE2 <- sqrt(vcovmat2[1,1])
beta1SE2
beta2t2 <- secondregsum$coefficients[2,1]/vcovmat2[2,2]
beta2t2
beta2p2 <- pnorm(beta2t2)
beta2p2
beta2SE2 <- sqrt(vcovmat2[2,2])
beta2SE2
beta3t2 <- secondregsum$coefficients[3,1]/vcovmat2[3,3]
beta3t2
beta3p2 <- 1 - pnorm(beta3t2)
beta3p2
beta3SE2 <- sqrt(vcovmat2[3,3])
beta3SE2
beta4t2 <- secondregsum$coefficients[4,1]/vcovmat2[4,4]
beta4t2
beta4p2 <- 1 - pnorm(beta4t2)
beta4p2
beta4SE2 <- sqrt(vcovmat2[4,4])
beta4SE2
beta5t2 <- secondregsum$coefficients[5,1]/vcovmat2[5,5]
beta5t2
beta5p2 <- 1 - pnorm(beta5t2)
beta5p2
beta5SE2 <- sqrt(vcovmat2[5,5])
beta5SE2
beta6t2 <- secondregsum$coefficients[6,1]/vcovmat2[6,6]
beta6t2
beta6p2 <- 1 - pnorm(beta6t2)
beta6p2
beta6SE2 <- sqrt(vcovmat2[6,6])
beta6SE2

