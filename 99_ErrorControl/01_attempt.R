rm(list=ls())
setwd("C:/Users/Public/Documents/RDataAnalysis/99_ErrorControl")

# let's work on some random data, alright?
set.seed(11259)
x1 <- rnorm(1000, mean=49.7, sd=12.93)
x2 <- rnorm(1000, mean= 5.3, sd= 1.07)
x3 <- rnorm(1000, mean=18.8, sd= 2.75)
x4 <- rnorm(1000, mean=55.5, sd= 18.9)

# this is a random noise to be added to the regression
err <- rnorm(1000, mean=5, sd=8)

# these are TRUE regression coefficients in some sort of a transfer function 
# that could finaly
a <- c(0.254, 1.364, 8.001, 0.0) # quanto maior o valor de a[i] maior a correlaçao entre xi e y.

# a[4]=0 means that there is no relation between x4 and y

y  <- rnorm(1, mean=7.8, sd=0.76) + a[1]*x1 + a[2]*x2 + a[3]*x3 + a[4]*x4 + err
y  <- 7.8                         + a[1]*x1 + a[2]*x2 + a[3]*x3 + a[4]*x4 + err

pairs(y~x1+x2+x3+x4, pch=19, col="red", cex=1.2)

# let's create a linear model
lm.1 <- lm(y ~ x1+x2+x3+x4)
summary(lm.1)

plot(y ~ lm.1$fitted.values, pch=19, col="blue", cex=1.2)
abline(a=0, b=1, lwd=2, col="red")
title("model lm.1")

plot(err ~ lm.1$residuals, pch=19, col="blue", cex=1.2)
abline(a=0, b=1, lwd=2, col="red")
title("residuals in lm.1 x true noise err")
