rm(list=ls())
setwd("C:/Users/Public/Documents/RDataAnalysis/02_FeMn_Selection")

source("C:/Users/Public/Documents/RDataAnalysis/01_ULC_deoxidation/removeol.r")

data <- read.csv("./data/doe_data.csv")

data$bloco1
names(data)[8]

data2 <- data[,-8]

model <- lm(rendimento ~ ., data=data2, singular.ok=TRUE)
summary(model)

names(model)
plot(model$fitted.values ~ data$rendimento, pch=19, cex=1.5, col="blue")

plot(model$residuals)
normp(model$residuals)

mean(model$residuals)
sum(model$residuals)