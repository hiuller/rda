rm(list=ls())

setwd("C:/Users/Public/Documents/RDataAnalysis/08_reblow");
rawData <- read.csv("qry_ressopro_global.csv", sep=";");
names(rawData);

rawData$mes <- format(as.Date(rawData$DT_PRODC_OBTD_ACI, format="%d/%m/%Y"), format="%m")
rawData$ressopro <- (rawData$VOL_RESS > 40)

tapply(rawData$ressopro, rawData$mes, sum)/table(rawData$mes)