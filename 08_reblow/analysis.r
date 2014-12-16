rm(list=ls())

setwd("C:/Users/Public/Documents/RDataAnalysis/08_reblow");
rawData <- read.csv("qry_ressopro_global.csv", sep=";");
names(rawData);

rawData$mes <- format(as.Date(rawData$DT_PRODC_OBTD_ACI, format="%d/%m/%Y"), format="%m")
rawData$ressopro <- (rawData$VOL_RESS > 40)

tapply(rawData$ressopro, rawData$mes, sum)/table(rawData$mes)

j <- table(rawData$TP_CORR)
incluir <- names(j)[j>100]

sub <- subset(rawData, subset=rawData$TP_CORR %in% incluir)
table(as.character(sub$TP_CORR))

sub$TP_CORR <- droplevels(sub$TP_CORR)
sub$TP_CORR


plot(sub$PERC_SUCATA ~ jitter(as.numeric(sub$TP_CORR)), pch=19, col='red', cex=1.2, axis=NA)