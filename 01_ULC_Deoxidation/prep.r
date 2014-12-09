# PREP.R - pre-processing raw data
# In this script data is loaded to R using ODBC connectivity
# Then outliers are removed from 3 specific columns.
# At the end the result is saved to a csv file so
# it can be loaded further in the EDA analysis

rm(list=ls())

setwd("J:/RDataAnalysis/01_ULC_Deoxidation")

# in this function we don't use file extentions
# the original dataset was created in 
#   23/12/2013 @ 10:21 AM
#      source("sql2csv.r")
#      system.time(sql2csv("./sql/desox_ubc"))

data <- read.csv("./sql/desox_ubc.csv")

# will treat ALGR=ALGO and SIBC=SIBR
data$LIGA[data$LIGA == "ALGR "] <- "ALGO "

# dischange the other values other than ALGO or SIBR
data <- subset(data, LIGA=="ALGO " | LIGA=="SIBR ")
data$LIGA <- factor(data$LIGA)

table(data$LIGA)

source("removeol.r")
cols <- c(16, 17, 21) # remove outliers from the columns with these numbers

ol1 <- removeol(data, cols=cols, draw=FALSE)
ol2 <- removeol(ol1 , cols=cols, draw=FALSE)
ol3 <- removeol(ol2 , cols=cols, draw=FALSE)
rm(ol1, ol2)

# now we have a dataset without outliers in the 3 main variables
#    write.csv(ol3, "./data/data_wo_ol.csv")