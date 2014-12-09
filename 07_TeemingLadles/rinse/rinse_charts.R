



########################################################
##
## charting the rinse effect in the HIC steel grade
##
## Date: Aug-14 2014
##
########################################################
rm(list=ls())
setwd("C:/Users/Public/Documents/RDataAnalysis/07_TeemingLadles")
rinse <- read.csv('./rinse/rinse.csv')
names(rinse)[1] <- 'rinse'

## computing the relative drop
rinse$queda <- (rinse$pbs-rinse$pcs)/rinse$pbs

plot(rinse$queda ~ jitter(rinse$rinse, amount=0.05), cex=1.2, pch=19, col='red', xlim=c(-0.5, 1.5), xaxt='n', ylab='(pbs-pcs)/pbs)', xlab='Rinse')
boxplot(rinse$queda ~ rinse$rinse, col=NA, at=c(0, 1), boxwex=0.3, add=TRUE, xaxt="n")
axis(side=1, at=c(0, 1), labels=c("SEM", 'COM'))
title('Reduçao percentual do teor de [P]')

library(qualityTools)

table(rinse$rinse)
old.par <- par()

par(mar=c(5,5,6,2)+0.1)
plot  (rinse$pbs[rinse$rinse==0] ~ rep(1, 7), pch=19, cex=1.2, col='blue', xlim=c(0, 9), ylim=c(0,12), xaxt='n', xlab='Rinse', ylab='[P], pontos')
points(rinse$pcs[rinse$rinse==0] ~ rep(3, 7), pch=19, cex=1.2, col='blue', xlim=c(0, 9), ylim=c(0,12), xaxt='n', xlab=NA, ylab=NA, yaxt='n')
points(rinse$pbs[rinse$rinse==1] ~ rep(6, 4), pch=19, cex=1.2, col='red',  xlim=c(0, 9), ylim=c(0,12), xaxt='n', xlab=NA, ylab=NA, yaxt='n')
points(rinse$pcs[rinse$rinse==1] ~ rep(8, 4), pch=19, cex=1.2, col='red',  xlim=c(0, 9), ylim=c(0,12), xaxt='n', xlab=NA, ylab=NA, yaxt='n')
axis(side=1, at=c(2,7), labels=c('SEM', 'COM'))
axis(side=3, at=c(1, 3, 6, 8), labels=c('BS', 'CS', 'BS', 'CS'))
title('Efeito do rinse no teor de [P]')

# blue lines
linhas <- data.frame( rep(1,7), rinse$pbs[rinse$rinse==0], rep(3,7), rinse$pcs[rinse$rinse==0])
names(linhas) <- c('x1', 'y1', 'x2', 'y2')
for(i in 1:dim(linhas)[1] ){
  lines(x=c(linhas$x1[i], linhas$x2[i]), y=c(linhas$y1[i], linhas$y2[i]), col='blue')
}

# red lines
linhas <- data.frame( rep(6,4), rinse$pbs[rinse$rinse==1], rep(8,4), rinse$pcs[rinse$rinse==1])
names(linhas) <- c('x1', 'y1', 'x2', 'y2')
for(i in 1:dim(linhas)[1] ){
  lines(x=c(linhas$x1[i], linhas$x2[i]), y=c(linhas$y1[i], linhas$y2[i]), col='red')
}
