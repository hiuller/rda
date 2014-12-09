rm(list=ls())
setwd("J:/RDataAnalysis/02_FeMn_Selection")
setwd("C:/Users/Public/Documents/RDataAnalysis/02_FeMn_Selection")

rank <- read.csv("./data/01_alloy_importance_rank.csv")
names(rank)[1] <- "ligas"
rank$cst_spec <- round(rank$custo/rank$peso, 2)

old.par <- par()

pdf("latex/figures/fig01.pdf", height=6, width=6)
par(mfcol=c(1,1), mai=rep(0.5, 4), oma=c(1,1,1,1), mar=c(5,4,2,2)+0.1, xpd=NA) #c(bottom, left, top, right)
par(mar=c(5,4,2,2)+0.1) # this is a no-title margin
plot(cst_spec ~ peso, data=rank, pch=19, col="blue", cex=1.5, 
     ylab="Custo da liga, R$/t", xlab="Peso consumido, t",
     ylim=c(2000, 6000), xlim=c(2000, 10000))
text(y=rank$cst_spec, x=rank$peso, labels=rank$ligas, pos=3, offset=0.8)
par(old.par)
dev.off() # clear the graphical output