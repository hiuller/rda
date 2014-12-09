rm(list=ls())
setwd("C:/Users/Public/Documents/RDataAnalysis/06_Fluorite")
data <- read.csv("./data/fluorite2.csv") ## new dataset

###############################################################################
## non ulc steel
###############################################################################

  # removing NA values
  #====================
  mild <- subset(x=data, data$C_VIS > 2)
  mild <- mild[mild$P_MIN == 0,]    
  mild <- mild[!is.na(mild$pbs),]
  mild <- mild[!is.na(mild$TFS),]
  
  ## we have three groups:
  ##  1) no fluorite at all - 0 - NÂO
  ##  2) false fluorite - 1 placebo
  ##  3) real fluorite - 3 - SIM
  
  ## let's decode this on a indicator variable
  grp2 <- as.numeric(mild$NEFE_REAL > 0)
  grp3 <- as.numeric(mild$NEFE > 0)*(as.numeric(substr(mild$NUM_CORR_ACI, 1, 1)) - 3)
  ## response
  #===========
  pfs <- mild$pbs
  
  ## confounders
  #=============
  tfs <- mild$TFS
  cfs <- mild$cbs  
    mild$CARGA_LIQUIDA[mild$CARGA_LIQUIDA == 357880] <- 155000
    mild$CARGA_LIQUIDA[mild$CARGA_LIQUIDA == 207850] <- 155000
  mgo <- mild$MGO_INPUT/((mild$SUCATA_TOTAL+mild$CARGA_LIQUIDA)/1000.0)     
  cao.si <- mild$CAO_INPUT/(mild$CARGA_LIQUIDA * mild$SI_GUSA/10000.0)
  pgusa <- mild$P_GUSA
    pgusa[pgusa == 13] <- 100
    pgusa[pgusa == 41] <- 100


## i'll use my own script to remove outliers
source("C:/Users/Public/Documents/RDataAnalysis/rscripts/removeol.r")

    ## this data.frame will be used to do isonomy testings
    frame    <- data.frame(grp2, grp3, pfs, tfs, cfs, mgo, cao.si, pgusa)
    frameol1 <- removeol(frame   , 4:length(frame), draw=T)
    frameol2 <- removeol(frameol1, 4:length(frame), draw=T)

    for(i in 4:length(frame)) 
    {
      boxplot(frameol2[,i] ~ frameol2$grp2, main=sprintf("Boxplot of %s", names(frameol2)[i]), xaxt="n")
      axis(side=1)
    }

    ## batch ANOVA testing
    #======================
    for(i in 4:length(frameol2) )  {
      for (j in 1:2) {        
        p <- anova( lm(frameol2[,i] ~ factor(frameol2[,j])) )$"Pr(>F)"[1] 
        nomes <- names(frameol2)
        print(sprintf("The p-value of %s with regard to %s is %.3f", nomes[j], nomes[i], p))
      }    
    }
    rm(i, j, p, nomes)
    
    ## charts
    #=========
  plot(table(mild$P_MAX), main="Distribuiçao do [P] visado", ylab="[P], em pontos", xlab="[P] visado")
  table(mild$P_MAX)

  boxplot(frameol2$tfs ~ frameol2$grp2, main="TFS em funçao da adiçao de sodalita", at=c(0,1), xaxt="n")
   axis(side=1, at=c(0,1), labels=c("NÃO", "SIM") )
  boxplot(frameol2$mgo ~ frameol2$grp2, main="MgO em funçao da adiçao de sodalita", at=c(0,1), xaxt="n")
   axis(side=1, at=c(0,1), labels=c("NÃO", "SIM") )

  boxplot(frameol2$tfs ~ frameol2$grp3, main="TFS em funçao da adiçao de sodalita", at=c(0,1,2), xaxt="n")
   axis(side=1, at=c(0,1,2), labels=c("NÃO", "PLACEBO", "SIM") )
  boxplot(frameol2$mgo ~ frameol2$grp3, main="MgO em funçao da adiçao de sodalita", at=c(0,1,2), xaxt="n")
   axis(side=1, at=c(0,1,2), labels=c("NÃO", "PLACEBO", "SIM") )


## analysis of tfs and mgo
## mgo
mgonao <- frameol2$mgo[frameol2$grp3 == 0]
mgopla <- frameol2$mgo[frameol2$grp3 == 1]
mgosim <- frameol2$mgo[frameol2$grp3 == 2]
teste.mgo <- t.test(x=mgonao, y=mgosim)
teste.mgo
## tfs
tfsnao <- frameol2$tfs[frameol2$grp3 == 0]
tfspla <- frameol2$tfs[frameol2$grp3 == 1]
tfssim <- frameol2$tfs[frameol2$grp3 == 2]
teste.tfs <- t.test(x=tfsnao, y=tfssim)
teste.tfs

## final testing
#================
anova.pfs2 <- anova( lm(frameol2$pfs ~ frameol2$grp2) )
anova.pfs3 <- anova( lm(frameol2$pfs ~ frameol2$grp3) )





  ############# TEMPERATURE
  old.par <- par()
  par(mfcol=c(1, 2), cex=1)
  par( mar=c(5.1, 4.1, 2.1, 2.1) )      

        plot(frameol2$tfs ~jitter(frameol2$grp2, amount=.05), pch=19, 
             col=frameol2$grp2+2, xlim=c(-0.25, 1.25),
             xaxt="n", ## without x axis,
             xlab="Dois agrupamentos", ylab="Temperatura fim de sopro, ºC")
        boxplot(frameol2$tfs ~ frameol2$grp2, col=NA, yaxt="n", xaxt="n", at=c(0, 1), boxwex=0.3, add=TRUE)
        axis(side=1, at=c(0,1), labels=c("NÃO", "SIM"))
        

        plot(frameol2$tfs ~jitter(frameol2$grp3, amount=.05), pch=19, 
             col=frameol2$grp3+2, xlim=c(-0.25, 2.25),
             xaxt="n", ## without x axis,
             xlab="Três agrupamentos", ylab=NA)
        boxplot(frameol2$tfs ~ frameol2$grp3, col=NA, yaxt="n", xaxt="n", at=c(0, 1, 2), boxwex=0.3, add=TRUE)
        axis(side=1, at=c(0,1, 2), labels=c("NÃO", "PLACEBO", "SIM"))

  par(mfcol=c(1,1))
  ############# MGO

  par(mfcol=c(1, 2), cex=1)
  par( mar=c(5.1, 4.1, 2.1, 2.1) )

        plot(frameol2$mgo ~jitter(frameol2$grp2, amount=.05), pch=19, 
             col=frameol2$grp2+2, xlim=c(-0.25, 1.25),
             xaxt="n", ## without x axis,
             xlab="Dois agrupamentos", ylab="Input de MgO, kg/t")
        boxplot(frameol2$mgo ~ frameol2$grp2, col=NA, yaxt="n", xaxt="n", at=c(0, 1), boxwex=0.3, add=TRUE)
        axis(side=1, at=c(0,1), labels=c("NÃO", "SIM"))
        
        plot(frameol2$mgo ~jitter(frameol2$grp3, amount=.05), pch=19, 
             col=frameol2$grp3+2, xlim=c(-0.25, 2.25),
             xaxt="n", ## without x axis,
             xlab="Três agrupamentos", ylab=NA)
        boxplot(frameol2$mgo ~ frameol2$grp3, col=NA, yaxt="n", xaxt="n", at=c(0, 1, 2), boxwex=0.3, add=TRUE)
        axis(side=1, at=c(0,1, 2), labels=c("NÃO", "PLACEBO", "SIM"))

  par(mfcol=c(1,1))
        
  par(old.par)


  ## make two histograms of temperatures
  old.par <- par()

  ############################################################################
  ### CHART ONE
  par(old.par)
  plot(frameol2$pfs ~jitter(frameol2$grp2, amount=.05), pch=19, 
       col=frameol2$grp2+2, xlim=c(-0.25, 1.25),
       xaxt="n", ## without x axis,
       xlab="Utilizaçao de Sodalita-Nefelina", ylab="[P] fim de sopro")
  axis(side=1, at=c(0,1), labels=c("SIM", "NÃO"))
  title("Efeito da Sodalita-Nefelina no [P] fim de sopro")
  ############################################################################


  ############################################################################
  ### CHART TWO
  #c(bottom, left, top, right)
  par( mfcol=c(2,1), cex=1, las=0 ) ## layout for two rows and one column
  par( omi=c(0, 1, 0, 0.5) )          # outter margin in inches (outter-margin)
  par( mai=c(0, 0, 1, 0) )          # margin size in inches (margin-inches)
  hist(frameol2$tfs[frameol2$grp2 == 0], nclass=15, col="red",  xlim=c(1620, 1720), ylim=c(0, 20), main=NA, xaxt="n", ylab=NA, xlab=NA, ann=FALSE)
  par( mar=c(4, 0, 0, 0))
  hist(frameol2$tfs[frameol2$grp2 == 1], nclass=15, col="blue", xlim=c(1620, 1720), ylim=c(20, 0), main=NA,           ylab=NA, xlab="Temperatura, ºC")
  par(oma=c(0,0,5,0))
  title("Temperatura fim de sopro por tratamento", outer=TRUE)
  par(old.par)
  ############################################################################

  
  ############################################################################
  ### CHART TWO
  plot(frameol2$pfs ~ frameol2$tfs, pch=frameol2$grp3+17, cex=1.2, col=frameol2$grp3+2, xlab="TFS, ºC", ylab="[P] FS", xlim=c(1630, 1710))
  legend(x="topleft", legend=c("NÃO", "PLACEBO", "SIM"), pch=17:19, cex=0.9, col=2:4)
  title("PFS em funçao da TFS para os diferentes tratamentos")
  cor(x=frameol2$pfs, y=frameol2$tfs)
  
