rm(list=ls())
setwd("C:/Users/Public/Documents/RDataAnalysis/06_Fluorite")

# how to import data from access into R
data <- read.csv("./data/B1 join.csv")   ## old dataset
data <- read.csv("./data/fluorite2.csv") ## new dataset
#names(data)

  # this subdataset is only for ULC heats
  ulc <- subset(data, data$C_VIS <= 1.0)
  
    # the number of ulc whitout p1
    #sum(is.na(ulc$pp1)); sum(is.na(ulc$pbs)); m <- dim(ulc)[1];

  # only ULC with p1 sample
    ulcp1 <- subset(ulc, !is.na(ulc$pp1))
    #plot(pp1 ~ MGO_INPUT, data=ulcp1, pch=19, col="blue", cex=1.2)
    #
    data1 <- ulcp1[,c(3,6,7,10:25)]
    data1$sep1 <- NULL
    
    for(i in 1:length(data1)[1]) plot(data1[,i], ylab=names(data1[i]), pch=19, col="red", cex=1.3)
    # looking at those charts we can see that something has to be done in order to prepare the dataset
    # 1st: get rid of the heats with P addition:
    data1 <- data1[data1$P_MIN == 0,]
    # 1st: get rid of the heats without TA scrap
    data1 <- data1[data1$PS_TA >  0,]

    # a matrix plot of selected columns
    plot(data1[,c(2,3,4,5,11,12,13,14,15,16,17,18)], lower.panel=NULL, pch=19, col="blue", cex=0.8)

    # deal with some outliers here
    plot(pp1 ~ TPAN, data=data1, pch=19, col="red", cex=1.2)
    data1 <- data1[data1$TPAN > 1580,]  
    data1 <- data1[!is.na(data1$cp1),]

    # exploring correlations
    names(data1)[5]; names(data1)[18]
    data1 <- data1[!is.na(data1$TPAN),]
    cor(data1$pp1, data1$TPAN)
    plot(data1$pp1, data1$TPAN, pch=19, col="blue", cex=1.2)
    abline(coef=lm(data1$TPAN ~ data1$pp1)$coefficients, col="red", lwd=2.0)


data1$NEFE_REAL[is.na(data1$NEFE_REAL)] <- 0
hasNefe <- (data1$NEFE_REAL > 0)
stdModel <- lm(data1$pp1 [ hasNefe] ~ data1$TPAN[ hasNefe])
altModel <- lm(data1$pp1 [!hasNefe] ~ data1$TPAN[!hasNefe])

plot(data1$pp1 ~jitter(as.numeric(hasNefe), amount=.05), pch=19, col=hasNefe+2)

    # the endblow carbon is the same for the two samples?
    t.test(data1$cp1 ~ hasNefe)
    lm1 <- lm(data1$cp1 ~ hasNefe)
    an1 <- anova(lm1)


###############################################################################
## non ulc steel
###############################################################################

  mild <- subset(x=data, data$C_VIS > 2)
  mild <- mild[mild$P_MIN == 0,]  
  sum(is.na(mild$pbs)) ## (78=old; 135=new) heats without BS analysis
  mild <- mild[!is.na(mild$pbs),]
  mild <- mild[!is.na(mild$TFS),]
  plot(table(mild$P_MAX), main="Distribuiçao do [P] visado", ylab="[P], em pontos", xlab="[P] visado")
  table(mild$P_MAX)

  #input of CaO/input of Si
  sum(is.na(mild$P_GUSA))
  

  t.test(x=mild$pbs[mild$NEFE_REAL>0], y=mild$pbs[mild$NEFE_REAL==0])
  plot(mild$pbs ~ mild$TFS, pch=19, col=(mild$NEFE_REAL > 0)+2, cex=1.2)
  cor(mild$pbs, mild$TFS)^2


  ## we have three groups:
  ##  1) false fluorite
  ##  2) no fluorite at all
  ##  3) false fluorite
  ## let's decode this on a indicator variable
  grp2 <- as.numeric(mild$NEFE_REAL > 0)
  grp3 <- as.numeric(mild$NEFE > 0)*(as.numeric(substr(mild$NUM_CORR_ACI, 1, 1)) - 3)
  ## response
  pfs <- mild$pbs
  sum(is.na(pfs))
  ## confounders
  tfs <- mild$TFS
  cfs <- mild$cbs
  mgo <- mild$MGO_INPUT/((mild$SUCATA_TOTAL+mild$CARGA_LIQUIDA)/1000.0)
    mild$CARGA_LIQUIDA[mild$CARGA_LIQUIDA == 357880] <- 155000
    mild$CARGA_LIQUIDA[mild$CARGA_LIQUIDA == 207850] <- 155000
  cao.si <- mild$CAO_INPUT/(mild$CARGA_LIQUIDA * mild$SI_GUSA/10000.0)
  pgusa <- mild$P_GUSA
    pgusa[pgusa == 13] <- 100
    pgusa[pgusa == 41] <- 100

## i'll use my own script to remove outliers
source("C:/Users/Public/Documents/RDataAnalysis/rscripts/removeol.r")

    ## this data.frame will be used to do isonomy testings
    frame <- data.frame(grp2, grp3, pfs, tfs, cfs, mgo, cao.si, pgusa)
    frameol1 <- removeol(frame   , 4:length(frame))
    frameol2 <- removeol(frameol1, 4:length(frame))

    for(i in 4:length(frame)) 
    {
      boxplot(frameol2[,i] ~ frameol2$grp2, main=sprintf("Boxplot of %s", names(frameol2)[i]), xaxt="n")
      axis(side=1)
    }

    # ANOVA test
    for(i in 4:length(frameol2) )  {
      for (j in 1:2) {        
        p <- anova( lm(frameol2[,i] ~ factor(frameol2[,j])) )$"Pr(>F)"[1] 
        nomes <- names(frameol2)
        print(sprintf("The p-value of %s with regard to %s is %.3f", nomes[j], nomes[i], p))
      }    
    }
    rm(i, j, p, nomes)

  boxplot(frameol2$tfs ~ frameol2$grp2)
  boxplot(frameol2$tfs ~ frameol2$grp3)

  boxplot(frameol2$mgo ~ frameol2$grp2)
  boxplot(frameol2$mgo ~ frameol2$grp3)

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

  ## analysis of tfs and mgo
    ## mgo
mgonao <- frameol2$mgo[frameol2$grp3 == 0]
mgosim <- frameol2$mgo[frameol2$grp3 == 1]
teste.mgo <- t.test(x=mgonao, y=mgosim)
    ## tfs
tfsnao <- frameol2$tfs[frameol2$grp3 == 0]
tfssim <- frameol2$tfs[frameol2$grp3 == 1]
teste.tfs <- t.test(x=tfsnao, y=tfssim)

## make two histograms of temperatures
old.par <- par()

  ############################################################################
  ### CHART ONE
  par(old.par)
  plot(mild$pbs ~jitter(as.numeric(mild$NEFE_REAL > 0), amount=.05), pch=19, 
       col=(mild$NEFE_REAL > 0)+2, xlim=c(-0.25, 1.25),
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
  hist(mild$TFS[mild$NEFE_REAL> 0], nclass=15, col="red",  xlim=c(1620, 1720), ylim=c(0, 20), main=NA, xaxt="n", ylab=NA, xlab=NA, ann=FALSE)
  par( mar=c(4, 0, 0, 0))
  hist(mild$TFS[mild$NEFE_REAL==0], nclass=15, col="blue", xlim=c(1620, 1720), ylim=c(20, 0), main=NA,           ylab=NA, xlab="Temperatura, ºC")
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
  

  anova.pfs2 <- anova( lm(frameol2$pfs ~ frameol2$grp2) )
  anova.pfs3 <- anova( lm(frameol2$pfs ~ frameol2$grp3) )