############################################################################
## initial setup
	rm(list=ls())
	setwd("C:/Users/Public/Documents/RDataAnalysis/03_Rendimento")
	old.par <- par()
	
## imports
	library(car)
	library(nortest) # normality testing
	source("C:/Users/Public/Documents/RDataAnalysis/01_ULC_Deoxidation/removeol.r")

## reading the data
	data <- read.csv("./data/aaca.csv")
	names(data)[20] <- "rend.new"

## utils
	periodos <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")

## exploratory charts
	pairs(~ rend.old    + cao.t + mgo.t,data=data2, pch=19, col="red",   cex=1.8)
	pairs(~ rend.new    + cao.t + mgo.t,data=data2, pch=19, col="blue",  cex=1.8)
	pairs(~ rend.cb.esc + cao.t + mgo.t,data=data2, pch=19, col="green", cex=1.8)

## data transformation
	data2$camg.t <- data2$cao.t + data2$mgo.t # this is the total flux mass

############################################################################
## LINEAR MODELS
############################################################################
## PRIMAL MODEL

	primal   <-lm(data2$rend.old ~ data2$sagp.t + data2$sint.t + data2$pSuc + data2$ligas.t + data2$tacb.t + data2$esc.t + data2$cao.t + data2$mgo.t)
	primal   <-lm(data2$rend.old ~ data2$sagp.t + data2$sint.t              + data2$ligas.t + data2$tacb.t + data2$esc.t + data2$cao.t + data2$mgo.t)
	summary(primal)	
	ad.test(primal$residuals)
	
	dual <- step(primal) # this dual is the AIC step model of the first

	
	
	
	
	
	
	
	
	jan.14 <- c(1, 40.73,	1.847,	11.1890646,	26.85,	18.63295393, 0.8959)
	names(jan.14) <- c("one",  "sagp.t",	"sint.t",	"ligas.t",	"tacb.t",	"esc.t", "rend")
	rend.hat.jan <- t(primal$coefficients)%*%jan.14[1:6]
	rend.jan <- jan.14[7]
	resid.jan <- rend.jan - rend.hat.jan

	# goodness of the last linear model 
	plot(data2$rend.old ~ primal$fitted.values, pch=19, col="blue", cex=1.2, xlim=c(0.88, 0.95), ylim=c(0.88, 0.95))
	#abline(reg=primal$coefficients[1], col="darkgrey")
	abline(a=0, b=1, col="red", lwd=1)
	text(x=0.813, y=0.855, label=paste("Intercepto=", round(primal$coefficients[1],4)))
	title(paste("Rendimento x previsto modelo 3: r^2=", round(cor(data2$rend.old, primal$fitted.values)^2,2)))
	points(rend.jan, rend.hat.jan, col="red", pch=19, cex=1.5)
	dev.off()

	#residuals  
	plot(primal$residuals, pch=19, type="b", col="blue", cex=1.1, xlim=c(0,13))
	abline(a=0, b=0, col="grey", lwd=1, lty="dashed", cex=2)
	title("Residuals of primal")
	points(x=13, y=resid.jan, col="red", pch=19, cex=1.5)
	text(x=13.0, y=-0.0038, labels="jan/14")
	legend("topright", inset=0.03, pch=19, lwd=c(2, NA), legend=c("2013", "2014"), col=c("blue","red"), bty="n", cex=1.3)

	locator(1)
	dev.off()


	aicNew <- step(primal)
	summary(aicNew)
	plot(data2$rend.old ~ aicNew$fitted.values, pch=19, col="blue", cex=1.2, xlim=c(0.88, 0.95), ylim=c(0.88, 0.95))
	points(data2$rend.old ~primal$fitted.values, pch=19, col="green", cex=1.2, xlim=c(0.88, 0.95), ylim=c(0.88, 0.95))
	abline(a=0, b=1, col="red", lwd=1)
	title("Comparisson between original and step model")
	dev.off()

	plot(data2$rend.old, type="b", pch=19, col="blue", cex=1.2, xaxt="n")
	axis(1, 1:12, labels=periodos)
	lines(primal$fitted.values, type="b", pch=17, col="red", cex=1.2, xaxt="n")
	lines(aicNew$fitted.values, type="b", pch=15, col="green", cex=1.2, xaxt="n")
	legend("topright", inset=0.05, pch=c(19,17,15), lwd=2, col=c("blue", "red", "green"), legend=c("Actual", "Original", "Step"), ncol=1, bty="n", cex=1.2)

	pdf("latex/figures/fig09.pdf", height=4, width=6)
	par(mfcol=c(1,1), mai=rep(0.5, 4), oma=c(0.5,0.5,0.5,0.5))#, xpd=NA) #c(bottom, left, top, right)
	par(mar=c(3.8,3.8,0,0)+0.1) # this is a no-title margin
	plot(data2$rend.old, type="b", pch=19, col="blue", xlab="2013", cex=1.2, xaxt="n", ylim=c(0.86, 0.95), ylab="Rendimento")
	axis(1, 1:12, labels=periodos)
	points(data2$rend.new, type="b", pch=17, col="red", cex=1.2)
	points(data2$rend.cb.esc, type="b", pch=15, col="green", cex=1.2)
	legend("topright", legend=c("(a)", "(b)", "(c)"), col=c("blue", "red", "green"), lty=1, lwd=2, ncol=1, bty="n", cex=1.1, inset=0.05, pch=c(19, 17, 15))
	dev.off()

	# goodness of the last linear model 
	plot(data2$rend.old ~ aicNew$fitted.values, pch=19, col="blue", cex=1.2, xlim=c(0.86, 0.915), ylim=c(0.86, 0.96))
	abline(h=aicNew$coefficients[1], col="darkgrey")
	abline(a=0, b=1, col="red", lwd=1)
	text(x=0.813, y=0.855, label=paste("Intercepto=", round(aicNew$coefficients[1],4)))
	title("Rendimento sem CB x previsto modelo (sinter e ligas)")
	dev.off()

	plot(data2$rend.new ~data2$ligas.t, pch=19, col="red", cex=1.1)
	abline(coef=lm(data2$rend.new ~data2$ligas.t)$coefficients, pch=2)
	title("Rendimento sem CB e ligas (kg/t")
	plot(data2$rend.new ~data2$sint.t,  pch=19, col="red", cex=1.1)
	abline(coef=lm(data2$rend.new ~data2$sint.t)$coefficients, pch=2)
	title("Rendimento sem CB e sinter (kg/t")

	plot(data2$rend.old ~ data2$esc.t, pch=19, col="red", cex=1.2)
	mean(data2$rend.new)

	plot(aicFormula$residuals, pch=19, col="red", cex=1.2)

	normp(aicFormula$residuals)
	sum(aicFormula$residuals) ## a soma dos resÃ�duos Ã© zero




	######################################
	##  GrÃ¡ficos para relatÃ³rio
	plot(lm.dt2$fitted.values ~data2$rend, pch=19, cex=1.5, col="blue", ylab="Rendimento (modelo)", xlab="Rendimento (Real)")
	lines(x=c(0,1), y=c(0,1), lwd=3, col="red")
	text(x=data2$rend, y=lm.dt2$fitted.values, labels=periodos)

	pdf("latex/figures/fig01.pdf", height=6, width=6)
	par(mfcol=c(1,1), mai=rep(0.5, 4), oma=c(1,1,1,1), mar=c(5,4,2,2)+0.1)#, xpd=NA) #c(bottom, left, top, right)
	par(mar=c(5,4,2,2)+0.1) # this is a no-title margin
	plot(lm.dt2$fitted.values ~data2$rend.old, pch=19, cex=1.5, col="blue", ylab="Rendimento (modelo)", xlab="Rendimento (Real)")
	lines(x=c(0,1), y=c(0,1), lwd=3, col="red")
	# title("Modelo regressao mÃºltipla para o rendimento do convertedor")
	par(old.par)
	dev.off() # clear the graphical output

	# Carga sÃ³lida
	pdf("latex/figures/fig03.pdf", height=6, width=6)
	par(mfcol=c(1,1), mai=rep(0.5, 4), oma=c(1,1,1,1), mar=c(5,4,2,2)+0.1)#, xpd=NA) #c(bottom, left, top, right)
	par(mar=c(5,4,2,2)+0.1) # this is a no-title margin
	plot(data2$rend ~ data2$pSuc, pch=19, col="blue", ylab="Rendimento Real", xlab="Percentual de carga sÃ³lida", cex=1.5)
	abline(reg=lm(data2$rend~data2$pSuc), lwd=3, col="red")
	par(old.par)
	dev.off() # clear the graphical output

	# SINTER
	pdf("latex/figures/fig04.pdf", height=6, width=6)
	par(mfcol=c(1,1), mai=rep(0.5, 4), oma=c(1,1,1,1), mar=c(5,4,2,2)+0.1)#, xpd=NA) #c(bottom, left, top, right)
	par(mar=c(5,4,2,2)+0.1) # this is a no-title margin
	plot(data2$rend ~ data2$sint.t, pch=19, col="blue", ylab="Rendimento Real", xlab="Consumo de sÃ�nter (kg/t)", cex=1.5)
	abline(reg=lm(data2$rend~data2$sint.t), lwd=3, col="red")
	par(old.par)
	dev.off() # clear the graphical output

	# CB
	pdf("latex/figures/fig05.pdf", height=6, width=6)
	par(mfcol=c(1,1), mai=rep(0.5, 4), oma=c(1,1,1,1), mar=c(5,4,2,2)+0.1)#, xpd=NA) #c(bottom, left, top, right)
	par(mar=c(5,4,2,2)+0.1) # this is a no-title margin
	plot(data2$rend ~ data2$cb.t, pch=19, col="blue", ylab="Rendimento Real", xlab="Consumo de CB (kg/t)", cex=1.5)
	abline(reg=lm(data2$rend~data2$cb.t), lwd=3, col="red")
	par(old.par)
	dev.off() # clear the graphical output

	# AA
	pdf("latex/figures/fig06.pdf", height=6, width=6)
	par(mfcol=c(1,1), mai=rep(0.5, 4), oma=c(1,1,1,1), mar=c(5,4,2,2)+0.1)#, xpd=NA) #c(bottom, left, top, right)
	par(mar=c(5,4,2,2)+0.1) # this is a no-title margin
	plot(data2$rend ~ data2$pAA, pch=19, col="blue", ylab="Rendimento Real", xlab="Percentual de corridas AA", cex=1.5)
	abline(reg=lm(data2$rend~data2$pAA), lwd=3, col="red")
	par(old.par)
	dev.off() # clear the graphical output

	# SAGP
	pdf("latex/figures/fig07.pdf", height=6, width=6)
	par(mfcol=c(1,1), mai=rep(0.5, 4), oma=c(1,1,1,1), mar=c(5,4,2,2)+0.1)#, xpd=NA) #c(bottom, left, top, right)
	par(mar=c(5,4,2,2)+0.1) # this is a no-title margin
	plot(data2$rend ~ data2$sagp.t, pch=19, col="blue", ylab="Rendimento Real", xlab="Consumo de gusa sÃ³lido (kg/t)", cex=1.5)
	abline(reg=lm(data2$rend~data2$sagp.t), lwd=3, col="red")
	par(old.par)
	dev.off() # clear the graphical output

	# LIGAS
	pdf("latex/figures/fig10.pdf", height=6, width=6)
	par(mfcol=c(1,1), mai=rep(0.5, 4), oma=c(1,1,1,1), mar=c(5,4,2,2)+0.1)#, xpd=NA) #c(bottom, left, top, right)
	par(mar=c(5,4,2,2)+0.1) # this is a no-title margin
	plot(data2$rend.old ~ data2$ligas.t, pch=19, col="blue", ylab="Rendimento Real", xlab="Consumo de ferroligas (kg/t)", cex=1.5)
	abline(reg=lm(data2$rend.old ~data2$ligas.t), lwd=3, col="red")
	par(old.par)
	dev.off() # clear the graphical output

	######################################

	dev.off()

	########################### anÃ¡lise do rendimento de dezembro #####################
	rend <- c(0.9142, 0.9079, 0.9094, 0.9055, 0.8976, 0.8928, 0.9023, 0.9015, 0.9054, 0.8985, 0.8979, 0.8883)
	rend.dez <- rend[12]
	rend<-rend[1:11]
	## rend.new  Ã© normal

	normp(rend)
	library(car)
	myBoxCox <- function(var, lambda=seq(from=-2, to=2, by=0.1)) {
	i <- 1
	p <- vector(length=length(lambda))
	for(i in 1:length(lambda)) p[i] <- ad.test(bcPower(U=var, lambda=lambda[i]))$statistic
	plot(p ~ lambda)
	return(p)
	}

	xbar <- mean(rend)
	s    <- sd(rend)
	z <- (rend.dez - xbar)/s
	### prob. de observar um valor extremo
	pnorm(z)