
rm(list=ls())
old.par <- par()

#setwd("J:/RDataAnalysis/01_ULC_Deoxidation")
setwd("C:/Users/Public/Documents/RDataAnalysis/01_ULC_Deoxidation")

# after the preprossecing.r
data <- read.csv("./data/data_wo_ol.csv")

# this code is for tabulation purposes
data$cod <- paste(as.integer(data$N_BLOW>0), as.integer(data$CANR_APOS_DESOX>0))
table(data$LIGA, data$cod)

# this bollean tells weather it as pre-deoxidized or not (don't forget the space at the end...)
data$PRED = (data$LIGA == "SIBR ")

# this Ind. var. tells weather it was a good decision or not
for(i in 1:5386){
  if (data$cod[i] == "0 0"){
    data$RESULT[i] <- TRUE 
  } else  {
    if (data$cod[i] == "1 1"){
      data$RESULT[i] <- FALSE
    } else {
      
      if(data$PRED[i]==TRUE) {
        if(data$cod[i] == "1 0") {
          data$RESULT[i] <- FALSE
        } else  {
          data$RESULT[i] <- TRUE
        }
      } else {
        if(data$cod[i] == "1 0") {
          data$RESULT[i] <- TRUE
        } else  {
          data$RESULT[i] <- FALSE
        }        
      }
    }
    
  }
}
rm(i)

table(data$LIGA)
table(data$RESULT)

  
# using the layout() approach to do the same sort of chart
  pdf("latex/figures/fig01.pdf", height=3.9, width=6)
  par(mfcol=c(1,1), mai=rep(0.5, 4), oma=c(1,1,1,1), mar=c(4,3,2,1), xpd=NA) #c(bottom, left, top, right)
  layout(rbind(c(1, 2), c(3, 3)), heights=c(6, 1))
  plot(data$TEMPA[data$RESULT==T]~data$OPPMA[data$RESULT==T], col=as.integer(data$PRED[data$RESULT==T])+3, 
       pch=19, main="(a)", xlab="[O], ppm", ylab="T, ºC")
  plot(data$TEMPA[data$RESULT==F]~data$OPPMA[data$RESULT==F], col=as.integer(data$PRED[data$RESULT==T])+3, 
       pch=19, main="(b)", xlab="[O], ppm", ylab="")
  #title("Last Celox measurement before deoxidation", outer=TRUE) 
  par(mai=c(0,0,0,0))
  plot.new()
  legend("center", cex=1, title="Desoxidante:", legend=c("FeSi", "Al"), pch=19, col=as.integer(c(T,F))+3, bty="n", horiz=TRUE)
  layout(1)
  par(old.par)
  dev.off()

# the binary logistic regression model
  trainning <- subset(data, data$RESULT==TRUE)
  lr.model <- glm(PRED ~ TEMPA + OPPMA + T_LIBER, data=trainning, family=binomial("logit"))
  summary(lr.model)
  
  
  # output this plot to use in LateX
  # http://stackoverflow.com/questions/1890215/getting-r-plots-into-latex
  par(old.par)
  par(mar=c(2,4,0,2)+0.1)
  pdf("latex/figures/fig02.pdf", height=6, width=6)
  plot(lr.model$fitted.values, col=as.integer(trainning$PRED)+3, pch=19,
       xlab="Observação", ylab="Chance")
  #title("Binary logistic regression on the use of FeSi")
  legend(x=20, y=0.9, pch=19, col=3:4, legend=c("Al","FeSi"), bg="white")
  dev.off()

  
  # Figure 04, T liberaçao
  par(old.par)
  par(mar=c(2,4,0,2)+0.1, mgp=c(0,1,0))
  pdf("latex/figures/fig04.pdf", height=6, width=6)
  plot(trainning$T_LIBER, pch=19,
       xlab="Observação", ylab="T liberaçao, ºC")   
  dev.off()

  # there is a threshold value between 0 and 1 for which one should use FeSi
  # when the odds ratio is greather than this threshold value. it could be
  # equal 0.5 and this will yield to a certain error rate. I need to figure out
  # the best error measurement and then optimize the threshold value.

  # let's use the precision/recall methodology
  # this function computes the Fstatistic based on the value of the threshold (L)
  fstat <- function(actual, predicted)    
  {
    tp <- sum(as.integer(predicted  & actual))
    fp <- sum(as.integer(predicted  & !actual))
    fn <- sum(as.integer(!predicted & actual))
    
    precision <- tp/(tp+fp); 
    recall    <- tp/(tp+fn); 
    
    return(2*((precision*recall)/(precision+recall)))
  }

  pred <- function(odds, L)
  {
    return(odds > L)
  }
  actual <- as.vector(trainning$PRED)

# this procedure finds the threshold value for the maximum Fstat
  index <- seq(from=0.1, to=1.0, by=0.005)
  f <- vector(mode="numeric", length=length(index))
  fs <- function(L){fstat(actual, pred(lr.model$fitted.values, L))}
  for(i in 1:length(index)) f[i] <- fs(index[i])
    opt <- optimize(f=fs, interval=c(0,1), maximum=TRUE)

      par(old.par)
      par(mar=c(2,4,0,2)+0.1)
      pdf("latex/figures/fig03.pdf", height=6, width=6)
      plot(f ~ index, pch=19, col="darkgrey", cex=1.4, ylim=c(0, 1.0), ylab="Valor-F", xlab="Valor limiar, pl")
      abline(v=opt$maximum)
      abline(h=opt$objective)
      mtext(text="0.3627243", side=3, outer=FALSE, at=0.35)
      dev.off()

  # make predictions based on the optimum value for the threshold
  predicted <- pred(lr.model$fitted.values, opt$maximum)

  # construct a truth table for appraisal of the model:
  tp <- sum(as.integer( predicted &  actual))
  fp <- sum(as.integer( predicted & !actual))
  fn <- sum(as.integer(!predicted &  actual))
  tn <- sum(as.integer(!predicted & !actual))
  result = rbind(c(tp, fp), c(fn, tn))
  print(result)
  sum(result) == dim(trainning)[1] # this should resolve true to ensure the calculation is done right

  hitting_rate = (tp + tn)/sum(result)
  print(hitting_rate)

  source("removeol.r")
  normp(trainning$T_LIBER)

  # this curve is to be presented along with the repport
  func <- function(Oppm, TLib, modelo){    
    pl <- 0.3627243
    return((log(pl/(1-pl)) - modelo$coefficients[1] - modelo$coefficients[3]*Oppm - modelo$coefficients[4]*TLib) / modelo$coefficients[2])
  }

  curve(expr=func(x, 1560, lr.model), from=200, to=1200, xlab="[O], ppm", ylab="T, ºC")
  curve(expr=func(x, 1570, lr.model), from=200, to=1200, xlab="[O], ppm", ylab="T, ºC", add=TRUE)
  curve(expr=func(x, 1580, lr.model), from=200, to=1200, xlab="[O], ppm", ylab="T, ºC", add=TRUE)
  curve(expr=func(x, 1590, lr.model), from=200, to=1200, xlab="[O], ppm", ylab="T, ºC", add=TRUE)
  curve(expr=func(x, 1600, lr.model), from=200, to=1200, xlab="[O], ppm", ylab="T, ºC", add=TRUE)
  curve(expr=func(x, 1610, lr.model), from=200, to=1200, xlab="[O], ppm", ylab="T, ºC", add=TRUE)

  # making the contour plot
  par(old.par)
  par(mar=c(2,4,0,2)+0.1)
  pdf("latex/figures/fig05.pdf", height=6, width=6)
# http://acad.depauw.edu/harvey_web/Chem%20351/PDF%20Files/Handouts/RDocs/Creating%20Surface%20and%20Contour%20Plots%20in%20R.pdf
  x <- seq(from=200,  to=1200, by=100) #oppm
  y <- seq(from=1540, to=1610, by=30)  #tcelox
  lev <- c(1570, 1580, 1590)
  myfunc <- function(a, b) func(Oppm=a, TLib=b, model=lr.model)
  z <- outer(x, y, myfunc)

  z2 <- cbind(rev(z[,1]), rev(z[,2]), rev(z[,3]))
  # http://www.bretttolbert.com/projects/colorscale/
  mycolors = c('#3d22e5', '#2a7ee5', '#33e5dd', '#3be479', '#68e444', '#c8e44c', '#e3a955', '#e35d5d')
  myLab = c("Tl=1570ºC", "Tl=1580ºC", "Tl=1590°C")
  contour (x, y, z2, levels=lev, xlab="[O], ppm", ylab="T, ºC", lwd=2, col="blue", labcex=1.1, labels=myLab)
    # those are the actual points
    # I believe this is wrong
    # for(j in 1:length(y)) {   for(i in 1:length(x)) { points(x[i], z[i,j], pch=19, col="red")} }
  dev.off()