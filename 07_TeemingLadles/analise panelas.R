rm(list=ls())
#setwd("C:/Users/Public/usina")
setwd("C:/Users/Public/Documents/RDataAnalysis/07_TeemingLadles")

# old dataset
#dados <- read.csv('dados.csv')
#names(dados)[1] <- 'row'
dados <- read.csv('export.csv')



  # dados now contains: "PANELA" "CAMP" "VIDA_PAN_ACO" "DR" 
  # nl <- length(table(dados$PANELA)) ## the count of distinct ladles
  # maxlife <- max(dados$VIDA_PAN_ACO) ## maior vida de uma panela de aco
  lista <- split(dados, f=dados$PANELA)

#   I want to make a chart where I plot
#   each ladle on the y axis (column ID)
#   on the x axis goes the life (ignore NAs)
#   and we color a DR heat and leave a common heat blank


# a list with the labels
num <- length(lista)
labs <- names(lista)

  # the grouping
  camp <- c() # vector(mode="list", length=num) ## this is a factor   
  for( i in 1:num ){
    camp <- c(camp, lista[[i]] [1,2])
  }
  rm(i)  

     
    

    ## make a dataset of the number of heats between DR for each ladle
    result <- vector(mode="list", length=num)
      for(l in 1:num)
      {
        ladle <- lista[[l]]$DR
        life <- length(ladle)
        # nhbdr stands for number of heats between DRs
        nhbdr <- c()
        lastdr <- 0
        for(i in 1:life){
          if(ladle[i] == 1)
          {
            nhbdr <- c(nhbdr, i-lastdr)
            lastdr <- i
          }
        }
        result[[l]] <- nhbdr    
      }
      rm(l, i, ladle, lastdr, life, nhbdr)

    # I saw that result[[21]] is NULL, let's see why. Also 77, 75, 64, 
    nulls <- c();
    for(i in 1:length(result)){
      if(is.null(result[[i]])) nulls <- c(nulls, i)
    }
    rm(i)
  
## removing null from labels list and from lista list
  result[nulls] <- NULL
  lista [nulls] <- NULL
  camp <- camp[-nulls]
  labs <- labs[-nulls]
  rm(nulls)

## now it's time to define a methodology to carry out comparissons between data
## should we bind all data from different periods?

meses <- unique(camp)
mensal  <- vector(mode="list", length=length(meses))
valores <- vector(mode="list", length=length(meses))
rows <- 1:length(camp)
tamanhos <- c()
rotulos <- c()

for(i in 1:length(meses)){  
  mensal [[i]] <- rows[ camp == meses[i] ]
  valores[[i]] <- unlist( result[ as.numeric(mensal[[i]] ) ] )
  tamanhos <- c(tamanhos, length(valores[[i]]) )
  rotulos <- c(rotulos, rep(x=meses[i], times=tamanhos[i]))
}
rm(i)

df <- data.frame(rotulos, unlist(valores))


names(df) <- c('mes', 'intervalo')
per <- c('jan', 'fev', 'mar', 'abr', 'mai', 'jun', 'jul', 'ago')

fit <- aov(intervalo~mes, data=df) 
fit2 <- oneway.test(df$intervalo~df$mes)
summary(fit)

    ## mixed chart boxplot and jittered plot
    plot(intervalo~jitter(mes, amount=0.2), data=df, pch=19, col='red', cex=1.2, xaxt='n', xlab='Período')
    boxplot(intervalo~mes, data=df, add=T, col=NA, xaxt='n', yaxt='n', at=meses)
    axis(side=1, at=meses, labels=per)
    title('Intervalo entre corridas DR')

plot(valores[[8]])
valores[[8]]

#     # charting the data
#     row <- 1
#     #plot(rep(row, life) ~ as.numeric(1:life), col=1, bg=isdr, pch=22, ylim=c(1, life), xlim=c(1, 140))
#     # ladle 1 to 85
#     plot.new()
#     plot.window(ylim=c(1, num), xlim=c(1, 140))
#     for(i in 1:num) {
#       row <- i
#       life <- sum(!is.na(dados[,row+1])) 
#       isdr <- (dados[,row+1] == 1)[1:life]
#       points(rep(row, life) ~ as.numeric(1:life), col=1, bg=isdr, pch=22)
#     }
#     axis(side=2, at=2:num, labels=labs )

