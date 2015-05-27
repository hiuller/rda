rm(list=ls())
setwd("C:/Users/Public/Documents/RDataAnalysis/10_cas_family")

raw <- read.csv("./fosforo.csv", sep=';')
h<-hist(raw$TEMP_INTER_SUBLANCA_LD, plot=F)
bins <- h$breaks
rm(h)
#são 12 quebras, quero classificar as observacoes em cada quebra para depois contar
n<-dim(raw)[1]
bins <- c(bins, 1e5) #adicionar um valor elevado para representar > Li & < Infinity
bin <- list();
for(i in 1:n)
{
  for(j in 1:12)
  {
    if(raw$TEMP_INTER_SUBLANCA_LD[i] >= bins[j] & raw$TEMP_INTER_SUBLANCA_LD[i] < bins[j+1])
    {
      bin[[i]] <- j
    }
  }
}
rm(i, j)
bin <- as.numeric(bin)
raw$bin <- bin
# aqui temos uma visao dos agrupamentos.
table(raw$bin,raw$fam_TA)
# podemos escolher os bins 3, 5 e 7 e programar um bootstrap para fazer a análise DOE
filtro <- subset(raw, subset=raw$bin %in% c(3, 5, 7))
table(filtro$bin,filtro$fam_TA)
filtro$fator <- paste(filtro$bin, filtro$fam_TA)
table(filtro$fator)
fatores<-names(table(filtro$fator))
nf<-length(fatores)
replicas<-10
# quero compor amostras de tamanho 10 que vou tratar como réplicas para estudar no MINITAB

#############################################################################################
    c1<-vector(); c2<-vector(); c3<-vector()
    lb<-vector()
    set.seed(23061983)
    for(i in 1:nf)
    {
      c1<- c(c1, sample(x=filtro$P[filtro$fator == fatores[i]], size=replicas))
      c2<- c(c2, sample(x=filtro$P[filtro$fator == fatores[i]], size=replicas))
      c3<- c(c3, sample(x=filtro$P[filtro$fator == fatores[i]], size=replicas))
      lb <- c(lb, rep(x=fatores[i], times=replicas))
    }

    saida <- data.frame(lb, c1, c2, c3)
    #write.table(saida, sep=';', file='saida_doe.csv')
    
    #determinar o ponto médio dos bins
    temp<-vector()
    for(i in c(3,5,7))
    {
      temp<-c(temp, bins[i]+(bins[i+1]-bins[i])/2)
    }
    temp2<-c(0, 0, temp[1], 0, temp[2], 0, temp[3])
    rm(i)
    ta<-c(0,2,8)
    rot<-c("min", "media", "max")
    
    temperatura<-vector()
    sucata<-vector()
    for(i in 1:length(lb))
    {
      str<-strsplit(lb[[i]], split=' ')
      temperatura[[i]]<-temp2[ as.numeric(str[[1]][1])]
      sucata[[i]]<-ta[ rot==str[[1]][2]]
    }
    saida$temperatura<-temperatura
    saida$sucata<-sucata
    rm(i, lb, rot, replicas, n, nf, temperatura, sucata, str, bin, bins, fatores, s, temp2)
    
    levels<-3
    temp_effect<-vector(); ta_effect<-vector()
    for(i in 1:levels)
    {
      temp_effect[[i]]<-mean(saida$s[saida$temperatura == temp[i]])
      ta_effect[[i]]  <-mean(saida$s[saida$sucata      == ta  [i]])
    }
    rm(i, levels)

##########################################################################################

# plot the mais effects
mainEffect <- function(effects, name, labels)
{
  plot(effects~c(1:3), pch=19, col='blue', cex=1.2, xlim=c(0.5,3.5), xaxt='n', xlab=NA, ylab=NA)
  axis(side=1, at=c(1:3), labels=labels)
  lines(effects~c(1:3))
  abline(h=mean(saida$s), col='grey')
  title(paste('Main effects plot for ', name))
}

par(mfrow=c(1,2))
mainEffect(temp_effect, 'Temp', temp)
mainEffect(ta_effect, 'TA', ta)
par(mfrow=c(1,1))

