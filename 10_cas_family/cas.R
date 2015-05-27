# Nome dos datasets
# =================
#   raw -- dados brutos. size=5704
#   sub -- raw (-) familias com baixa frequencia. size=5325
# * clean -- sub (-) outliers de todas as familias. size=5174
#   small -- clean com menos atributos para fazer teste de hipótese. size=5174
#   comb -- pivot table feito com small. size=420
#   unico -- comb (-) duplas repedidas. e.g: A/B == B/A. size=210
#   sample -- unico com apenas as colunas 'a' e 'b'. size=
#   sample.melt -- sample aplicado à funçao melt. size=

rm(list=ls())
setwd("C:/Users/Public/Documents/RDataAnalysis/10_cas_family")
raw <- read.csv("./03-.csv", sep=';')

#create a family indicator
raw$family <- paste(raw$GRAU, raw$TRAT, raw$S_MAX, sep='-')

#look at the counts
tbl <- table(raw$family)

#wanna a subset of raw with only familie with count greather than 50
freq <- as.data.frame(tbl)$Freq
names <- names(tbl)
keep <- names[freq > 50]

#subset
sub <- subset(raw, subset=(raw$family %in% keep) )

#remove temp variables
rm(freq, names, keep, tbl)

#use one of my scripts
source("c:/users/public/documents/rdataanalysis/rscripts/removeol.r")

#remove all outliers from each group (wrote this function today)
boxplot(sub$AL_CAS ~ sub$family, main="antes da remocao de OLs")
clean <- multilevelrmol(sub, 5, 9)
boxplot(clean$AL_CAS ~ clean$family, main="depois da remocao de OLs")
rm(multilevelrmol, ol, removeol)
rm(sub)
#apply the k=means
# write.table(clean, file="./result.csv", sep=';')

#cluster analysis
small <- subset(clean, select=names(clean) %in% c('AL_CAS', 'GRAU', 'TRAT', 'S_MAX', 'family'))

#o número máximo de combinações é 3*2*9=54
#apareecram no banco de dados:21 famílias. se eu plotar um intervalo de confiança para todas
#posso saber quais são diferentes e quais são iguais...

#criar uma matriz com o produto cartesiano das familias pelas familias
#21*21=441
#441-21(repetiçoes)=420
fam <- names(table(clean$family))
comb <- expand.grid(fam, fam)
same <- comb$Var1 == comb$Var2
comb <- subset(comb, subset=!same)
rm(same)
# realizar um teste t para cada um dos pares, lembrando que ainda temos que remover a metade porque X com
#Y é a mesma coisa que Y com X
pvs <- list();
for(i in 1:length(comb$Var1))
{  
  pvs[i] <- t.test(
    x=clean$AL_CAS[clean$family==as.character(comb$Var1[i])], 
    y=clean$AL_CAS[clean$family==as.character(comb$Var2[i])]
    )$p.value
}
rm(i)
comb$pvs <- as.numeric(pvs)
pvs <- as.numeric(pvs)
plot(pvs, pch=19, col=(pvs>0.05)+1, cex=1.2)
abline(h=0.05, col='blue')
title("p-values: vermelhos iguais, pretos diferentes")
rm(pvs)

#para remover as duplas iguais vamos usar a propriedade da ordem dos produtos
#atribuindo um produto a cada linha
nc<-dim(comb)[1]
a <- numeric(nc)
b <- numeric(nc)
for(i in 1:21)
{
  a <- a + i*as.numeric(comb$Var1 == fam[i])
  b <- b + i*as.numeric(comb$Var2 == fam[i])
}
comb$a <- a
comb$b <- b
rm(i, a, b)

label <- list()
for(i in 1:nc)
{  
  ordenado<-sort(c(comb$a[i], comb$b[i]))
  label[i] <- paste(ordenado[1], ordenado[2], sep='/')  
}
comb$label <- as.character(label)
rm(i, ordenado, label)
# nao funcionou porque 60=20*3=15*4=12*5=10*6
#comb$produto <-NULL
#vars <- subset(comb, select=c("Var1", "Var2"))
#vars <- unique(vars)


comb$a <- NULL
comb$b <- NULL
comb$Var1 <- NULL
comb$Var2 <- NULL

unico <- unique(comb)

#retornar com os rotulos que foram removidos
a <- list()
b <- list()
n <- dim(unico)[1]
#   ia<-list();ib<-list()
for(i in 1:n)
{
  str <- unico$label[i]
  #strsplit(unico$label, '/')
  ## aqui estou perdendo uma familia
  indices <- as.numeric(strsplit(str, '/')[[1]] )
  familias <- fam[indices]
#     ia[i]<-indices[1]; ib[i]<-indices[2]
  a[i]<-familias[1]
  b[i]<-familias[2]
}
rm(i, n, str, indices, familias)  ## ao invés de 21, a e b só tem tamanho 20. nao descubro o problema
#   rm(ia, ib)

unico$a <- as.character(a)
unico$b <- as.character(b)
rm(a, b)
unico$iguais <- (unico$pvs > 0.05)

#View(unico)
rm(comb, small)

# Aqui ainda tenho 21 familias
length(table(c(unico$a, unico$b)))

#exportar os dados para usar no Excel
#####################################
#          write.table(unico, file="./unique.csv", sep=';')

#tentar fazer um dendograma com as medidas de proximidade
# preciso fazer uma tabela pivot com colunas a, linhas b e valores p (matriz triangular)
sample <- subset(unico, select=c('a', 'b', 'pvs'))
library(reshape2)
#vou normalizar os p-valores para que o dendograma fique mais visivel
  newP <- sample$pvs
  newP <- ifelse(newP < 1e-4, 0.1, newP)
  normP <- (newP - min(newP)/(max(newP)-min(newP)))
  sample$normP <- 1/(normP+1)
  sample$pvs <- NULL
  rm(newP, normP)
sample.melt <- melt(sample)

#aqui ainda tenho 21 familias
length(table(c(sample.melt$a, sample.melt$b)))
# aqui deixei de ter 21 e passei a ter 20. será que o numero de combinaçoes é 210? 21C2=210. está correto

mdm <- acast(sample.melt, a~b, fun.aggregate=sum, drop=FALSE)

rm(sample, sample.melt, unico)



mdm <- mdm + t(mdm)
rdm <- as.dist(mdm)
hc <- hclust(rdm, method="single")
plot(hc)
# desenha um retângulo ao redor dos k clusters e armazena os rótulos
kc=6 # number of clusters
x<-rect.hclust(hc, k=kc)

rm(rdm, mdm)

# colocar os rótulos no data.frame <clean>
cluster <- list()
for(i in 1:length(fam))
{
  for(j in 1:kc)
  {
    if( sum( as.numeric( fam[i] == names( x[[j]]))) > 0)
      cluster[i] <- j
  }
}
cluster <- as.numeric(cluster)
rm(i, j, kc )

# Percorrer o data.frame clean e descobrindo qual o cluster de cada famíia 
n<-dim(clean)[1]
lcl<-list()
for(i in 1:n)
{
  lcl[i]<-cluster[which(clean$family[i] == fam, T)]
}
clean$cluster <- as.numeric(lcl)
rm(i, lcl, n, nc, x)

#fazer uma anova entre os clusteres
lm<-lm(clean$AL_CAS ~ clean$cluster)
anova.obj<-anova(lm)
anova.obj

# o p-value foi zero. Assim, vamos fazer um interval plot, igual aquela

# essa funçao retorna o E para construçao de intervalo de confiança usando a distribuiçao t de student
# autor: eu em 25/05/2015 às 13h45
margin <-function(x, conf=0.95)
{
  n <- length(x)
  t_half_alpha <- qt(p=(conf+(1-conf)/2), df=n-1)
  return(t_half_alpha*sd(x)/sqrt(n))
}

# fazer um teste
al_cas <- list()
margens <- list()
medias <- list()
inferior <- list()
superior <- list()
for(i in 1:6)
{
  al_cas[[i]] <- as.numeric( subset( clean, subset=clean$cluster==i, select='AL_CAS')$AL_CAS)
  margens[[i]] <- margin( al_cas[[i]] )
  medias[[i]] <- mean( al_cas[[i]])
  inferior[[i]] <- medias[[i]]-margens[[i]]
  superior[[i]] <- medias[[i]]+margens[[i]]
}
rm(i)


# saida grafica do Minitab
library(plotrix)
plotCI(x=1:6, y=as.numeric(medias), ui=as.numeric(superior), li=as.numeric(inferior)) #example of interval plot
rm(medias, margens, inferior, superior)


