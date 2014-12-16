# Análise dos dados de ressopro

O objetivo deste relatório é retratar a análise de dados conduzida para descobrir evidências que levem as causas do suposto aumento no índice de ressopro observado na Aciaria 2.

Foi preparada uma consulta em linguagem SQL para obtenção de dados. Os registros foram exportados para o formato **CSV** (*comma separated values*) para tratamento no software **R**.


```r
rm(list=ls())
setwd("C:/Users/Public/Documents/RDataAnalysis/08_reblow")
rawData <- read.csv("qry_ressopro_global.csv", sep=";")
names(rawData)
```

```
##  [1] "NUM_CORR_ACI"        "TEMPO_INIC_SOPRO_LD" "DT_PRODC_OBTD_ACI"  
##  [4] "GRAU"                "CD_SOP"              "CD_PDR_O2"          
##  [7] "TGUSA"               "MED_TGUSA"           "TP_CORR"            
## [10] "T_1INTER"            "T_2INTER"            "T_FS"               
## [13] "T_VAZAMENTO"         "VOL_RESS"            "PERC_SUCATA"
```

O número de observações é 1774. São 15 colunas com informações das corridas sendo que o campo chave é `NUM_CORR_ACI`.

## Análise do efeito do percentual de sucata

O percentual de sucata ao longo dos meses é apresentado a seguir. Inicialmente foi preciso criar uma coluna para agrupar os meses do ano de 2014. A maior data disponível é 2014-12-16 (no formato YYYY-MM-DD). `rawData` é um `data.frame` e vamos adicionar uma coluna chamada `mes` para conter apenas o número do mês corrente extraido da data (`DT_PRODC_OBTD_ACI`).


```r
rawData$mes <- format(as.Date(rawData$DT_PRODC_OBTD_ACI, format="%d/%m/%Y"), format="%m")
```

O ressopro foi considerado quando o volume de oxigênio soprado foi superior a 40 Nm^3 .


```r
rawData$ressopro <- (rawData$VOL_RESS > 40)
```

A taxa de ressopro mensal pôde, então, ser calculada:


```r
tapply(rawData$ressopro, rawData$mes, sum)/table(rawData$mes)
```

```
##     11     12 
## 0.1951 0.1643
```

Dentro dessas 1774 corridas, temos diferentes siglas (que possuem teores máximos admissíveis de fósforo diferentes) e diferentes percentuais de sucata. Queremos investigar se as taxas de ressopro obtidas em amostras estratificadas seriam consistentemente diferentes. Vamos criar um *bootstrap* a partir do *pool* de corridas em `rawData`. Realizei esta mesma análise no Excel e a conclusão obtida era a de que não havia diferença. Agora, vou criar uma função para fazer o *bootstrapping* automaticamente e conferir se as conclusões se sustentam. Depois, vamos umsar um banco de dados maior porque usando apenas 2 meses (como foi o caso da análise via Excel) estamos tirando o efeito 'aumento do ressopro' pois estamos analisando apenas corridas oriúndas do período de 'alto índice de ressopro'. 

Vamos dar uma olhada no número de corridas por família:

```r
j <- table(rawData$TP_CORR)
```

Em função da quantidade de observações, vamos compor as amostras somente com as famílias em que a contagem for superior a 100 corridas.


```r
incluir <- names(j)[j>100]
sub <- subset(rawData, subset=rawData$TP_CORR %in% incluir)
table(as.character(sub$TP_CORR))
```

```
## 
##        AA S >  6 Duplo refino CAS  Duplo refino LF              UBC 
##              513              139              241              646
```

Depois deste procedimento ficamos com um subconjunto dos dados originais chamado `sub` que contém 1539 corridas. O percentual de sucata (variável em estudo) para as corridas do conjunto `sub` é mostrado a seguir:


```r
sub$TP_CORR <- droplevels(sub$TP_CORR)
plot(sub$PERC_SUCATA ~ jitter(as.numeric(sub$TP_CORR)), pch=19, col='red', cex=1.5, xlab="Tipo de aço", ylab="% sucata", xaxt='n')
title("Percentual de sucata por tipo de aço")
axis(side=1, at=c(1, 2, 3, 4), labels=c("AA", "DR-CAS", "DR-FP", "UBC"))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

Os tipos de aço foram codificados em números usando `as.numeric(sub$TP_CORR)`. Para saber quem é quem foi usado:


```r
table(sub$TP_CORR, as.numeric(sub$TP_CORR))
```

```
##                   
##                      1   2   3   4
##   AA S >  6        513   0   0   0
##   Duplo refino CAS   0 139   0   0
##   Duplo refino LF    0   0 241   0
##   UBC                0   0   0 646
```

O gráfico da distribuição de sucata por tipo de aço mostra que existe uma concentração que cobre todas as famílias ao redor de 14% e 16%. O aço `AA` tem muitas corridas com mais de 23% e, neste caso, é possível fazer uma comparação onde o percentual de sucata tem um range maior entre os agrupamentos, perdendo apenas, a capacidade de avaliar o efeito do tipo de aço.

### Dividindo o conjunto de dados em grupos pelo percentual de sucata

Vamos percorrer o conjunto de dados com janelas de banda fixa (inicialmente 1%) e vamos procurar as bandas que absorvem grande número de corridas transversalmente (entre todas as famílias). Sabemos pelo *jittered plot* que vamos encontrar tais famílias entre 14 e 16% de sucata. O comprimento da banda é importante porque entre 14 e 16% temos 2% de diferença. Se a banda tiver esse mesmo tamanho estaremos mais propensos ao confundimento. Iremos usar, portanto, *non overlapping bands*.

O algorítmo para varredura está resumido nas seguintes tarefas:
* obter um valor médio inicial
* calcular o limite inferior como = valor inicial - (banda/2)
* calcular o limite superior como = valor inicial + (banda/2)
* contar quantos valores de cada família estao incluídos na banda
 

```r
banda <- 1.0  
i <- seq(from=13, to=20, by=0.1)
result <- c()
for(x in i){
  intervalo <- c(x-(banda/2), x+(banda/2))
  temp <- as.numeric(tapply(as.numeric(sub$PERC_SUCATA > intervalo[1] & sub$PERC_SUCATA < intervalo[2]), sub$TP_CORR, sum))
  result <- rbind(result, c(x, temp))
}
```

Para obter uma saída gráfica para as contagens, usamos:


```r
plot  (result[,2]~result[,1], pch=19, col="blue",   cex=1.5, ylab="Contagem", xlab="Meio faixa")
points(result[,3]~result[,1], pch=19, col="red",    cex=1.5)
points(result[,4]~result[,1], pch=19, col="green",  cex=1.5)
points(result[,5]~result[,1], pch=19, col="cyan",   cex=1.5)
title("Contagem de corridas por faixa de sucata")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 
 


### Programando o *bootrsapping*


Para o teste de diferença de proporção, usar: `prop.test`.
