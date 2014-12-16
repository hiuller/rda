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
