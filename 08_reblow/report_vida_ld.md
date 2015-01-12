# Efeito da vida do convertedor no índice de ressopro

O objetivo deste estudo é investigar o efeito da vida do revestimento refratário no índice de ressopro. Existem outros fatores que se acredita podem afetar este índice e que, portanto, deveriam se levados em conta na fase do projeto da análise estatística.

Os possíveis confundidores, podem ser:
* Percentual de sucata utilizado;
* Teor de fósforo da sigla
* Temperatura e composição do gusa
* Peso total de fundentes utilizados no sopro

## Método proposto para estudo *preliminar*

Criar amostras *balanceadas* de determinadas siglas de aço com percentuais de sucata dentro de uma faixa de variação de 1%. As amostrar terão um tamanho fixo `n=30` para cada sigla e o fator em estudo (vida do convertedor) será utilizado nos níveis **baixo** (entre a corrida 100 e a corrida 1000) e **alto** (entre a corrida 2100 e 3000). Em suma, estamos bloqueando apenas o percentual de sucata como confundidor. Os outros possíveis confundidores estão sendo desprezados porque se acredita ser muito difícil obter um conjunto de dados grande o bastante com 2 níveis suficientemente separados para *todos* os fatores confundidores.

Manter as análises no mesmo convertedor ou podemos *misturar* convertedores e campanhas? Pode não ser possível manter as diferenças no mesmo vaso por motivos de disponibilidade de dados. Portanto, estamos decidindo por trtar indistintamente convertedores diferentes. 

Podemos conduzir um estudo *preliminar* usando apenas aços UBC que são mais abundantes e, portanto, vão proporcionar um abnco de dados mais homogênio. 

## Projeto do banco de dados

Para o estudo *preliminar* vamos precisar das seguintes informações
* Número do LD
* Número da campanha
* Vida do revestimento
* Nível do fator vida (alto ou baixo)
* Percentual de sucata da corrida


```r
rm(list=ls())
setwd("C:/Users/Public/Documents/RDataAnalysis/08_reblow")
#rawData <- read.csv("qry_ressopro_global.csv", sep=";")
```

## Como detectar a ocorrência de reação fim de sopro?

