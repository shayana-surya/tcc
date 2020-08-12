### ---
### title: "tcc"
### author: "Shayana e Rafael"
### date: "6 de novembro de 2019"
### ---

library(readxl)
library(data.table)
library(REAT)
library(Rcpp)
library(rlist)
source(file="functions.r")
#Define o endereco do arquivo Cpp
#setwd("D:\\Programas\\TCC")
sourceCpp('exemplo.cpp')


dados2018 <- read_excel("DadosAlimentadoresCEMIG_XY.xlsx", sheet = 2)
dados2013 <- read_excel("DadosAlimentadoresCEMIG_XY.xlsx", sheet = 1)

joinDados <- merge(x = dados2013, y = dados2018, by="alimentador", all=TRUE)
list_data <- createData(joinDados)
diffCemigData <- data.frame(list_data[1])
excluidas <- data.frame(list_data[2])
relacaoAlimentadorId <- data.frame(list_data[3])


bound <- 1000
raio <- 200000
k <- 250


## USANDO DIST
## Tempo: 0.16 s
#ti <- proc.time()
#matrizDistancia <- createEuclideanDistance(diffCemigData)
#proc.time() - ti

### USANDO Rcpp
## Tempo: 0.04 s
ti <- proc.time()
matrizDistancia <- createEuclideanDistanceUsingRcpp(diffCemigData)
proc.time() - ti

### Matriz de Index
## Tempo: 0.38 s
#ti <- proc.time()
#IndexMatrix <- createIndexMatrix(matrizDistancia)
#proc.time() - ti


########################### UTILIZA RAIO #############################

# Tempo:  0,17s
ti <- proc.time()
clustersRaio <- geradorClusterPorRaio(matrizDistancia,diffCemigData,raio)
proc.time() - ti

# Raio = 200 km e 1000 Sim
# Tempo: 165,50 S
ti <- proc.time()
resultSimulRaio <- monteCarloSimuRaio(diffCemigData, clustersRaio, raio, bound)
proc.time() - ti
print(length(clustersRaio))

# Tempo: 0,11 S
 ti <- proc.time()
 significativosRaio <- clustersSignificativosRaio(resultSimulRaio,clustersRaio,raio)
 proc.time() - ti
 
 print(significativosRaio)

#histMatrixSimul <- hist(resultSimulRaio)


########################### UTILIZA K #############################
#
##para k = 30 levamos 155,88 segundos
##com a remocao do segundo for e inclusao do rbind para alimentar resultados, o tempo caiu para 115,22 segundos
##para k igual a 250 levou-se 125,68 segundos
#ti <- proc.time()
#clusters <- geradorCluster(TestEstatistic, matrizDistancia, diffCemigData, k)
#proc.time() - ti
#
##200 simulacoes com k=10 levaram 28.694,02 segundos. Quase 8hrs
## 66,77 segundos para uma simulacao de 100 iteracoes com k igual a 30. Isso apos as alteracoes
## 1000 simulações para k igual a 250 levou 3.601,34 segundos ou 1hr
#ti <- proc.time()
#resultSimul <- monteCarloSimu(TestEstatistic, diffCemigData, clusters, k, bound)
#proc.time() - ti
#
#ti <- proc.time()
#significativos <- clustersSignificativos(resultSimul,clusters,k)
#proc.time() - ti
#
#histMatrixSimul <- hist(resultSimul)
#
#