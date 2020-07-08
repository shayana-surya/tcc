### ---
### title: "tcc"
### author: "Shayana e Rafael"
### date: "6 de novembro de 2019"
### ---

#include <Rcpp.h>
#include <functions.r>
library(readxl)
library(data.table)
library(REAT)
library(Rcpp)
library(rlist)
source(file="functions.r")


dados2018 <- read_excel("DadosAlimentadoresCEMIG_XY.xlsx", sheet = 2)
dados2013 <- read_excel("DadosAlimentadoresCEMIG_XY.xlsx", sheet = 1)

joinDados <- merge(x = dados2013, y = dados2018, by="alimentador", all=TRUE)

##exemplo de utilizacao do .Rcpp
##Define o endereco do arquivo Cpp
#setwd("D:\\Documentos\\tcc")
##Compila o codigo em C++
#sourceCpp('exemplo.cpp')
##Inova a funcao compilada
#timesTwo(42)
#EDA - Exploratory Data Analysis


list_data <- createData(joinDados)
diffCemigData <- data.frame(list_data[1])
diffCemigData$ID <- 1:nrow(diffCemigData)
excluidas <- data.frame(list_data[2])


bound <- 1000
raio <- 1000

ti <- proc.time()
matrizDistancia <- createEuclideanDistance(diffCemigData)
proc.time() - ti

TestEstatistic <- EstatisticTestElementsCalculator(diffCemigData)

ti <- proc.time()
clustersRaio <- geradorClusterPorRaio(TestEstatistic,matrizDistancia,diffCemigData,raio)
proc.time() - ti

# Ainda n ta funcionando
#ti <- proc.time()
#resultSimulRaio <- monteCarloSimuRaio(TestEstatistic, diffCemigData, clustersRaio, raio, bound)
#proc.time() - ti
#print(resultSimulRaio)

# ti <- proc.time()
# significativosRaio <- clustersSignificativosRaio(resultSimulRaio,clustersRaio,raio)
# proc.time() - ti

k <- 250


#para k = 30 levamos 155,88 segundos
#com a remocao do segundo for e inclusao do rbind para alimentar resultados, o tempo caiu para 115,22 segundos
#para k igual a 250 levou-se 125,68 segundos
ti <- proc.time()
clusters <- geradorCluster(TestEstatistic, matrizDistancia, diffCemigData, k)
proc.time() - ti

#200 simulacoes com k=10 levaram 28.694,02 segundos. Quase 8hrs
# 66,77 segundos para uma simulacao de 100 iteracoes com k igual a 30. Isso apos as alteracoes
# 1000 simulações para k igual a 250 levou 3.601,34 segundos ou 1hr
ti <- proc.time()
resultSimul <- monteCarloSimu(TestEstatistic, diffCemigData, clusters, k, bound)
proc.time() - ti

ti <- proc.time()
significativos <- clustersSignificativos(resultSimul,clusters,k)
proc.time() - ti

histMatrixSimul <- hist(resultSimul)

