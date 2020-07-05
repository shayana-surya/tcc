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

ti <- proc.time()
matrizDistancia <- createEuclideanDistance(diffCemigData)
proc.time() - ti

centroides <- diffCemigData$ID
TestEstatistic <- EstatisticTestElementsCalculator(centroides,diffCemigData)

#clustersRaio <- geradorClusterPorRaio(TestEstatistic,FALSE,matrizDistancia,diffCemigData,1000)

#k <- 3
#resultados <- data.frame()
#pos <- c(rep(TRUE,k),rep(FALSE,(nrow(diffCemigData)-k)))
#for (i in 1:k)
#{
#resultados[i,] <- diffCemigData$ID[pos]
#
#}
#resultados

k <- 250
bound <- 1000

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

