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
raio <- 1000

matrizDistancia <- createEuclideanDistance(diffCemigData)

#centroides <- diffCemigData$ID
TestEstatistic <- EstatisticTestElementsCalculator(diffCemigData)

clustersRaio <- geradorClusterPorRaio(TestEstatistic,FALSE,matrizDistancia,diffCemigData,raio)

#k <- 3
#resultados <- data.frame()
#pos <- c(rep(TRUE,k),rep(FALSE,(nrow(diffCemigData)-k)))
#for (i in 1:k)
#{
#resultados[i,] <- diffCemigData$ID[pos]
#
#}
#resultados

k <- 30
bound <- 100

#para k = 30 levamos 155,88 segundos
ti <- proc.time()
clusters <- geradorCluster(TestEstatistic,FALSE,matrizDistancia,diffCemigData,k)
proc.time() - ti

#200 simulacoes com k=10 levaram 28.694,02 segundos. Quase 8hrs
ti <- proc.time()
resultSimul <- monteCarloSimu(TestEstatistic,matrizDistancia,diffCemigData,k,bound)
proc.time() - ti

ti <- proc.time()
significativos <- clustersSignificativos(resultSimul,clusters,k)
proc.time() - ti

histMatrixSimul <- hist(resultSimul)
histMatrixDist <- hist(matrizDistancia$distance)




