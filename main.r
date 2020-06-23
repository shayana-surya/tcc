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


matrizDistancia <- createEuclideanDistance(diffCemigData)

centroides <- diffCemigData$ID
TestEstatistic <- EstatisticTestElementsCalculator(centroides,diffCemigData)

clustersRaio <- geradorClusterPorRaio(TestEstatistic,FALSE,matrizDistancia,diffCemigData,1000)

#k <- 3
#resultados <- data.frame()
#pos <- c(rep(TRUE,k),rep(FALSE,(nrow(diffCemigData)-k)))
#for (i in 1:k)
#{
#resultados[i,] <- diffCemigData$ID[pos]
#
#}
#resultados

clusters <- geradorCluster(TestEstatistic,FALSE,matrizDistancia,diffCemigData,k)
resultSimul <- monteCarloSimu(TestEstatistic,matrizDistancia,diffCemigData,k)
significativos <- clustersSignificativos(resultSimul,clusters,k)

histMatrixSimul <- hist(resultSimul)
histMatrixDist <- hist(matrizDistancia$distance)




