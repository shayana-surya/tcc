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

##exemplo de utilização do .Rcpp
##Define o endereco do arquivo Cpp
#setwd("D:\\Documentos\\tcc")
##Compila o codigo em C++
#sourceCpp('exemplo.cpp')
##Inova a funcao compilada
#timesTwo(42)


list_data <- createData(joinDados)
diffCemigData <- data.frame(list_data[1])
excluidas <- data.frame(list_data[2])


matrizDistancia <- createEuclideanDistance (diffCemigData)

centroides <- diffCemigData$alimentador
k <- 3

TestEstatistic <- calculatorElementsTestEstatistic(centroides,diffCemigData)

clusters <- geradorCluster(FALSE,diffCemigData,TestEstatistic,k)

resultSimul <- monteCarloSimu(diffCemigData,TestEstatistic,k)


