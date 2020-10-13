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
#library(dplyr)
#library(leaflet)

source(file="functions.r")
sourceCpp('functionsC++.cpp')

###################### LEITURA E TRATAMENTO DA BASE DE DADOS ###################### 

dados2018 <- read_excel("DadosAlimentadoresCEMIG_XY.xlsx", sheet = 2)
dados2013 <- read_excel("DadosAlimentadoresCEMIG_XY.xlsx", sheet = 1)

joinData <- merge(x = dados2013, y = dados2018, by="alimentador", all=TRUE)
names(joinData)<- c("alimentador", "lat.2013", "lon.2013","x.2013", "y.2013","ConsumoMedido.2013",
                    "lat.2018", "lon.2018","x.2018", "y.2018","ConsumoMedido.2018")	

list_data <- createData(joinData)
diffCemigData <- data.frame(list_data[1])
excluded <- data.frame(list_data[2])
relacaoAlimentadorId <- data.frame(list_data[3])

###################### PARAMETROS PARA GERACAO DE CLUSTERS ###################### 

radius <- 55000
bound <-99
alpha <- 0.05
k <- 250
type <- 1


############## TESTE SHINY - APAGAR ####################
#ti <- proc.time()

#showMapInfo(2,list_data,relacaoAlimentadorId)

sig <- calculator_K_R(k,bound,diffCemigData,alpha,1)
#boxsplotFunction(diffCemigData,2)


data <- calculator_R(radius,bound,diffCemigData,alpha,1)
showSignificantClustersInfo(data[[1]],relacaoAlimentadorId)

#proc.time() - ti


########################### SECAO RCPP ########################### 

########## RCPP - GERAL

# CALCULO DA MATRIZ DE DISTANCIA
# Tempo: 0.04s
ti <- proc.time()
distanceMatrix_Rcpp <- createEuclideanDistanceRcpp(as.matrix(diffCemigData),nrow(diffCemigData))
proc.time() - ti

########## RCPP - LOGICA POR RAIO

# GERACAO DE CLUSTERS PARTINDO DE CADA ALIMENTADOR
# Tempo: 0.07s
ti <- proc.time()
radiusClusters_Rcpp <- clusterGeneratorRadiusRcpp(as.matrix(distanceMatrix_Rcpp),as.vector(diffCemigData[,type + 3]),nrow(diffCemigData),radius)
proc.time() - ti

# FUNCAO MONTE CARLO PARA ITERAR AS SIMULACOES
# Tempo: 28.78s
ti <- proc.time()
resultSimulRadius_Rcpp <- monteCarloSimulRadiusRcpp(as.matrix(distanceMatrix_Rcpp),as.vector(diffCemigData[,type + 3]),nrow(diffCemigData),radius,bound)
proc.time() - ti

# IDENTIFICACAO DE CLUSTERS SIGNIFICATIVOS - FUNCAO ESTA EM R
# Tempo: 0.05s
ti <- proc.time()
sigRadiusClusters_Rcpp <- significantRadiusClustersRcpp(resultSimulRadius_Rcpp,radiusClusters_Rcpp,alpha)
proc.time() - ti

########## RCPP - LOGICA K-ALIMENTADORES

## CRIACAO DE UMA MATRIZ DE IDs COM BASE NA MATRIZ DE DISTANCIA
# Tempo: 0.25s
ti <- proc.time()
IndexMatrix_Rcpp <- createIndexMatrixRcpp(as.matrix(distanceMatrix_Rcpp),nrow(distanceMatrix_Rcpp))
proc.time() - ti

# GERACAO DE CLUSTERS PARTINDO DE CADA ALIMENTADOR
# Tempo: 0.07s
ti <- proc.time()
Kclusters_Rcpp <- clusterGeneratorK_Rcpp(as.matrix(IndexMatrix_Rcpp),as.vector(diffCemigData[,type + 3]),nrow(diffCemigData),k)
proc.time() - ti

# FUNCAO MONTE CARLO PARA ITERAR AS SIMULACOES
# Tempo: 30.34s
ti <- proc.time()
resultSimulK_Rcpp <- monteCarloSimulK_Rcpp(as.matrix(IndexMatrix_Rcpp),as.vector(diffCemigData[,type + 3]),nrow(diffCemigData),k,bound)
proc.time() - ti

# IDENTIFICACAO DE CLUSTERS SIGNIFICATIVOS - FUNCAO ESTA EM R
# Tempo: 0.03s
ti <- proc.time()
sigKClusters_Rcpp <- significantKClustersRcpp(resultSimulK_Rcpp,Kclusters_Rcpp,alpha)
proc.time() - ti

############################## SECAO R ##############################

########## R - GERAL

# CALCULO DA MATRIZ DE DISTANCIA
# Tempo: 0.16 s
ti <- proc.time()
distanceMatrix_R <- createEuclideanDistanceR(diffCemigData)
proc.time() - ti

########## R - LOGICA POR RAIO

# GERACAO DE CLUSTERS PARTINDO DE CADA ALIMENTADOR
# Tempo:  0,50s
ti <- proc.time()
radiusClusters_R <- clusterGeneratorRadius(distanceMatrix_R,diffCemigData[,type + 3],nrow(diffCemigData),radius)
proc.time() - ti

# FUNCAO MONTE CARLO PARA ITERAR AS SIMULACOES
# Tempo: 40,35 S
ti <- proc.time()
resultSimulRadius_R <- monteCarloSimulRadiusR(diffCemigData[,type + 3],nrow(diffCemigData),radiusClusters_R,radius,bound)
proc.time() - ti

# IDENTIFICACAO DE CLUSTERS SIGNIFICATIVOS
# Tempo: 0,02 S
ti <- proc.time()
sigRadiusClusters_R <- significantRadiusClustersR(resultSimulRadius_R,radiusClusters_R,alpha)
proc.time() - ti

########## R - LOGICA K-ALIMENTADORES

## CRIACAO DE UMA MATRIZ DE IDs COM BASE NA MATRIZ DE DISTANCIA
# Tempo: 0.38s
ti <- proc.time()
IndexMatrix_R <- createIndexMatrixR(distanceMatrix_R)
proc.time() - ti

# GERACAO DE CLUSTERS PARTINDO DE CADA ALIMENTADOR
# Tempo: 7.91s
ti <- proc.time()
Kclusters_R <- clusterGeneratorK_R(IndexMatrix_R,diffCemigData[,type + 3],nrow(diffCemigData),k)
proc.time() - ti

# FUNCAO MONTE CARLO PARA ITERAR AS SIMULACOES
# Tempo: 92.11s
ti <- proc.time()
resultSimulK_R <- monteCarloSimulK_R(IndexMatrix_R,diffCemigData[,type + 3],nrow(diffCemigData),Kclusters_R,k,bound)
proc.time() - ti

# IDENTIFICACAO DE CLUSTERS SIGNIFICATIVOS
# Tempo: 0,06 S
ti <- proc.time()
sigKClusters_R <- significantKClustersR(resultSimulK_R,Kclusters_R,k,alpha)
proc.time() - ti


