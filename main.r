### ---
### title: "tcc"
### author: "Shayana e Rafael"
### date: "6 de novembro de 2019"
### ---

require(readxl)
require(data.table)
require(REAT)
require(Rcpp)
require(rlist)

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

###################### PARAMETROS PARA GERACAO DE CLUSTERS###################### 

radius <- 46460
bound <-499
alpha <- 0.05
#ti <- proc.time()
#significantClusters_R <- calculator_R(radius,bound,diffCemigData,alpha)
#typeof(significantClusters_R)
#showSignificantClustersInfo(significantClusters_R,relacaoAlimentadorId,radius)
#proc.time() - ti


########################### ALGORITMOS EM RCPP ########################### 

## Tempo: 0.04 s
#ti <- proc.time()
#distanceMatrix_Rcpp <- createEuclideanDistanceUsingRcpp(diffCemigData)
#proc.time() - ti

# Teste em Rcpp
# Tempo: 0.05s
#ti <- proc.time()
#clustersRaio_Rcpp <- geradorClustersRcpp(as.matrix(distanceMatrix_Rcpp),as.matrix(diffCemigData),nrow(diffCemigData),radius)
#proc.time() - ti

# Teste em Rcpp
# Raio = 300 km e 499 Sim
# Tempo: 20.13 segundos
#ti <- proc.time()
#resultSimulRaio_Rcpp <- MonteCarloRcpp(distanceMatrix_Rcpp,diffCemigData,radius,bound)
#proc.time() - ti

#ti <- proc.time()
#significativosRaio_Rcpp <- clustersSignificativosRaioRcpp(resultSimulRaio_Rcpp,clustersRaio_Rcpp)
#proc.time() - ti


########################### ALGORITMOS EM R #############################

#for (radius in seq(from = 1500, to = 100000, by = 10))
#{
## Tempo: 0.16 s
#ti <- proc.time()
distanceMatrix_R <- createEuclideanDistance(diffCemigData)
#proc.time() - ti


# # Tempo:  0,17s
# ti <- proc.time()
radiusClusters_R <- clusterGeneratorRadius(distanceMatrix_R,diffCemigData,radius)
# proc.time() - ti

# Tempo: 15,00 S
# ti <- proc.time()
resultSimulRadius_R <- monteCarloSimuRadius(diffCemigData, radiusClusters_R, radius, bound)
# proc.time() - ti
#print(radius)
# Tempo: 0,11 S
#ti <- proc.time()
significantClusters_R <- significantRadiusClusters(resultSimulRadius_R,radiusClusters_R,alpha)
#proc.time() - ti
#print(significantClusters_R)
#}

########################### SHINY ########################### 

#showSignificantClustersInfo(significantClusters_R,relacaoAlimentadorId)
 

