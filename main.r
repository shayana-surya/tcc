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
radius <- 55000
bound <-99
alpha <- 0.05

#ti <- proc.time()

sig <- calculator_R(radius,bound,diffCemigData,alpha,1)
print(sig)
showSignificantClustersInfo(sig,relacaoAlimentadorId,radius)


#significantClusters_R <- calculator_R(radius,bound,diffCemigData,alpha)
#typeof(significantClusters_R)
#showSignificantClustersInfo(significantClusters_R,relacaoAlimentadorId,radius)
#proc.time() - ti

########################### SECAO RCPP POR RAIO ########################### 

## Tempo: 0.04 s
#ti <- proc.time()
distanceMatrix_Rcpp <- createEuclideanDistanceUsingRcpp(diffCemigData)
#proc.time() - ti

# Teste em Rcpp
# Tempo: 0.05s
#ti <- proc.time()
clustersRaio_Rcpp <- geradorClustersRcpp(as.matrix(distanceMatrix_Rcpp),as.matrix(diffCemigData),nrow(diffCemigData),radius)
#proc.time() - ti

# Teste em Rcpp
# Raio = 300 km e 499 Sim
# Tempo: 20.13 segundos
#ti <- proc.time()
resultSimulRaio_Rcpp <- MonteCarloRcpp(distanceMatrix_Rcpp,diffCemigData,radius,bound)
#proc.time() - ti

#ti <- proc.time()
significativosRaio_Rcpp <- clustersSignificativosRaioRcpp(resultSimulRaio_Rcpp,clustersRaio_Rcpp)
#proc.time() - ti


########################### SECAO R POR RAIO #############################

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



########################### SECAO R POR k #############################

#para k = 30 levamos 155,88 segundos
#com a remocao do segundo for e inclusao do rbind para alimentar resultados, o tempo caiu para 115,22 segundos
#para k igual a 250 levou-se 125,68 segundos
ti <- proc.time()
clusters <- geradorCluster_K(TestEstatistic, matrizDistancia, diffCemigData, k)
proc.time() - ti

#200 simulacoes com k=10 levaram 28.694,02 segundos. Quase 8hrs
# 66,77 segundos para uma simulacao de 100 iteracoes com k igual a 30. Isso apos as alteracoes
# 1000 simulações para k igual a 250 levou 3.601,34 segundos ou 1hr
ti <- proc.time()
resultSimul <- monteCarloSimu_K(TestEstatistic, diffCemigData, clusters, k, bound)
proc.time() - ti

ti <- proc.time()
clustersSignificativos_K <- clustersSignificativos(resultSimul,clusters,k)
proc.time() - ti
 

