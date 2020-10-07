
createData <- function(joinData){
  
  relacaoAlimentadorId <- data.table(ID = numeric(),
                                     alimentadora = character(),
                                     lat = numeric(),
                                     lon = numeric()
                                     
  )
  
  diffCemigData <- data.table(alimentadora = character(),
                              ID = numeric(),
                              x = numeric(),
                              y = numeric(),
                              difConsumo = numeric(),
                              lat = numeric(),
                              lon = numeric()
  )
  
  excluded <- data.table(alimentadora = character(),
                          dados2013.x = numeric(),
                          dados2013.y = numeric(),
                          dados2013.lat = numeric(),
                          dados2013.lon = numeric(),
                          dados2018.x = numeric(),
                          dados2018.y = numeric(),
                          dados2018.lat = numeric(),
                          dados2018.lon = numeric(),
                          dados2013.consumo = numeric(),
                          dados2018.consumo = numeric()
  )
  
  for (i in 1:nrow(joinData))
  {
    if(
      (!is.na(joinData$ConsumoMedido.2013[i]) || !is.na(joinData$ConsumoMedido.2018[i])) &&
      (!is.na(joinData$x.2013[i]) || !is.na(joinData$x.2018[i])) && 
      (!is.na(joinData$y.2013[i]) || !is.na(joinData$y.2018[i])))
    {
      
      if (is.na(joinData$ConsumoMedido.2013[i]))
        joinData$ConsumoMedido.2013[i] <- 0
      if (is.na(joinData$ConsumoMedido.2018[i]))
        joinData$ConsumoMedido.2018[i] <- 0
      
      if(!is.na(joinData$x.2013[i]) && !is.na(joinData$y.2013[i]))
      {
        validX = joinData$x.2013[i]
        validY = joinData$y.2013[i]
        validLat = joinData$lat.2013[i]
        validLon = joinData$lon.2013[i]
        
      }
      else if (!is.na(joinData$x.2018[i]) && !is.na(joinData$y.2018[i]))
      {
        validX = joinData$x.2018[i]
        validY = joinData$y.2018[i]
        validLat = joinData$lat.2018[i]
        validLon = joinData$lon.2018[i]
      }
      diffCemigData <- rbind(diffCemigData, data.frame(
        ID = i,
        alimentadora = joinData$alimentador[i],
        x = validX,
        y = validY,
        difConsumo = abs(joinData$ConsumoMedido.2018[i] - joinData$ConsumoMedido.2013[i]),
        lat = validLat,
        lon = validLon
        
      ))
    }
    else
    {
      excluded <- rbind(excluded, data.frame(
        alimentadora = joinData$alimentador[i],
        dados2013.x = joinData$x.2013[i],
        dados2013.y = joinData$y.2013[i],
        dados2013.lat = joinData$lat.2013[i],
        dados2013.lon = joinData$lon.2013[i],
        dados2018.x = joinData$x.2018[i],
        dados2018.y = joinData$y.2018[i],
        dados2018.lat = joinData$lat.2018[i],
        dados2018.lon = joinData$lon.2018[i],
        dados2013.consumo = joinData$ConsumoMedido.2013[i],
        dados2018.consumo = joinData$ConsumoMedido.2018[i]
      ),fill=TRUE)
    }
  }
  
  relacaoAlimentadorId <-diffCemigData[,c("ID","alimentadora","lat","lon")]
  diffCemigData <- diffCemigData[,2:5]
  list_data <- list(diffCemigData,excluded,relacaoAlimentadorId)
  return (list_data);
}

########################### ALGORITMOS EM RCPP ###########################

createEuclideanDistanceUsingRcpp <- function(diffCemigData) {
  
  distanceMatrix <- createEuclideanDistanceRcpp(as.matrix(diffCemigData),nrow(diffCemigData))
  
  return (distanceMatrix)
}


MonteCarloRcpp <- function(matrizDistancia,diffCemigData,raio,bound){
  
  resultSimul <- vector()
  dados <- diffCemigData
  N <- nrow(diffCemigData)
  
  for (m in 1:bound) {
    dados$difConsumo <- sample(diffCemigData$difConsumo,nrow(diffCemigData)) 
    resultSimul[m] <- SimulClustersRcpp(as.matrix(matrizDistancia),as.matrix(dados),N,raio)
  }
  return (resultSimul)
}

clustersSignificativosRaioRcpp <- function(resultSimul,clustersRaioRcpp){
  alpha <- 0.05
  significativosRaio <- vector()
  nsim = length(resultSimul)
  #abaixo preciso ver quantas observacoes na simulacao sao maiores que o valor realmente observado
  for(i in 1:nrow(clustersRaioRcpp)){
    
    EstTeste <- clustersRaioRcpp[i,3]
    pvalor <- (sum(resultSimul > EstTeste))/(nsim + 1)
    if(pvalor < alpha){
      #salvando o centroide daquele cluster   
      significativosRaio <- append(significativosRaio,i)
    }
  }
  return (significativosRaio)
}


########################### ALGORITMOS EM R #############################

createEuclideanDistance <- function(diffCemigData) {
  
  distanceMatrix <- as.matrix( dist(diffCemigData[,c("x","y")], method = "euclidean"))

  return (distanceMatrix)
}

clusterGeneratorRadius <- function(distanceMatrix,diffCemigData,Radius){
  
  N = nrow(diffCemigData)
    
  resultados <- list()
  
  for (i in 1:nrow(diffCemigData)){
    pos <- (distanceMatrix[i, ] < Radius)
    # alimentadoras mais prox
    resultadosParciais <- which(pos == TRUE) 
    
    x.in  <- diffCemigData$difConsumo[pos]
    x.out <- diffCemigData$difConsumo[!pos]
    miz <- mean(x.in)
    lambdaz <- mean(x.out)
    
    sigma2z <- sum( (x.in - miz)^2 ) + sum( (x.out - lambdaz)^2 )
    sigma2z <- sigma2z/N
    
    estatistica <- -N*log(sigma2z)/2
    j <- sum(pos)
    
    resultadosParciais[j + 1] <- estatistica
    resultados[[i]] <- resultadosParciais
    
    }
    return(resultados)
}
 
monteCarloSimuRadius <- function(diffCemigData,radiusClusters,radius,bound){
  
  N = nrow(diffCemigData)
  resultSimul <- vector()
  
  for (m in 1:bound) {
    consumo <- sample(diffCemigData$difConsumo, N) 
    resultSimul[m] <- clusterGeneratorRadiusSimul(consumo,radiusClusters,radius)
  }
  return (resultSimul)
}

clusterGeneratorRadiusSimul <- function(consumo,radiusClusters,radius){
  
  N = length(consumo)
  estatisticaAux <- rep(NA, N)

  for (i in 1:N) {
    
    # numero de elemento no cluster menos a estatistica de teste que esta salva na ultima casa
    j <- length(radiusClusters[[i]]) - 1 
    
    # ID das alimentadoras do cluster
    alimentadorasCluster <- radiusClusters[[i]][1:j]
    
    pos <- rep(FALSE,N)
    pos[alimentadorasCluster] <- TRUE
    
    x.in  <- consumo[pos]
    x.out <- consumo[!pos]
    miz <- mean(x.in)
    lambdaz <- mean(x.out)
    sigma2z <- sum((x.in - miz)^2 ) + sum( (x.out - lambdaz)^2)
    sigma2z <- sigma2z/N
    
    estatisticaAux[i] <- -N*log(sigma2z)/2
    
  }
  return(max(estatisticaAux))
}

significantRadiusClusters <- function(resultSimul,radiusClusters,alpha){
  teste <- 1
  significativosRadius <- vector()
  nsim = length(resultSimul)
  #abaixo preciso ver quantas observacoes na simulacao sao maiores que o valor realmente observado
  for(i in 1:length(radiusClusters)){
    
    pos <- radiusClusters[[i]]
    EstTeste <- pos[length(pos)]
    
    pvalor <- sum(resultSimul > EstTeste)/(nsim + 1)
    if(pvalor < alpha){
      #salvando o centroide daquele cluster   
      significativosRadius <- append(significativosRadius,i)
    }
    if (pvalor < teste){
      teste <- pvalor
    }
  }
  print(teste)
  return (significativosRadius)
}


################# SHINY #####################################

showHist <- function(dataNum,dados2013,dados2018,diffCemigData)
{
  if (dataNum == 1)
  {
    option = dados2013$ConsumoMedido
    data = dados2013
    min = 0
    max = 77000000
    
  }
    
  else if (dataNum == 2)
  {
    option = dados2018$ConsumoMedido
    data = dados2018
    min = 0
    max = 82000000
  }
  else if (dataNum == 3)
  {
    option = diffCemigData$difConsumo
    data = diffCemigData
    min = -50000000
    max = 50000000
  }
  else
  {
    return()
  }
    
  
  step = 20000000
  print(ggplot(data, aes(x = option)) +
      geom_histogram(color = "white", fill = "lightblue") +
      theme_classic(base_size = 18) +
      scale_x_continuous(breaks = seq(from = min,to = max,by = step), limits = c(min,max)) +
      xlab("Consumo Energetico (KWh)") + 
      ylab("Frequencia"))
  
}

showMapInfo <- function(dataNum,dados2013,dados2018,list_data)
{
  if (dataNum == 1)
    option = dados2013[,1:3]
  else if (dataNum == 2)
    option = dados2018[,1:3]
  else if (dataNum == 3)
    option = data.frame(list_data[3])[,2:4]
  else
    option = optionExcluded(data.frame(list_data[2]))
    
  shp <- readOGR("Mapa\\.","MG_UF_2019", stringsAsFactors=FALSE, encoding="UTF-8")
  pal <- colorBin("Blues",domain = NULL,n=5) #cores do mapa
  
  leaflet(data = shp) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(fillColor = ~pal(1), 
                fillOpacity = 0.8, 
                color = "#BDBDC3", 
                weight = 1)%>%
    addMarkers(lng = option$lon,lat= option$lat, clusterOptions = markerClusterOptions())
}

showSummary <- function(dataNum,dados2013,dados2018,diffCemigData)
{
  if (dataNum == 1)
    return(summary(dados2013$ConsumoMedido))
  else if (dataNum == 2)
    return(summary(dados2018$ConsumoMedido))
  else if (dataNum == 3)
    return(summary(diffCemigData$difConsumo))
  else
    return()
  
  
}

optionExcluded <- function(data)
{
  response <- data.table(alimentadora = character(),
                         lat = numeric(),
                         lon = numeric()
                         
  )
  for (i in 1:nrow(data))
  {
    if(!is.na(data$dados2013.lat[i]) && !is.na(data$dados2013.lon[i]))
    {
      response <- rbind(response, data.frame(
        alimentadora = data$alimentadora[i],
        lat = data$dados2013.lat[i],
        lon = data$dados2013.lon[i]))
    }
    else if(!is.na(data$dados2018.lat[i]) && !is.na(data$dados2018.lon[i]))
    {
      response <- rbind(response, data.frame(
        alimentadora = data$alimentadora[i],
        lat = data$dados2018.lat[i],
        lon = data$dados2018.lon[i]))
    }
  }
  return (response)
}

calculator_R <- function(radius,bound,diffCemigData,alpha)
{

  distanceMatrix_R <- createEuclideanDistance(diffCemigData)
  radiusClusters_R <- clusterGeneratorRadius(distanceMatrix_R,diffCemigData,radius)
  resultSimulRadius_R <- monteCarloSimuRadius(diffCemigData, radiusClusters_R, radius, bound)
  significantClusters_R <- significantRadiusClusters(resultSimulRadius_R,radiusClusters_R,alpha)
  #list_data_R <- list(distanceMatrix_R,radiusClusters_R,resultSimulRadius_R,significantClusters_R)
  return(significantClusters_R)
}

calculator_Rcpp <- function(radius,bound,diffCemigData,alpha)
{
  
  distanceMatrix_Rcpp <- createEuclideanDistanceUsingRcpp(diffCemigData)
  clustersRaio_Rcpp <- geradorClustersRcpp(distanceMatrix_Rcpp,diffCemigData,radius)
  resultSimulRaio_Rcpp <- MonteCarloRcpp(diffCemigData, clustersRaio_Rcpp, radius, bound)
  significativosRaio_Rcpp <- clustersSignificativosRaioRcpp(resultSimulRaio_Rcpp,clustersRaio_Rcpp,alpha)
}

showSignificantClustersInfo <- function(significantClusters_R,relacaoAlimentadorId,radius_R)
{

  #significantClusters_R<- vector(list_data_R[4])

  option = data.table(relacaoAlimentadorId[significantClusters_R,2:4])
  
  shp <- readOGR("Mapa\\.","MG_UF_2019", stringsAsFactors=FALSE, encoding="UTF-8")
  pal <- colorBin("Blues",domain = NULL,n=5) #cores do mapa
  print(radius_R)
  leaflet(data = shp) %>%
    addProviderTiles("CartoDB.Positron") %>%
    #addPolygons(fillColor = ~pal(1), 
    #            fillOpacity = 0.8, 
    #            color = "#BDBDC3", 
    #            weight = 1)%>%
    addCircles(lng = option$lon,lat= option$lat, radius = radius_R)
}

histogramaSignificantCluster <- function(list_data_R){
  resultsimul<- list_data_R[3]
  significativos <- list_data_R[4]
  
  hist(resultSimul, xlim=c(min(c(resultSimul,significativos)), max(c(resultSimul,significativos))), col="light blue") 
  rug(resultSimul)
  abline(v=significativos, lwd=2, lty=2, col="red")
}

