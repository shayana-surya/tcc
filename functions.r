
########################### Limpeza dos dados de entrada ######
createData <- function(joinData){
  id <- 1
  relacaoAlimentadorId <- data.table(ID = numeric(),
                                     alimentadora = character(),
                                     lat = numeric(),
                                     lon = numeric()
                                     
  )
  
  diffCemigData <- data.table(alimentadora = character(),
                              ID = numeric(),
                              x = numeric(),
                              y = numeric(),
                              consumo2013 = numeric(),
                              consumo2018 = numeric(),
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
        ID = id,
        alimentadora = joinData$alimentador[i],
        x = validX,
        y = validY,
        consumo2013 = joinData$ConsumoMedido.2013[i],
        consumo2018 = joinData$ConsumoMedido.2018[i],
        difConsumo = joinData$ConsumoMedido.2018[i] - joinData$ConsumoMedido.2013[i],
        lat = validLat,
        lon = validLon
        
      ))
      id <- id + 1
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
  diffCemigData <- diffCemigData[,2:7]
  list_data <- list(diffCemigData,excluded,relacaoAlimentadorId)
  return (list_data);
}

whatDataWillBeUsed_Raio <- function (diffCemigData,flag)
{
  if (flag == 1)
    return (diffCemigData$consumo2013)
  else if (flag == 2)
    return (diffCemigData$consumo2018)
  else if (flag == 3)
    return (diffCemigData$difConsumo)
  
}

whatDataWillBeUsed_K <- function (diffCemigData,flag)
{
  if (flag == 1)
    return (4)
  else if (flag == 2)
    return (5)
  else if (flag == 3)
    return (6)
  
}

########################### ALGORITMOS EM RCPP - RAIO ###########################

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

clustersSignificativosRaioRcpp <- function(resultSimul,clustersRaioRcpp,alpha){
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


########################### ALGORITMOS EM R - RAIO #############################

createEuclideanDistance <- function(diffCemigData) {
  
  distanceMatrix <- as.matrix( dist(diffCemigData[,c("x","y")], method = "euclidean"))

  return (distanceMatrix)
}

clusterGeneratorRadius <- function(distanceMatrix,diffCemigData,Radius,flag){
  
  consumo = whatDataWillBeUsed_Raio(diffCemigData,flag)
  N = nrow(diffCemigData)
    
  resultados <- list()
  
  for (i in 1:nrow(diffCemigData)){
    pos <- (distanceMatrix[i, ] < Radius)
    # alimentadoras mais prox
    resultadosParciais <- which(pos == TRUE) 
    
    x.in  <- consumo[pos]
    x.out <- consumo[!pos]
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
 
monteCarloSimuRadius <- function(diffCemigData,radiusClusters,radius,bound, flag){
  
  dataUsed = whatDataWillBeUsed_Raio(diffCemigData,flag)
   
  N = nrow(diffCemigData)
  resultSimul <- vector()
  
  for (m in 1:bound) {
    consumo <- sample(dataUsed, N) 
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
    
    pvalor <- ((sum(resultSimul > EstTeste) + 1)/(nsim + 1))
    if(pvalor < alpha){
      #salvando o centroide daquele cluster   
      significativosRadius <- append(significativosRadius,i)
    }
  }
  return (significativosRadius)
}

########################### ALGORITMOS EM RCPP - k ###########################



########################### ALGORITMO EM R - K

createIndexMatrix <- function(matrizDistancia) {
  #obs: A função order não retorna o vetor original ordenado, mas retorna um vetor com as posições para que x fique em ordem crescente.
  
  matIDX <- matrix(NA, nrow(matrizDistancia), ncol(matrizDistancia))
  for(idx in 1:nrow(matrizDistancia)){
    matIDX[idx,] <- order(matrizDistancia[idx,], decreasing=FALSE)
  }
  return (matIDX)
}

geradorCluster_K <- function(IndexMatrix, dados, k,flag){
  
  col <- whatColumnWillBeUsed_K(IndexMatrix,flag)
  
  resultados <- data.frame()
  N = nrow(dados)
  
  for (i in 1:N) {
    pos <- IndexMatrix[i,] #Pego toda a linha da matriz de distancia ID
    x.in  <- dados[pos[1:k],col]
    x.out <- dados[pos[(k+1):N],col]
    
    sigma2z <- sum((x.in-mean(x.in))^2) + sum((x.out-mean(x.out))^2)
    sigma2z <- sigma2z/N
    estatistica <- (- N*log(sqrt(sigma2z)))
    
    relacao <- pos[1:k]
    relacao[k+1] <- estatistica
    resultados <- rbind(resultados,relacao)
  }
  
  resultados <- resultados[order(resultados[k+1],decreasing = TRUE),]
  return(resultados)
}

monteCarloSimu_K <- function(IndexMatrix,diffCemigData, clusters, k, bound,flag){
  
  resultSimul <- vector()
  col <- whatColumnWillBeUsed_K(IndexMatrix,flag)
  
  for (m in 1:bound) {
    baseRandom <- diffCemigData[,1:4]
    baseRandom[,4] <- sample(diffCemigData[,col]) #randomizando a coluna diferenca de consumo
    resultSimul[m] <- geradorClusterSimul(IndexMatrix, baseRandom, clusters, k)
  }
  return (resultSimul)
}

geradorClusterSimul_K <- function(IndexMatrix,dados,clusters,k){
  
  N = nrow(dados)
  
  for (i in 1:N) {
    
    pos <- IndexMatrix[i,] #Pego toda a linha da matriz de distancia ID
    x.in  <- dados[pos[1:k],4]
    x.out <- dados[pos[(k+1):N],4]
    
    sigma2z <- sum((x.in-mean(x.in))^2) + sum((x.out-mean(x.out))^2)
    sigma2z <- sigma2z/N
    
    estatistica <- (- N*log(sqrt(sigma2z)))
    
    clusters[i,k+1] <- estatistica
  }
  clusters <- clusters[order(clusters[k+1],decreasing = TRUE),]
  return(clusters[1,k+1])
  
}

clustersSignificativos_K <- function(resultSimul,clusters,k, flag){
  
  alpha <- 0.05
  resultSimul <- resultSimul[order(resultSimul,decreasing = TRUE)] #ordenando ele do maior para o menor
  significativos <- vector()
  
  #abaixo preciso ver quantas observacoes na simulacao sao maiores que o valor realmente observado
  for(i in 1:nrow(clusters)){
    pvalor <- (sum(resultSimul > clusters[i,k+1]))/(length(resultSimul)+1)
    
    if(pvalor < alpha){
      significativos[length(significativos)+1] <- clusters[i,1] #salvando o centroide daquele cluster
    }
    else{
      #se eu cair aqui, entendo que nao vai ter mais nenhum significativo
      return(significativos)
    }
  }
  return(significativos)
  }

########################### SHINY #####################################

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


calculator_R <- function(radius,bound,diffCemigData,alpha,flag)
{
  radius <- as.numeric(radius)
  bound <- as.numeric(bound)
  alpha <- as.numeric(alpha)
  flag <- as.numeric(flag)

  distanceMatrix_R <- createEuclideanDistance(diffCemigData)
  radiusClusters_R <- clusterGeneratorRadius(distanceMatrix_R,diffCemigData,radius,flag)
  resultSimulRadius_R <- monteCarloSimuRadius(diffCemigData, radiusClusters_R, radius, bound,flag)
  significantClusters_R <- significantRadiusClusters(resultSimulRadius_R,radiusClusters_R,alpha)
  #list_data_R <- list(distanceMatrix_R,radiusClusters_R,resultSimulRadius_R,significantClusters_R)
 
  return(significantClusters_R)
}

calculator_Rcpp <- function(radius,bound,diffCemigData,alpha,flag)
{
  radius <- as.numeric(radius)
  bound <- as.numeric(bound)
  alpha <- as.numeric(alpha)
  flag <- as.numeric(flag)
  
  col <- whatDataWillBeUsed_K(diffCemigData,flag)
  
  distanceMatrix_Rcpp <- createEuclideanDistanceUsingRcpp(diffCemigData)
  clustersRaio_Rcpp <- geradorClustersRcpp(as.matrix(distanceMatrix_Rcpp),as.vector(diffCemigData[,col]),nrow(diffCemigData),radius)
  resultSimulRaio_Rcpp <- MonteCarloRcpp(as.matrix(distanceMatrix_Rcpp), as.vector(diffCemigData[,col]),nrow(diffCemigData), radius, bound)
  significativosRaio_Rcpp <- clustersSignificativosRaioRcpp(resultSimulRaio_Rcpp,clustersRaio_Rcpp,alpha)
  return(significativosRaio_Rcpp)
}

calculator_K_R <- function(k,bound,diffCemigData,alpha,flag)
{
  k <- as.numeric(k)
  bound <- as.numeric(bound)
  alpha <- as.numeric(alpha)
  flag <- as.numeric(flag)
  
  distanceMatrix_K_R <- createEuclideanDistance(diffCemigData)
  IndexMatrix_K_R <-createIndexMatrix(distanceMatrix_K_R)
  Clusters_K_R <- geradorCluster_K(IndexMatrix_K_R, diffCemigData, k,flag)
  resultSimul_K_R <- monteCarloSimu_K(IndexMatrix_K_R,diffCemigData, Clusters_K_R, k, bound,flag)
  significantClusters_K_R <- clustersSignificativos_K(resultSimul_K_R,Clusters_K_R,alpha)
  return(significantClusters_K_R)
}

calculator_K_Rcpp <- function(k,bound,diffCemigData,alpha,flag)
{
  k <- as.numeric(k)
  bound <- as.numeric(bound)
  alpha <- as.numeric(alpha)
  flag <- as.numeric(flag)
  
  distanceMatrix_K_Rcpp <- createEuclideanDistanceUsingRcpp(diffCemigData)
  createIndexMatrix_K_Rcpp <- createIndexMatrix(distanceMatrix_K_R)
  
  return(createIndexMatrix_K_Rcpp)
}

showSignificantClustersInfo <- function(significantClusters_R,relacaoAlimentadorId,radius_R)
{
  option = data.table(relacaoAlimentadorId[significantClusters_R,2:4])
  
  shp <- readOGR("Mapa\\.","MG_UF_2019", stringsAsFactors=FALSE, encoding="UTF-8")
  pal <- colorBin("Blues",domain = NULL,n=5) #cores do mapa
  leaflet(data = shp) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(fillColor = ~pal(1), 
                fillOpacity = 0.8, 
                color = "#BDBDC3", 
                weight = 1)%>%
    addCircles(lng = option$lon,lat= option$lat,weight = 1, radius = as.numeric(radius_R))
}

boxsplotFunction<- function(diffCemigData,flag){

  if(flag == 1 || flag ==2)
  {
  df3 <- data.frame(".2013" = remove_outliers(diffCemigData$consumo2013) ,".2018" = remove_outliers(diffCemigData$consumo2018))
  
  meltData <- melt(df3)
                              
  ggplot(meltData, aes(factor(variable),value)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
    labs(x="Ano", y = "Consumo (Kwh)")
  }
  else if (flag ==3)
  {
    ggplot(diffCemigData, aes(x="Diferença entre anos",y=remove_outliers(difConsumo))) + 
      geom_boxplot(fill="slateblue", alpha=0.2,width = .2) + 
      labs(y = "Consumo (Kwh)")
    
  }

}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 2.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

