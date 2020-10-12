
############################## SECAO GERAL ##############################
createData <- function(joinData){
  id <- 1
  relacaoAlimentadorId <- data.table(ID = numeric(),
                                     alimentadora = character(),
                                     lat = numeric(),
                                     lon = numeric(),
                                     consumo2013 = numeric(),
                                     consumo2018 = numeric(),
                                     difConsumo = numeric()
                                     
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
  
  relacaoAlimentadorId <-diffCemigData[,c("ID","alimentadora","lat","lon","consumo2013","consumo2018","difConsumo")]
  diffCemigData <- diffCemigData[,2:7]
  list_data <- list(diffCemigData,excluded,relacaoAlimentadorId)
  return (list_data);
}

########################### SECAO RCPP ###########################

########## Rcpp - LOGICA POR RAIO

significantRadiusClustersRcpp <- function(resultSimul,clustersRcpp,alpha){
  significativosRaio <- vector()
  nsim = length(resultSimul)
  #abaixo preciso ver quantas observacoes na simulacao sao maiores que o valor realmente observado
  for(i in 1:nrow(clustersRcpp)){
    
    EstTeste <- clustersRcpp[i,3]
    pvalor <- (sum(resultSimul > EstTeste))/(nsim + 1)
    if(pvalor < alpha){
      #salvando o centroide daquele cluster   
      significativosRaio <- append(significativosRaio,i)
    }
  }
  return (significativosRaio)
}

########## Rcpp - LOGICA POR K-ALIMENTADORES

significantKClustersRcpp <- function(resultSimul,clustersRcpp,alpha){
  significativosK <- vector()
  nsim = length(resultSimul)
  #abaixo preciso ver quantas observacoes na simulacao sao maiores que o valor realmente observado
  for(i in 1:nrow(clustersRcpp)){
    
    EstTeste <- clustersRcpp[i,3]
    pvalor <- (sum(resultSimul > EstTeste))/(nsim + 1)
    if(pvalor < alpha){
      #salvando o centroide daquele cluster   
      significativosK <- append(significativosK,i)
    }
  }
  return (significativosK)
}


############################## SECAO R ##############################

########## R - GERAL

createEuclideanDistanceR <- function(diffCemigData) {
  
  distanceMatrix <- as.matrix( dist(diffCemigData[,c("x","y")], method = "euclidean"))
  
  return (distanceMatrix)
}
########## R - LOGICA POR RAIO

clusterGeneratorRadius <- function(distanceMatrix,consumo,N,Radius){
  
  resultados <- list()
  
  for (i in 1:N){
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
 
monteCarloSimulRadiusR <- function(consumo,N,radiusClusters,radius,bound){
  
  resultSimul <- vector()
  
  for (m in 1:bound) {
    baseRandom <- sample(consumo, N) 
    resultSimul[m] <- clusterGeneratorRadiusSimulR(baseRandom,radiusClusters,radius)
  }
  return (resultSimul)
}

clusterGeneratorRadiusSimulR <- function(consumo,radiusClusters,radius){
  
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

significantRadiusClustersR <- function(resultSimul,radiusClusters,alpha){
  
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

########## R - LOGICA K-ALIMENTADORES

createIndexMatrixR <- function(matrizDistancia) {
  #obs: A funcao order não retorna o vetor original ordenado, mas retorna um vetor com as posicao para que x fique em ordem crescente.
  
  matIDX <- matrix(NA, nrow(matrizDistancia), ncol(matrizDistancia))
  for(idx in 1:nrow(matrizDistancia)){
    matIDX[idx,] <- order(matrizDistancia[idx,], decreasing=FALSE)
  }
  return (matIDX)
}

clusterGeneratorK_R <- function(IndexMatrix,consumo,N,k){
  
  resultados <- data.frame()
  
  for (i in 1:N) {
    pos <- IndexMatrix[i,] #Pego toda a linha da matriz de distancia ID
    x.in  <- consumo[pos[1:k]]
    x.out <- consumo[pos[(k+1):N]]
    
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

monteCarloSimulK_R <- function(IndexMatrix,consumo,N,clusters,k,bound){
  
  resultSimul <- vector()
  
  for (m in 1:bound) {
    baseRandom <- sample(consumo,N)
    resultSimul[m] <- clusterGeneratorKSimulR(IndexMatrix,baseRandom,N,clusters,k)
  }
  return (resultSimul)
}

clusterGeneratorKSimulR <- function(IndexMatrix,consumo,N,clusters,k){
  
  for (i in 1:N) {
    
    pos <- IndexMatrix[i,] #Pego toda a linha da matriz de distancia ID
    x.in  <- consumo[pos[1:k]]
    x.out <- consumo[pos[(k+1):N]]
    
    sigma2z <- sum((x.in-mean(x.in))^2) + sum((x.out-mean(x.out))^2)
    sigma2z <- sigma2z/N
    
    estatistica <- (- N*log(sqrt(sigma2z)))
    
    clusters[i,k+1] <- estatistica
  }
  clusters <- clusters[order(clusters[k+1],decreasing = TRUE),]
  return(clusters[1,k+1])
  
}

significantKClustersR <- function(resultSimul,clusters,k,alpha){

  
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

showHist <- function(dataNum,dados2013,dados2018,diffCemigData){
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

showMapInfo <- function(dataNum,list_data,relacaoAlimentadorId){
  if (dataNum == 1)
    option = data.table(relacaoAlimentadorId[,2:5])
  else if (dataNum == 2)
    option = data.table(relacaoAlimentadorId[c("alimentadora","lat","lon","consumo2018")])
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
    addMarkers(lng = option$lon,lat= option$lat,label = option$alimentador,clusterOptions = markerClusterOptions())
}

showSummary <- function(dataNum,dados2013,dados2018,diffCemigData){
  if (dataNum == 1)
    return(summary(dados2013$ConsumoMedido))
  else if (dataNum == 2)
    return(summary(dados2018$ConsumoMedido))
  else if (dataNum == 3)
    return(summary(diffCemigData$difConsumo))
  else
    return()
  
  
}

optionExcluded <- function(data){
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

whatDataWillBeUsed <- function (diffCemigData,flag){
  if (flag == 1)
    return (4)
  else if (flag == 2)
    return (5)
  else if (flag == 3)
    return (6)
}

calculator_R <- function(radius,bound,diffCemigData,alpha,flag){
  
  radius <- as.numeric(radius)
  bound <- as.numeric(bound)
  alpha <- as.numeric(alpha)
  flag <- as.numeric(flag)
  N <- nrow(diffCemigData)
  
  col <- whatDataWillBeUsed(diffCemigData,flag)
  data <-diffCemigData[,col]
  timeVector <- rep(0, 5)
  
  ti <- Sys.time()
  distanceMatrix_R <- createEuclideanDistanceR(diffCemigData)
  timeVector[1] <- round(as.numeric(Sys.time() - ti, units = "secs"),2)
  
  ti <- Sys.time()
  radiusClusters_R <- clusterGeneratorRadius(distanceMatrix_R,data,N,radius)
  timeVector[2] <- round(as.numeric(Sys.time() - ti, units = "secs"),2)
  
  ti <- Sys.time()
  resultSimulRadius_R <- monteCarloSimulRadiusR(data,N, radiusClusters_R, radius,bound)
  timeVector[3] <- round(as.numeric(Sys.time() - ti, units = "secs"),2)
  
  ti <- Sys.time()
  sigRadiusClusters_R <- significantRadiusClustersR(resultSimulRadius_R,radiusClusters_R,alpha)
  timeVector[4] <- round(as.numeric(Sys.time() - ti, units = "secs"),2)
  
  timeVector[5] <- round(as.numeric(timeVector[1]+ timeVector[2] + timeVector[3] + timeVector[4],units = "secs"),2)
  list_data <- list(sigRadiusClusters_R,timeVector,resultSimulRadius_R)
  
  return(list_data)
}

calculator_Rcpp <- function(radius,bound,diffCemigData,alpha,flag){
  
  radius <- as.numeric(radius)
  bound <- as.numeric(bound)
  alpha <- as.numeric(alpha)
  flag <- as.numeric(flag)
  N <- nrow(diffCemigData)
  
  col <- whatDataWillBeUsed(diffCemigData,flag)
  data <-diffCemigData[,col]
  timeVector <- rep(0, 5)
  
  ti <- Sys.time()
  distanceMatrix_Rcpp <- createEuclideanDistanceRcpp(as.matrix(diffCemigData),N)
  timeVector[1] <- round(as.numeric(Sys.time() - ti, units = "secs"),2)
  
  ti <- Sys.time()
  radiusClusters_Rcpp <- clusterGeneratorRadiusRcpp(as.matrix(distanceMatrix_Rcpp),as.vector(data),N,radius)
  timeVector[2] <- round(as.numeric(Sys.time() - ti, units = "secs"),2)
  
  ti <- Sys.time()
  resultSimulRadius_Rcpp <- monteCarloSimulRadiusRcpp(as.matrix(distanceMatrix_Rcpp), as.vector(data),N, radius, bound)
  timeVector[3] <- round(as.numeric(Sys.time() - ti, units = "secs"),2)
  
  ti <- Sys.time()
  sigRadiusClusters_Rcpp <- significantRadiusClustersRcpp(resultSimulRadius_Rcpp,radiusClusters_Rcpp,alpha)
  timeVector[4] <- round(as.numeric(Sys.time() - ti, units = "secs"),2)
  
  timeVector[5] <- round(as.numeric(timeVector[1]+ timeVector[2] + timeVector[3] + timeVector[4],units = "secs"),2)
  list_data <- list(sigRadiusClusters_Rcpp,timeVector)
  
  return(list_data)
}

calculator_K_R <- function(k,bound,diffCemigData,alpha,flag){
  k <- as.numeric(k)
  bound <- as.numeric(bound)
  alpha <- as.numeric(alpha)
  flag <- as.numeric(flag)
  N <- nrow(diffCemigData)
  
  col <- whatDataWillBeUsed(diffCemigData,flag)
  data <-diffCemigData[,col]
  timeVector <- rep(0, 6)
  
  ti <- Sys.time()
  distanceMatrixK_R <- createEuclideanDistanceR(diffCemigData)
  timeVector[1] <- round(as.numeric(Sys.time() - ti, units = "secs"), 2)
  
  ti <- Sys.time()
  IndexMatrix_R <-createIndexMatrixR(distanceMatrixK_R)
  timeVector[2] <- round(as.numeric(Sys.time() - ti, units = "secs"), 2)
  
  ti <- Sys.time()
  KClusters_R <- clusterGeneratorK_R(IndexMatrix_R, data,N, k)
  timeVector[3] <- round(as.numeric(Sys.time() - ti, units = "secs"), 2)
  
  ti <- Sys.time()
  resultSimulK_R <- monteCarloSimulK_R(IndexMatrix_R,data, N, KClusters_R, k, bound)
  timeVector[4] <- round(as.numeric(Sys.time() - ti, units = "secs"), 2)
  
  ti <- Sys.time()
  sigKClusters_R <- significantKClustersR(resultSimulK_R, KClusters_R,k,alpha)
  timeVector[5] <- round(as.numeric(Sys.time() - ti, units = "secs"), 2)
  
  timeVector[6] <- round(as.numeric(timeVector[1]+ timeVector[2] + timeVector[3] + timeVector[4] + timeVector[5],units = "secs"), 2)
  list_data <- list(sigKClusters_R,timeVector)
  
  return(list_data)
}

calculator_K_Rcpp <- function(k,bound,diffCemigData,alpha,flag){
  
  k <- as.numeric(k)
  bound <- as.numeric(bound)
  alpha <- as.numeric(alpha)
  flag <- as.numeric(flag)
  N <- nrow(diffCemigData)
  
  col <- whatDataWillBeUsed(diffCemigData,flag)
  data <-diffCemigData[,col]
  timeVector <- rep(0, 6)
  
  ti <- Sys.time()
  distanceMatrixK_Rcpp <- createEuclideanDistanceRcpp(as.matrix(diffCemigData),N)
  timeVector[1] <- round(as.numeric(Sys.time() - ti, units = "secs"),2)
  
  ti <- Sys.time()
  indexMatrix_Rcpp <- createIndexMatrixRcpp(as.matrix(distanceMatrixK_Rcpp), N) 
  timeVector[2] <- round(as.numeric(Sys.time() - ti, units = "secs"),2)
  
  ti <- Sys.time()
  Kclusters_Rcpp <- clusterGeneratorK_Rcpp(as.matrix(indexMatrix_Rcpp), as.vector(data), N, k)
  timeVector[3] <- round(as.numeric(Sys.time() - ti, units = "secs"),2)
  
  ti <- Sys.time()
  resultSimulK_Rcpp<- monteCarloSimulK_Rcpp(as.matrix(indexMatrix_Rcpp), as.vector(data), N, k, bound)
  timeVector[4] <- round(as.numeric(Sys.time() - ti, units = "secs"),2)
  
  ti <- Sys.time()
  sigKClusters_Rcpp <- significantKClustersRcpp(resultSimulK_Rcpp,Kclusters_Rcpp,alpha)
  timeVector[5] <- round(as.numeric(Sys.time() - ti, units = "secs"),2)
  
  timeVector[6] <- round(as.numeric(timeVector[1]+ timeVector[2] + timeVector[3] + timeVector[4] + timeVector[5],units = "secs"),2)
  list_data <- list(sigKClusters_Rcpp,timeVector)
  
  return(list_data)
}

showSignificantClustersInfo <- function(significantClusters_R,relacaoAlimentadorId){
  option = data.table(relacaoAlimentadorId[significantClusters_R,2:4])
  label_opt <- {}
  if(nrow(option) != 0)
    label_opt <- option$alimentadora
  
  shp <- readOGR("Mapa\\.","MG_UF_2019", stringsAsFactors=FALSE, encoding="UTF-8")
  pal <- colorBin("Blues",domain = NULL,n=5) #cores do mapa

  leaflet(data = shp) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(fillColor = ~pal(1), 
                fillOpacity = 0.8, 
                color = "#BDBDC3", 
                weight = 1)%>%
      addMarkers(lng = option$lon,lat= option$lat,label = label_opt,clusterOptions = markerClusterOptions())
}

boxsplotFunction<- function(diffCemigData,flag){

  flag <- as.numeric(flag)
  if(flag == 1 || flag ==2)
  {
  df3 <- data.table(".2013" = remove_outliers(diffCemigData$consumo2013) ,".2018" = remove_outliers(diffCemigData$consumo2018))
  
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


