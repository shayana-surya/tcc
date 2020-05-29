createData <- function(joinDados){
  
  diffCemigData <- data.table(alimentador = character(),
                              x = numeric(),
                              y = numeric(),
                              difConsumo = numeric()
  )
  
  excluidas <- data.table(alimentador = character(),
                          dados2013.x = numeric(),
                          dados2013.y = numeric(),
                          dados2018.x = numeric(),
                          dados2018.y = numeric(),
                          dados2013.consumo = numeric(),
                          dados2018.consumo = numeric()
  )
  
  for (i in 1:nrow(joinDados))
  {
    if(
      (!is.na(joinDados$ConsumoMedido.x[i]) || !is.na(joinDados$ConsumoMedido.y[i])) &&
      (!is.na(joinDados$x.x[i]) || !is.na(joinDados$x.y[i])) && 
      (!is.na(joinDados$y.x[i]) || !is.na(joinDados$y.y[i]))
    )
    {
      joinDados$ConsumoMedido.x[i] <- replace(x = joinDados$ConsumoMedido.x[i], list = is.na(joinDados$ConsumoMedido.x[i]), values = 0)
      joinDados$ConsumoMedido.y[i] <- replace(x = joinDados$ConsumoMedido.y[i], list = is.na(joinDados$ConsumoMedido.y[i]), values = 0)
      
      if(!is.na(joinDados$x.x[i]) && !is.na(joinDados$y.x[i]))
      {
        validX = joinDados$x.x[i]
        validY = joinDados$y.x[i]
        
      }
      else
      {
        validX = joinDados$x.y[i]
        validY = joinDados$y.y[i]
        
      }
      
      diffCemigData <- rbind(diffCemigData, data.frame(
        alimentador = joinDados$alimentador[i],
        x = validX,
        y = validY,
        difConsumo = joinDados$ConsumoMedido.y[i] - joinDados$ConsumoMedido.x[i]
        
      ))
    }
    else
    {
      excluidas <- rbind(excluidas, data.frame(
        alimentador = joinDados$alimentador[i],
        dados2013.x = joinDados$x.x[i],
        dados2013.y = joinDados$y.x[i],
        dados2018.x = joinDados$x.y[i],
        dados2018.y = joinDados$y.y[i],
        dados2013.consumo = joinDados$ConsumoMedido.x[i],
        dados2018.consumo = joinDados$ConsumoMedido.y[i]
      ),fill=TRUE)
    }
    
  }
  list_data <- list(diffCemigData,excluidas)
  return (list_data);
}


createEuclideanDistance <- function(diffCemigData) {
  
  matrizDistancia <- dist.mat(diffCemigData,"alimentador","y","x",diffCemigData,"alimentador","y","x", unit = "km")
  matrizDistancia <- matrizDistancia[order(matrizDistancia$from,matrizDistancia$distance),]
  
  return (matrizDistancia)
}

EstatisticTestElementsCalculator <- function(centroides,diffCemigData) {
  TestEstatistic <- data.frame(N = length(centroides), #Equivale ao total geral de observacoes
                               mi = mean(diffCemigData$difConsumo), #Media geral da diferenca de consumo
                               sigma2 =  var(diffCemigData$difConsumo),  #Variancia geral da diferenca de consumo
                               sigma = sd(diffCemigData$difConsumo), #Desvio padrao geral da diferenca de consumo
                              X = 0, #Somatorio da variavel diferenca de consumo
                              Qgeral = 0, #Somatorio do quadrado da variavel diferenca de consumo
                              parte2 = 0, #Somatorio em i do quadrado da diferenca de consumo menos a media, isso dividido por 2 vezes a variancia
                              fixa= 0
  )
  
  for (e in 1:TestEstatistic$N) {
    TestEstatistic$X <- TestEstatistic$X + as.numeric(diffCemigData[e,4])
    TestEstatistic$Qgeral <- TestEstatistic$Qgeral + as.numeric((diffCemigData[e,4])^2)
    TestEstatistic$parte2 <- TestEstatistic$parte2 + ((diffCemigData[e,4]-TestEstatistic$mi)^2)/(2*TestEstatistic$sigma2)
  }
  
  TestEstatistic$fixa <- as.numeric((TestEstatistic$N*log(TestEstatistic$sigma) + TestEstatistic$parte2 - TestEstatistic$N/2))
  
  
  return(TestEstatistic)
}


geradorCluster <- function(flagSimulacao,dados,TestEstatistic,k){
  i <- 1
  clustersSimul <- data.frame()
  clusters <- data.frame()
  while (i <= length(centroides)) {
    estatistica <- 0
    sigma2z <- 0
    resultados <- data.frame()
    somatorio <- 0 #Somatorio do quadrado da variavel diferenca de consumo dentro do cluster
    Xz <- 0 #Somatorio da variavel diferenca de consumo dentro do cluster
    df_aux <- matrizDistancia[matrizDistancia$from == centroides[i], ]
    
    for (j in 1:k) {
      resultados[i,j] <- as.character(df_aux[j, 2])
      somatorio <- somatorio + as.numeric((dados[dados$alimentador == resultados[i,j],4])^2)
      Xz <- Xz + as.numeric(dados[dados$alimentador == resultados[i,j],4])
    }
    
    Nz <- as.numeric(length(resultados[i,]))
    miz <- Xz/Nz
    lambdaz <- (TestEstatistic$X - Xz)/(TestEstatistic$N - Nz)
    
    sigma2z <- (1/TestEstatistic$N) * (somatorio - 2*Xz*miz + Nz*(miz^2) + (TestEstatistic$Qgeral - somatorio) - 2*(TestEstatistic$X - Xz)*lambdaz + (TestEstatistic$N - Nz)*(lambdaz^2))
    estatistica <- TestEstatistic$fixa - TestEstatistic$N*log(sqrt(sigma2z))
    
    resultados[i,j+1] <- estatistica
    if(flagSimulacao == FALSE){
      clusters <- rbind(clusters,resultados) #se estou gerando os clusters iniciais, com dados originais
    }
    else{
      clustersSimul <- rbind(clustersSimul,resultados)
    }
    
    i <- i+1
  }
  
  if(flagSimulacao == FALSE){
    clusters <- na.omit(clusters)
    clusters <- clusters[order(clusters[k+1],decreasing = TRUE),]
    return(clusters)
  }
  else{
    clustersSimul <- na.omit(clustersSimul)
    clustersSimul <- clustersSimul[order(clustersSimul[k+1],decreasing = TRUE),]
    return(as.numeric(clustersSimul[1,k+1]))
  }
  
}

monteCarloSimu <- function(diffCemigData,TestEstatistic,k){
  limite <- 5
  resultSimul <- vector()
  
  for (m in 1:limite) {
    baseRandom <- diffCemigData
    baseRandom[,4] <- sample(diffCemigData$difConsumo) #randomizando a coluna diferenca de consumo
    resultSimul[m] <- geradorCluster(TRUE,baseRandom,TestEstatistic,k)
  }
  return (resultSimul)
}
                          


