
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

################## DIST.MAT ############################

createEuclideanDistance <- function(diffCemigData) {
  
  matrizDistancia <- dist.mat(diffCemigData,"ID","y","x",diffCemigData,"ID","y","x")
  matrizDistancia$from <- as.numeric(as.character(matrizDistancia$from))
  matrizDistancia$to <- as.numeric(as.character(matrizDistancia$to))
  matrizDistancia <- matrizDistancia[order( matrizDistancia$from,matrizDistancia$distance),]
  
  
  return (matrizDistancia)
}
####################################################

################## Rcpp ############################
createEuclideanDistance3 <- function(diffCemigData) {
  
  n = nrow(diffCemigData) * nrow(diffCemigData)
  
  matrizDistancia <- data.frame(matrix(NA, nrow = n, ncol = 4))
  names(matrizDistancia)[1] <- "from"
  names(matrizDistancia)[2] <- "to"
  names(matrizDistancia)[3] <- "ignore"
  names(matrizDistancia)[4] <- "distance"
  
  matrizDistancia <- createEuclideanDistanceRcpp(as.matrix(matrizDistancia),as.matrix(diffCemigData),nrow(diffCemigData))
  matrizDistancia <- as.data.frame(matrizDistancia)
  matrizDistancia <- matrizDistancia[order( matrizDistancia$from,matrizDistancia$distance),]
  return (matrizDistancia)
}
####################################################


################## Teste ############################
createEuclideanDistance2 <- function(diffCemigData) {
  
  #matrizDistancia <- data.table(
  #                            from = numeric(),
  #                            to = numeric(),
  #                            ignore = numeric(),
  #                            distance = numeric())
  n = nrow(diffCemigData) * nrow(diffCemigData)
  
  matrizDistancia <- data.frame(matrix(NA, nrow = n, ncol = 4))
  names(matrizDistancia)[1] <- "from"
  names(matrizDistancia)[2] <- "to"
  names(matrizDistancia)[3] <- "ignore"
  names(matrizDistancia)[4] <- "distance"
  
  x = 1
  
  
  for(from in 1:nrow(diffCemigData)){
    for(to in 1:nrow(diffCemigData)){
      dist <- sqrt((diffCemigData$x[to]-diffCemigData$x[from])^2+(diffCemigData$y[to]-diffCemigData$y[from])^2)
      matrizDistancia[x, ] = data.frame(
                            from = diffCemigData$ID[from],
                            to = diffCemigData$ID[to],
                            ignore = 0,
                            distance = dist)
                          
      x = x + 1
    }
  }
  
  matrizDistancia <- matrizDistancia[order( matrizDistancia$from,matrizDistancia$distance),]
  return (matrizDistancia)
}
####################################################




EstatisticTestElementsCalculator <- function(diffCemigData) {
  TestEstatistic <- data.frame(N = nrow(diffCemigData), #Equivale ao total geral de observacoes
                                X = 0, #Somatorio da variavel diferenca de consumo
                                Qgeral = 0 #Somatorio do quadrado da variavel diferenca de consumo
  )
  
  for (e in 1:TestEstatistic$N) {
    TestEstatistic$X <- TestEstatistic$X + diffCemigData[e,4]
    TestEstatistic$Qgeral <- TestEstatistic$Qgeral + (diffCemigData[e,4])^2
  }
  
  return(TestEstatistic)
}

######################### CALCULO POR QUANTIDADE K DE CLUSTERS  #####################################

geradorCluster <- function(TestEst, matrizDistancia, dados, k){
  resultados <- data.frame()
  
  for (i in 1:nrow(dados)) {
    estatistica <- 0
    sigma2z <- 0
    somatorio <- 0 #Somatorio do quadrado da variavel diferenca de consumo dentro do cluster
    Xz <- 0 #Somatorio da variavel diferenca de consumo dentro do cluster
    df_aux <- matrizDistancia[matrizDistancia$from == i,]

    pos <- df_aux[1:k, 2]
    somatorio <- sum(dados[pos,4]^2)
    Xz <- sum(dados[pos,4])
    
    Nz <- k
    miz <- Xz/Nz
    lambdaz <- (TestEst$X - Xz)/(TestEst$N - Nz)
    sigma2z <- (1/TestEst$N) * (somatorio - 2*Xz*miz + Nz*(miz^2) + (TestEst$Qgeral - somatorio) - 2*(TestEst$X - Xz)*lambdaz + (TestEst$N - Nz)*(lambdaz^2))
    estatistica <- (- TestEst$N*log(sqrt(sigma2z)))
    
    pos[k+1] <- estatistica
    resultados <- rbind(resultados,pos)
  
  }
  
  resultados <- resultados[order(resultados[k+1],decreasing = TRUE),]
  
  return(resultados)
  
}

geradorClusterSimul <- function(TestEst,dados,clusters,k){
  
  for (i in 1:nrow(clusters)) {
    estatistica <- 0
    sigma2z <- 0
    somatorio <- 0 #Somatorio do quadrado da variavel diferenca de consumo dentro do cluster
    Xz <- 0 #Somatorio da variavel diferenca de consumo dentro do cluster
    
    pos <- as.numeric(clusters[i,1:k])
    somatorio <- sum(dados[pos,4]^2)
    Xz <- sum(dados[pos,4])
    
    Nz <- k
    miz <- Xz/Nz
    lambdaz <- (TestEst$X - Xz)/(TestEst$N - Nz)
    sigma2z <- (1/TestEst$N) * (somatorio - 2*Xz*miz + Nz*(miz^2) + (TestEst$Qgeral - somatorio) - 2*(TestEst$X - Xz)*lambdaz + (TestEst$N - Nz)*(lambdaz^2))
    estatistica <- (- TestEst$N*log(sqrt(sigma2z)))
    
    clusters[i,k+1] <- estatistica
    
  }
  
  clusters <- clusters[order(clusters[k+1],decreasing = TRUE),]
  
  return(clusters[1,k+1])
  
}
  

monteCarloSimu <- function(TestEstatistic, diffCemigData, clusters, k, bound){
  
  resultSimul <- vector()
  
  for (m in 1:bound) {
    baseRandom <- diffCemigData
    baseRandom[,4] <- sample(diffCemigData$difConsumo) #randomizando a coluna diferenca de consumo
    resultSimul[m] <- geradorClusterSimul(TestEstatistic, baseRandom, clusters, k)
  }
  return (resultSimul)
}


clustersSignificativos <- function(resultSimul,clusters,k){

  alpha <- 0.05
  resultSimul <- resultSimul[order(resultSimul,decreasing = TRUE)] #ordenando ele do maior para o menor
  significativos <- vector()
  
  #abaixo preciso ver quantas observacoes na simulacao sao maiores que o valor realmente observado
  for(i in 1:nrow(clusters)){
    pvalor <- 0
    
    pvalor <- sum(resultSimul > clusters[i,k+1])/(length(resultSimul)+1)
    
    if(pvalor < alpha){
      significativos[length(significativos)+1] <- clusters[i,1] #salvando o centroide daquele cluster
    }
    else{
      #se eu cair aqui, entendo que nao vai ter mais nenhum significativo
      break()
    }
  }
}

######################### CALCULO POR RAIO #####################################


geradorClusterPorRaio <- function(TestEst,matrizDistancia,diffCemigData,raio){
  
  resultados <- list()
  
  for (i in 1:nrow(diffCemigData)){
    resultadosParciais <- vector()
    df_aux <- matrizDistancia[matrizDistancia$from == i, ]
    pos <- df_aux[(df_aux$distance < raio),2] # alimentadoras mais prox
    resultadosParciais <- pos
    
    somatorio <- sum(diffCemigData[pos,4]^2)
    Xz <- sum(diffCemigData[pos,4])
    j <- sum((df_aux$distance < raio))
    Nz <- j
    miz <- Xz/Nz
    lambdaz <- (TestEst$X - Xz)/(TestEst$N - Nz)
    sigma2z <- 1/TestEst$N * (somatorio - 2*Xz*miz + Nz*miz^2 + (TestEst$Qgeral - somatorio) - 2*(TestEst$X - Xz)*lambdaz + (TestEst$N - Nz)*((lambdaz)^2))
    estatistica <- (- TestEst$N*log(sqrt(sigma2z)))
    
    resultadosParciais[j+ 1] <- estatistica
    resultados[[i]] <- resultadosParciais
  }
    return(resultados)
}
 
monteCarloSimuRaio <- function(TestEst,diffCemigData,clustersRaio,raio,bound){
  
  resultSimul <- vector()
  
  for (m in 1:bound) {
    baseRandom <- diffCemigData
    baseRandom[,4] <- sample(diffCemigData$difConsumo) #randomizando a coluna diferenca de consumo
    resultSimul[m] <- geradorClusterRaioSimul(TestEst,baseRandom,clustersRaio,raio)
  }
  return (resultSimul)
}

geradorClusterRaioSimul <- function(TestEst,baseRandom,clustersRaio,raio){
  
  #Somatorio do quadrado da variavel diferenca de consumo dentro do cluster
  # Xz Somatorio da variavel diferenca de consumo dentro do 
  # a diferença de consumo é alterada mas não a distancia 
  bestResult <- 0
  estatisticaAux <- 0

  for (i in 1:nrow(baseRandom)) {
    j <- length(clustersRaio[[i]]) - 1 # numero de elemento no cluster menos a estatistica de teste que esta salva na ultima casa

    pos <- as.numeric(clustersRaio[[i]][1:j])
    
    somatorio <- sum(baseRandom[pos,4]^2)
    Xz <- sum(baseRandom[pos,4])

    Nz <- j
    miz <- Xz/Nz
    lambdaz <- (TestEst$X - Xz)/(TestEst$N - Nz)
    sigma2z <- 1/TestEst$N * (  somatorio - 2*Xz*miz + Nz*miz^2 + (TestEst$Qgeral - somatorio) - 2*(TestEst$X - Xz)*lambdaz + (TestEst$N - Nz)*((lambdaz)^2))
    
    estatisticaAux <- (- TestEst$N*log(sqrt(sigma2z)))
    
    if(is.null(estatisticaAux)){
      estatisticaAux = 0
    }
    else if(bestResult == 0 || estatisticaAux > bestResult){
      bestResult <- estatisticaAux
    }
  }
  return(bestResult)
}

# ctrl + shift + c
# 
# clustersSignificativosRaio <- function(resultSimul,clusters,k){
#   
#   alpha <- 0.05
#   resultSimul <- resultSimul[order(resultSimul,decreasing = TRUE)] #ordenando ele do maior para o menor
#   significativos <- vector()
#   
#   #abaixo preciso ver quantas observacoes na simulacao sao maiores que o valor realmente observado
#   for(i in 1:nrow(clusters)){
#     pvalor <- 0
#     
#     pvalor <- sum(resultSimul > clusters[i,k+1])/(length(resultSimul)+1)
#     
#     if(pvalor < alpha){
#       significativos[length(significativos)+1] <- clusters[i,1] #salvando o centroide daquele cluster
#     }
#     else{
#       #se eu cair aqui, entendo que nao vai ter mais nenhum significativo
#       break()
#     }
#   }
# }
