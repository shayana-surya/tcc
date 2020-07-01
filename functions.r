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
  matrizDistancia <- dist.mat(diffCemigData,"ID","y","x",diffCemigData,"ID","y","x")
  matrizDistancia <- matrizDistancia[order( matrizDistancia$from,matrizDistancia$distance),]
  matrizDistancia$from <- as.numeric(as.character(matrizDistancia$from))
  matrizDistancia$to <- as.numeric(as.character(matrizDistancia$to))
  
  return (matrizDistancia)
}

EstatisticTestElementsCalculator <- function(centroides,diffCemigData) {
  TestEstatistic <- data.frame(N = length(centroides), #Equivale ao total geral de observacoes
                                X = 0, #Somatorio da variavel diferenca de consumo
                                Qgeral = 0 #Somatorio do quadrado da variavel diferenca de consumo
  )
  
  for (e in 1:TestEstatistic$N) {
    TestEstatistic$X <- TestEstatistic$X + diffCemigData[e,4]
    TestEstatistic$Qgeral <- TestEstatistic$Qgeral + (diffCemigData[e,4])^2
  }
  
  return(TestEstatistic)
}


geradorCluster <- function(TestEst,isSimul,matrizDistancia,dados,k){
  resultados <- data.frame()
  
  for (i in 1:nrow(dados)) {
    estatistica <- 0
    sigma2z <- 0
    somatorio <- 0 #Somatorio do quadrado da variavel diferenca de consumo dentro do cluster
    Xz <- 0 #Somatorio da variavel diferenca de consumo dentro do cluster
    df_aux <- matrizDistancia[matrizDistancia$from == i,]

    #resultados[i,] <- df_aux[pos, 2]
    #somatorio <- sum(dados[pos,4]^2)
    #Xz <- sum(dados[pos,4])
    
    for (j in 1:k) {
      resultados[i,j] <- df_aux[j, 2]
      somatorio <- somatorio + (dados[resultados[i,j],4])^2
      Xz <- Xz + dados[resultados[i,j],4]
    }
    
    Nz <- k
    miz <- Xz/Nz
    lambdaz <- (TestEst$X - Xz)/(TestEst$N - Nz)
    sigma2z <- (1/TestEst$N) * (somatorio - 2*Xz*miz + Nz*(miz^2) + (TestEst$Qgeral - somatorio) - 2*(TestEst$X - Xz)*lambdaz + (TestEst$N - Nz)*(lambdaz^2))
    estatistica <- (- TestEst$N*log(sqrt(sigma2z)))
    
    resultados[i,k+1] <- estatistica
  
  }
  
  resultados <- resultados[order(resultados[k+1],decreasing = TRUE),]
 
  
  if(isSimul== FALSE){
    return(resultados)
  }
  else{
    return(resultados[1,k+1])
  }
  
}
  

monteCarloSimu <- function(TestEstatistic,matrizDistancia,diffCemigData,k,bound){
  
  resultSimul <- vector()
  
  for (m in 1:bound) {
    baseRandom <- diffCemigData
    baseRandom[,4] <- sample(diffCemigData$difConsumo) #randomizando a coluna diferenca de consumo
    resultSimul[m] <- geradorCluster(TestEstatistic,TRUE,matrizDistancia,baseRandom,k)
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




geradorClusterPorRaio <- function(TestEst,isSimul,matrizDistancia,dados,raio){
  
  resultados <- list()
  
  for (i in 1:nrow(dados)){

    resultadosParciais <- vector()
    df_aux <- matrizDistancia[matrizDistancia$from == centroides[i], ]
    pos <- (df_aux$distance < raio)
    resultadosParciais <- df_aux[pos, 2]
    somatorio <- sum(dados[pos,4]^2)
    Xz <- sum(dados[pos,4])
    j <- sum(pos)
    Nz <- j 
    miz <- Xz/Nz
    lambdaz <- (TestEst$X - Xz)/(TestEst$N - Nz)
    sigma2z <- (1/TestEst$N) * (somatorio - 2*Xz*miz + Nz*(miz^2) + (TestEst$Qgeral - somatorio) - 2*(TestEst$X - Xz)*lambdaz + (TestEst$N - Nz)*(lambdaz^2))
    estatistica <-(- TestEst$N*log(sqrt(sigma2z)))
    resultadosParciais[j+ 1] <- estatistica
    resultados[[i]] <- resultadosParciais
  }
  
  
  if(isSimul== FALSE){
    return(resultados)
  }
  else{
    return(resultados[1,k+1])
  }
  
}
