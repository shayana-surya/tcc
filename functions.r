
createData <- function(joinDados){
  
  relacaoAlimentadorId <- data.table(ID = numeric(),
                                     alimentadora = character()
  )
  
  diffCemigData <- data.table(alimentadora = character(),
                              ID = numeric(),
                              x = numeric(),
                              y = numeric(),
                              difConsumo = numeric()
  )
  
  excluidas <- data.table(alimentadora = character(),
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
      (!is.na(joinDados$y.x[i]) || !is.na(joinDados$y.y[i])))
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
        ID = i,
        alimentadora = joinDados$alimentador[i],
        x = validX,
        y = validY,
        difConsumo = joinDados$ConsumoMedido.y[i] - joinDados$ConsumoMedido.x[i]
        
      ))
    }
    else
    {
      excluidas <- rbind(excluidas, data.frame(
        alimentadora = joinDados$alimentador[i],
        dados2013.x = joinDados$x.x[i],
        dados2013.y = joinDados$y.x[i],
        dados2018.x = joinDados$x.y[i],
        dados2018.y = joinDados$y.y[i],
        dados2013.consumo = joinDados$ConsumoMedido.x[i],
        dados2018.consumo = joinDados$ConsumoMedido.y[i]
      ),fill=TRUE)
    }
  }
  
  relacaoAlimentadorId <-diffCemigData[,c("ID","alimentadora")]
  diffCemigData <- diffCemigData[,2:5]
  list_data <- list(diffCemigData,excluidas,relacaoAlimentadorId)
  return (list_data);
}

################## DIST ###########################

createEuclideanDistance <- function(diffCemigData) {
  
  matrizDistancia <- as.matrix( dist(diffCemigData[,c("x","y")], method = "euclidean"))

  return (matrizDistancia)
}
####################################################


################## Rcpp ############################
createEuclideanDistanceUsingRcpp <- function(diffCemigData) {
  
  matrizDistancia <- createEuclideanDistanceRcpp(as.matrix(diffCemigData),nrow(diffCemigData))

  return (matrizDistancia)
}
####################################################


################## Matriz de Indice #################
createIndexMatrix <- function(matrizDistancia) {
  #obs: A função order não retorna o vetor original ordenado, mas retorna um vetor com as posições para que x fique em ordem crescente.
  
  matIDX <- matrix(NA, nrow(matrizDistancia), ncol(matrizDistancia))
  for(idx in 1:nrow(matrizDistancia)){
    matIDX[idx,] <- order(matrizDistancia[idx,], decreasing=FALSE)
  }
  return (matIDX)
}
#####################################################


######################### CALCULO POR QUANTIDADE K DE CLUSTERS  #####################################

geradorCluster <- function(IndexMatrix, dados, k){
  resultados <- data.frame()
  N = nrow(dados)
  
  for (i in 1:N) {
    estatistica <- 0
    sigma2z <- 0
    somatorio <- 0 #Somatorio do quadrado da variavel diferenca de consumo dentro do cluster
    Xz <- 0 #Somatorio da variavel diferenca de consumo dentro do cluster
    
    #df_aux <- matrizDistancia[matrizDistancia$from == i,]
    #pos <- df_aux[1:k, 2]
    
    pos <- IndexMatrix[i,] #Pego toda a linha da matriz de distancia ID
    x.in  <- dados[pos[1:k],4]
    x.out <- dados[pos[(k+1):N],4]
    
    somatorioIn <- sum(x.in^2)
    somatorioOut <- sum(x.out^2)
    Xz <- sum(x.in)
    X <- sum(x.out)
    
    Nz <- k
    miz <- Xz/Nz
    lambdaz <- (X - Xz)/(N - Nz)
    sigma2z <- (1/N) * (somatorioIn - 2*Xz*miz + Nz*(miz^2) + (somatorioOut - somatorioIn) - 2*(X - Xz)*lambdaz + (N - Nz)*(lambdaz^2))
    estatistica <- (- N*log(sqrt(sigma2z)))
    
    relacao <- pos[1:k]
    relacao[k+1] <- estatistica
    resultados <- rbind(resultados,relacao)
    
  }
  
  resultados <- resultados[order(resultados[k+1],decreasing = TRUE),]
  
  return(resultados)
  
}

geradorClusterSimul <- function(IndexMatrix,dados,clusters,k){
  
  N = nrow(dados)
  
  for (i in 1:N) {
    estatistica <- 0
    sigma2z <- 0
    somatorio <- 0 #Somatorio do quadrado da variavel diferenca de consumo dentro do cluster
    Xz <- 0 #Somatorio da variavel diferenca de consumo dentro do cluster
    
    pos <- IndexMatrix[i,] #Pego toda a linha da matriz de distancia ID
    x.in  <- dados[pos[1:k],4]
    x.out <- dados[pos[(k+1):N],4]
    
    somatorioIn <- sum(x.in^2)
    somatorioOut <- sum(x.out^2)
    Xz <- sum(x.in)
    X <- sum(x.out)
    
    Nz <- k
    miz <- Xz/Nz
    lambdaz <- (X - Xz)/(N - Nz)
    sigma2z <- (1/N) * (somatorioIn - 2*Xz*miz + Nz*(miz^2) + (somatorioOut - somatorioIn) - 2*(X - Xz)*lambdaz + (N - Nz)*(lambdaz^2))
    estatistica <- (- N*log(sqrt(sigma2z)))
    
    clusters[i,k+1] <- estatistica
    
  }
  
  clusters <- clusters[order(clusters[k+1],decreasing = TRUE),]
  
  return(clusters[1,k+1])
  
}


monteCarloSimu <- function(IndexMatrix,diffCemigData, clusters, k, bound){
  
  resultSimul <- vector()
  
  for (m in 1:bound) {
    baseRandom <- diffCemigData
    baseRandom[,4] <- sample(diffCemigData$difConsumo) #randomizando a coluna diferenca de consumo
    resultSimul[m] <- geradorClusterSimul(IndexMatrix, baseRandom, clusters, k)
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
      return(significativos)
    }
  }
  return(significativos)
}

histograma <- function(resultsimul,significativos){
  
  hist(resultSimul, xlim=c(min(c(resultSimul,significativos)), max(c(resultSimul,significativos))), col="light blue") 
  rug(resultSimul)
  abline(v=significativos, lwd=2, lty=2, col="red")
  
}

######################### CALCULO POR RAIO #####################################


geradorClusterPorRaio <- function(matrizDistancia,diffCemigData,raio){
  
  N = nrow(diffCemigData)
  X = sum(diffCemigData$difConsumo)
  bestResult <- 0
  estatisticaAux <- 0
  position <- 0
    
  resultados <- list()
  
  for (i in 1:nrow(diffCemigData)){
    pos <- (matrizDistancia[i, ] < raio)
    resultadosParciais <- which(pos == TRUE) # alimentadoras mais prox
    # pergunta: eu n tenho os alimentadores em ordem; faz diferença?
    
    x.in   <- diffCemigData$difConsumo[pos]
    x.out  <- X - x.in
    
    sigma2z <- sum( (x.in - mean(x.in))^2 ) + sum( (x.out - mean(x.out))^2 )
    sigma2z <- sigma2z/N
    estatistica <- (-N*log(sigma2z)/2)

    j <- sum(pos)
    resultadosParciais[j + 1] <- estatistica
    resultados[[i]] <- resultadosParciais
    
    if(bestResult == 0 || estatistica > bestResult){
      bestResult <- estatisticaAux
      position <- i
    }
    resultados[[i+1]] <- c(position,bestResult)
  }
    return(resultados)
}
 
monteCarloSimuRaio <- function(diffCemigData,clustersRaio,raio,bound){
  
  resultSimul <- vector()
  
  for (m in 1:bound) {
    baseRandom <- diffCemigData
    baseRandom[,4] <- sample(diffCemigData$difConsumo) #randomizando a coluna diferenca de consumo
    resultSimul[m] <- geradorClusterRaioSimul(baseRandom,clustersRaio,raio)
  }
  return (resultSimul)
}

geradorClusterRaioSimul <- function(baseRandom,clustersRaio,raio){
  
  bestResult <- 0
  estatisticaAux <- 0
  N = nrow(baseRandom)
  X = sum(baseRandom$difConsumo)

  for (i in 1:nrow(baseRandom)) {
    j <- length(clustersRaio[[i]]) - 1 # numero de elemento no cluster menos a estatistica de teste que esta salva na ultima casa

    pos <- clustersRaio[[i]][1:j]
    
    x.in   <- baseRandom$difConsumo[pos]
    x.out  <- X - x.in
    
    sigma2z <- sum( (x.in - mean(x.in))^2 ) + sum( (x.out - mean(x.out))^2 )
    sigma2z <- sigma2z/N
    
    estatisticaAux <- (-N*log(sigma2z)/2)
    
    if(bestResult == 0 || estatisticaAux > bestResult){
      bestResult <- estatisticaAux
    }
  }
  return(bestResult)
}

# ctrl + shift + c

clustersSignificativosRaio <- function(resultSimul,clustersRaio,raio){

  alpha <- 0.05
  resultSimul <- sort(resultSimul,decreasing = TRUE) #ordenando ele do maior para o menor
  significativosRaio <- vector()

  #abaixo preciso ver quantas observacoes na simulacao sao maiores que o valor realmente observado
  for(i in 1:length(clustersRaio)){
    pos <- length(clustersRaio[[i]])
    pvalor <- (sum(resultSimul > clustersRaio[[i]][pos]) + 1)/(length(resultSimul)+1)

    if(pvalor < alpha){
      significativosRaio <- append(significativosRaio,i) #salvando o centroide daquele cluster
    }
  }
  return (significativosRaio)
}
