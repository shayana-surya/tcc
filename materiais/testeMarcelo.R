## Implementacao rapida Marcelo
## 08/07/2020

 rm(list=ls(all=TRUE))

 require(openxlsx)

 dados <- read.xlsx("DadosAlimentadoresCEMIG_XY.xlsx")
 dados <- na.omit(dados)
 hist(dados$ConsumoMedido)

 # Matriz de distancia
   matDist <- as.matrix( dist(dados[,c("x","y")], method = "euclidean") )

 # Matriz de indices
   matIDX <- matrix(NA, nrow(matDist), ncol(matDist))
   for(idx in 1:nrow(matDist)){
      matIDX[idx,] <- order(matDist[idx,], decreasing=FALSE)
   }

   rm(matDist) ## Apaga da memoria a matriz de distancias

 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # Varredura sobre TODOS os centroides com cluster de tamanho "k"
   k <- 350
   N <- nrow(matIDX)
   dados$LLK <- rep(NA, N) 

   for(idx in 1:nrow(matIDX)){
      pos    <- matIDX[idx,]
      x.in   <- dados$ConsumoMedido[pos[1:k]]
      x.out  <- dados$ConsumoMedido[pos[(k+1):N]]

      sigma2 <- sum( (x.in - mean(x.in))^2 ) + sum( (x.out - mean(x.out))^2 )
      sigma2 <- sigma2/N

      dados$LLK[idx] <- -N*log(sigma2)/2
   }
   BestIDX <- which.max(dados$LLK)
   BestLLK <- max(dados$LLK)

 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # SIMULACAO DE MONTE CARLO
   nsim    <- 499 ## Numero de simulacoes
   vec.LLK <- rep(NA, nsim)

   for(s in 1:nsim){
     consumo <- sample(dados$ConsumoMedido, N)
     vLLK    <- rep(NA, N)

     for(idx in 1:nrow(matIDX)){
        pos    <- matIDX[idx,]
        x.in   <- consumo[pos[1:k]]
        x.out  <- consumo[pos[(k+1):N]]

        sigma2 <- sum( (x.in - mean(x.in))^2 ) + sum( (x.out - mean(x.out))^2 )
        sigma2 <- sigma2/N

        vLLK[idx] <- -N*log(sigma2)/2
     }
     vec.LLK[s] <- max(vLLK)
   }

   ## Calcula o p-valor
   hist(vec.LLK, xlim=c(min(c(vec.LLK,BestLLK)), max(c(vec.LLK,BestLLK))), col="light blue") 
   rug(vec.LLK)
   abline(v=BestLLK, lwd=2, lty=2, col="red")
   pvalor <- (sum(vec.LLK > BestLLK) + 1)/(nsim + 1)

