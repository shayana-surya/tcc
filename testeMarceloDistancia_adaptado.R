## Implementacao rapida Marcelo
## 08/07/2020

 rm(list=ls(all=TRUE))

 require(openxlsx)
 require(packHV)

 #dados <- read.xlsx("DadosAlimentadoresCEMIG_XY.xlsx")
 #dados <- na.omit(dados)
 #rownames(dados) <- 1:nrow(dados)
 #hist_boxplot(dados$ConsumoMedido, col="light blue", main="")
 
 require(readr)
 dados <- read_csv("DiferencaConsumo.csv")
 hist_boxplot(dados$difConsumo, col="light blue", main="")

 # Matriz de distancia (em metros)
   matDist <- as.matrix( dist(dados[,c("x","y")], method = "euclidean") )

   ## Raio maximo para o cluster
   Raio  <- 300000 ## Raio em metros
   
   ## Manipulacao da matriz de Distancias
   ## Se o raio eh grande recebe N
   matDist[matDist > Raio] <- NA

 # Matriz de indices
   matIDX <- matrix(NA, nrow(matDist), ncol(matDist))
   for(idx in 1:nrow(matDist)){
      elementos    <- as.numeric( names(sort(matDist[idx,])) )
      matIDX[idx,1:length(elementos)] <- elementos
   }

   rm(matDist) ## Apaga da memoria a matriz de distancias

 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # Varredura sobre TODOS os centroides com cluster de raio "Raio"
   N <- nrow(matIDX)
   dados$LLK <- rep(NA, N) 

   for(idx in 1:nrow(matIDX)){
      pos.in  <- matIDX[idx,][!is.na(matIDX[idx,])] # Somente os que nao sao "NA"
      pos.out <- (1:nrow(matIDX))[ !((1:nrow(matIDX)) %in% pos.in) ]
      
      x.in   <- dados$difConsumo[pos.in]
      x.out  <- dados$difConsumo[pos.out]

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
     consumo <- sample(dados$difConsumo, N)
     vLLK    <- rep(NA, N)

     for(idx in 1:nrow(matIDX)){
        pos.in  <- matIDX[idx,][!is.na(matIDX[idx,])] # Somente os que nao sao "NA"
        pos.out <- (1:nrow(matIDX))[ !((1:nrow(matIDX)) %in% pos.in) ]
        x.in   <- consumo[pos.in]
        x.out  <- consumo[pos.out]

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
   (pvalor <- (sum(vec.LLK > BestLLK) + 1)/(nsim + 1))

