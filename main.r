### ---
### title: "tcc"
### author: "Shayana e Rafael"
### date: "6 de novembro de 2019"
### output: html_document
### ---
library(readxl)
library(data.table)
library(REAT)

dados2018 <- read_excel("DadosAlimentadoresCEMIG_XY.xlsx", sheet = 2)
dados2013 <- read_excel("DadosAlimentadoresCEMIG_XY.xlsx", sheet = 1)

joinDados <- merge(x = dados2013, y = dados2018, by="alimentador", all=TRUE)

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

count <- 0

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


matrizDistancia <- dist.mat(diffCemigData,"alimentador","y","x",diffCemigData,"alimentador","y","x")
matrizDistancia <- matrizDistancia[order(matrizDistancia$from,matrizDistancia$distance),]

centroides <- diffCemigData$alimentador
clusters <- data.frame()
df_aux <- data.frame()
resultados <- data.frame()


N <- nrow(diffCemigData)
desvioPadrao <- sd(diffCemigData$difConsumo)
variancia <- desvioPadrao^2
media <- mean(diffCemigData$difConsumo)
X <- 0
somatorioSegTermo <- 0
for (i in 1:nrow(diffCemigData))
{
  somatorioSegTermo <- somatorioSegTermo + ((diffCemigData$difConsumo[i]-media)^2)/(2*variancia)
  X <- X + diffCemigData$difConsumo[i];
}

estTesteParteFixa <- N*log(desvioPadrao)+somatorioSegTermo-N/2

k <- 10
i <- 1
Nz <- k

while (i <= length(centroides)) {
  
  somatorioEmZ <- 0
  somatorioForaZ <- 0
  varianciaZ <- 0
  Uz <- 0
  lambda <- 0
  Xz <- 0
  lambda <- 0
  resultados <- data.frame()
  df_aux <- matrizDistancia[matrizDistancia$from == centroides[i], ]
  
  for (j in 1:nrow(diffCemigData)) {
    if (j <= k){
      
      resultados[i,j] <- as.character(df_aux[j, 2])
      Xz <- as.numeric(Xz) + as.numeric(diffCemigData[diffCemigData$alimentador==resultados[i,j],4])
      somatorioEmZ <- somatorioEmZ + as.numeric((diffCemigData[diffCemigData$alimentador == resultados[i,j],4])^2)
    }
    else{
      aux <- as.character(df_aux[j, 2])
      somatorioForaZ <- somatorioForaZ + as.numeric((diffCemigData[diffCemigData$alimentador == aux,4])^2)
    }
  }
  
  Uz <- as.numeric(Xz/Nz)
  lambda <- (X-Xz)/(N-Nz)
  varianciaZ <- 1/N *(somatorioEmZ - 2*Xz*Uz + Nz*Uz^2 + somatorioForaZ - 2*(X-Xz)*lambda + (N - Nz)*lambda^2)
  resultados[i,k+1] <- estTesteParteFixa - N*log(sqrt(varianciaZ))
  
  clusters <- rbind(clusters,resultados)
  i <- i+1
}
clusters <- na.omit(clusters)
