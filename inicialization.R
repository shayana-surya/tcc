### ---
### title: "tcc"
### author: "Shayana e Rafael"
### date: "6 de novembro de 2019"
### ---

require(readxl)
require(data.table)
require(REAT)
require(Rcpp)
require(rlist)
library(shiny)

source(file="functions.r")
sourceCpp('functionsC++.cpp')

###################### LEITURA E TRATAMENTO DA BASE DE DADOS ###################### 

dados2018 <- read_excel("DadosAlimentadoresCEMIG_XY.xlsx", sheet = 2)
dados2013 <- read_excel("DadosAlimentadoresCEMIG_XY.xlsx", sheet = 1)

joinData <- merge(x = dados2013, y = dados2018, by="alimentador", all=TRUE)
names(joinData)<- c("alimentador", "lat.2013", "lon.2013","x.2013", "y.2013","ConsumoMedido.2013",
                    "lat.2018", "lon.2018","x.2018", "y.2018","ConsumoMedido.2018")	

list_data <- createData(joinData)
diffCemigData <- data.frame(list_data[1])
excluded <- data.frame(list_data[2])
relacaoAlimentadorId <- data.frame(list_data[3])