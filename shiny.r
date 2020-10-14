library(leaflet)
library(ggplot2)
library(rgdal)
library(DT)
library(shinycssloaders)
library(shinybusy)

source('inicialization.r')

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                      h4{
                        text-align: center;
                      }
                      
                      p{
                      color: #00a651;
                      text-align: justify;
                       t-size 30px;
                      }
                      
                      body{
                      background-color:#ecfff5;
                      color: #00a651;
                      }
                      
                  ")
          )
  ),
  navbarPage(title = tags$img(src="https://seeklogo.com/images/C/CEMIG-logo-42E5BC7D36-seeklogo.com.png", height = 30, width = 90),
             ######### PRIMEIRA ABA ######### 
             tabPanel("Sobre o trabalho",
                      mainPanel(
                        tags$h4("Análise comparativa de eficiência computacional entre duas linguagens de programação na implementação da estatística de varredura espacial - Scan"),
                        HTML("
                          <p> Um dos principais métodos utilizados para detecção de conglomerados espaciais é o método Scan. Com ele, é possível identificar regiões onde a incidência média do evento de interesse é significativamente maior ou menor que as demais regiões. </p>
                          <p> Para identificar tais regiões, disponibilizamos uma interface amigável que permite compreender a natureza dos dados analisados, assim como os resultados gerados a partir da aplicação do Scan em diferentes parâmetros, como por exemplo: 
                              <ul>
                                <li> Linguagem de programação;</li>
                                <li> Dados de entrada;</li>
                                <li> Janela de varredura;</li>
                                <li> Número de simulações;</li>
                              </ul>  
                          </p>
                          <p>A técnica foi aplicada em dados do mercado de energia de uma concessionária do estado de Minas Gerais. Os dados representam o consumo de energia de alimentadores (também conhecidos como religadores), que são disjuntores de 13,8 kV instalados em subestações de energia elétrica. Para cada equipamento há o consumo de energia dos anos de 2013 e 2018, bem como sua latitude e longitude. </p>

                             ")
                      )
                      
                      ),
             ######### SEGUNDA ABA ######### 
             tabPanel("Dados",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("var","1. Vizualização dos dados de entrada", choice = c("dados 2013" = 1, "dados 2018" =2, "Diferença de consumo entre os anos" = 3, "Dados não utilizados" = 4),selected = 3)
                  ),
                  mainPanel(
                    dataTableOutput("data")
                  )
                )
             ),
             ######### TERCEIRA ABA ######### 
             tabPanel("EDA",
                      sidebarLayout(
                        sidebarPanel(
                            selectInput("EDA2","1. Análise exploratória", choice = c("dados 2013" = 1, "dados 2018" =2, "Alimentadoras em comum (diferença de consumo entre anos)"=3, "Alimentadoras desconsideradas"=4),selected = 1),
                            conditionalPanel(
                              condition = "input.EDA2 != 4",
                              plotOutput("histPlot") %>% withSpinner(color="#0dc5c1"),
                              h4("Sumário"),
                              verbatimTextOutput("sum"),
                              plotOutput("boxsplot") %>% withSpinner(color="#0dc5c1")
                            )
                            , height=1),
                        mainPanel(
                           leafletOutput("plot2",height = "90vh") %>% withSpinner(color="#0dc5c1")
                        )
                      )
                  ),
             ######### QUARTA ABA ######### 
             tabPanel("Parâmetros de Entrada",
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Selecione os dados de entrada:"),
                          selectInput("flag","1. Dados a serem utilizado:", choice = c("Consumo de 2013" = 1, "Consumo de 2018" =2, "Diferença entre os anos" = 3),selected = 3),
                          radioButtons("codigo", "2. Algoritmo a ser utilizado:",
                                       c("R" = 1,
                                         "C++" = 2),inline = TRUE),
                          radioButtons("window", "3. Janela de varredura:",
                                       c("Por Raio" = 1,
                                         "Por K elementos" = 2),inline = TRUE),
                          conditionalPanel(
                            condition = "input.window == 1",
                            selectInput("raio","4. Tamanho do raio:", choice = c("30 km" = 30000, "60 km" = 60000, "80 km" = 80000,"100 km" = 100000,"150 km" = 150000),selected = 80000),
                          ),
                          conditionalPanel(
                            condition = "input.window == 2",
                            selectInput("k","4. Número K de elementos:", choice = c("100" = 100,"150" =150, "250" =250, "300" = 300),selected = 250),
                          ),
                          selectInput("alpha","5. Nível de Significância:", choice = c("1%" = 0.01, "5%" =0.05, "10%" = 0.1),selected = 0.05),
                          selectInput("simulacao","6. Número de simulações:", choice = c("99" = 99, "499" =499, "999" = 999),selected = 99),
                          actionButton("submit", "Submit")
                        ),
                      mainPanel(
                        tags$h4("Informações gerais"),
                        HTML("
                          <p> A partir dos dados selecionados é possível calcular, em tempo real, os clusters significativos e o tempo de execução das funções mais relevantes. Os resultados serão disponibilizado na Aba <b>'Análise de Resultados'</b></p>
                          <p><b> 1. Dados a serem utilizado:</b> Aqui deve ser selecionado os dados de entrada que deseja analisar, estes podem ser os dados de consumo de 2013, consumo de 2018 ou a diferença de consumo entre os dois anos</p>
                          <p><b> 2. Algoritmo a ser utilizado:</b> Para analisar o tempo de execução, disponibilizamos o código em duas linguagens diferentes: o R e o C++ (Rcpp). Por ser uma linguagem de mais baixo nível, é esperado que o tempo de execução seja menor no algoritmo em C++. Vale ressaltar que não são todas as funções que foram isoladas em C++ e sim aquelas que consideramos as mais importantes: o cálculo de distância, a geração de clusters e a simulação de Monte Carlo. </p>
                          <p><b> 3. Janela de varredura:</b> Duas diferentes abordagens foram disponibilizadas; a primeira utiliza um raio fixo e cada cluster contem números de alimentadores diferentes desde que a distância entre eles seja menor ou igual ao raio. Já utilizando K, limitados o número de alimentadores dentro do cluster, isso significa que surgirão clusters de raios diferentes porém com número de elementos iguais.</p>
                          <p><b> 4. Tamanho do raio/Número K de elementos:</b> Caso opte por realizar o cálculo por Raio, o tamanho do raio poderá ser selecionado no item 4; porém se deseja utilizar K a opção disponibilizará a seleção do número de elementos K dentro do cluster.</p>
                          <p><b> 5. Nível de Significância:</b> Os níveis de significância mais comuns foram disponibilizados, sendo eles 1%,5% e 10%. Este valor está associado com o nível de confiança de nosso teste; A um alfa de 10% temos um nível de 90% de confiança; para 5%, 95% de confiança e assim sucessivamente. </p>
                          <p><b> 6. Número de simulações:</b> Por fim, disponibilizamos o número de simulações, este parâmetro impacta diretamente no tempo de execução do programa. Quanto maior o número de simulações, mais confiável será o resultado gerado, porém,levará mais tempo para os resultados serem gerados.</p>
                          <p> Para executar o programa, após escolher os parâmetros desejados, submeta os dados e aguarde até a execução do programa ser finalizada.</p>
                             ")
                        )
                      )
                     ),
             ######### QUINTA ABA ######### 
                     tabPanel("Análise de Resultados",
                              sidebarLayout(
                                sidebarPanel(
                                  htmlOutput("option"),
                                  htmlOutput("time"),
                                  htmlOutput("sig"),
                                ),
                                mainPanel(
                                leafletOutput("mapa",height = "90vh") %>% withSpinner(color="#0dc5c1")
                              )
                              )
                        )
                     
            
      )
)

server <- function(input,output,session) {
  
  ######### SEGUNDA ABA ##################
  output$data <- renderDataTable({
    if(input$var== 1){
      datatable(dados2013, class = 'tableSetup')}
    else if(input$var == 2){
      datatable(dados2018, class = 'tableSetup')}
    else if(input$var == 3){
      datatable(diffCemigData, class = 'tableSetup')}
    else if(input$var == 4){
      datatable(excluded[,c("alimentadora","dados2013.x","dados2013.y","dados2018.x","dados2018.y","dados2013.consumo","dados2018.consumo")], class = 'tableSetup')}
  })


  ######### TERCEIRA ABA ##################  

  output$histPlot <- renderPlot(showHist(input$EDA2,dados2013,dados2018,diffCemigData))
  output$plot2 <- renderLeaflet({showMapInfo(input$EDA2,list_data,relacaoAlimentadorId)})
  output$sum <- renderPrint({showSummary(input$EDA2,dados2013,dados2018,diffCemigData)})
  output$boxsplot <- renderPlot(boxsplotFunction(diffCemigData,input$EDA2))

  
  ######### QUARTA ABA ##################
  
  action <- reactiveValues(data = NULL)
  
  observeEvent(input$submit, {
    show_modal_spinner(spin = "fading-circle",
                       text = "Por favor, aguarde...")
    if(input$window == 1)
    {
      if(input$codigo == 1)
        action$list_data <- calculator_R(input$raio,input$simulacao,diffCemigData,input$alpha,input$flag)
      
      else
        action$list_data <- calculator_Rcpp(input$raio,input$simulacao,diffCemigData,input$alpha,input$flag)
      
    }
    else
    {
      if(input$codigo == 1)
        action$list_data <-calculator_K_R(input$k,input$simulacao,diffCemigData,input$alpha,input$flag)
      
      else
        action$list_data <-calculator_K_Rcpp(input$k,input$simulacao,diffCemigData,input$alpha,input$flag)
    }
    remove_modal_spinner()
  })

    
  ######### QUINTA ABA ##################
  
  output$option1 <- renderText({
    if (is.null(action$list_data)) return()
    if (input$flag == 1)
      "Dados de entrada: Consumo de 2013;"
    else if (input$flag == 2)
      "Dados de entrada: Consumo de 2018;"
    else if (input$flag == 2)
      "Dados de entrada: Diferença de consumo entre os anos;"
  })

  data <- function()({
    if (input$flag == 1)
      return("2013")
    else if(input$flag == 2)
      return("2018")
    else
      return("Diferença entre os anos")
  })
  

  linguagem <- function()({
    if (input$codigo == 1)
      return("R")
    else
      return("C++")
  })
  
  varredura <- function()({
    if (input$window == 1)
      return("Raio")
    else
      return("K")
  })
  
  janela <-function()
  ({
    if (input$window == 1)
      return(as.numeric(input$raio)/1000)
    else
      return(input$k)
  })
  
  unt <- function()({
    if (input$window == 1)
      return("km<br>")
    else
      return("elementos<br>")
  })
  
  output$option <- renderText({
    if (is.null(action$list_data)) return()
    paste("<b>Entradas escolhidas<br></b>",
          "<b>Dados analisados : </b>", data(), "<br>",
          "<b>Linguagem utilizada : </b>", linguagem(), "<br>",
          "<b>Janela de varredura : </b>", varredura(), "fixo<br>",
          "<b>", varredura(), "utilizado: </b>", janela() , unt(),
          "<b>Nível de significancia: </b>", as.numeric(input$alpha)*100, "%<br>",
          "<b>simulações de Monte Carlo: </b>", input$simulacao , "iterações<br><br>")
    
  })
  
  output$time <- renderText({
    if (is.null(action$list_data)) return()
    if(input$window == 1)
    {
      paste("<b>Tempo de execução<br></b>",
            "<b>Matriz de Distância : </b>", action$list_data[[2]][1], "seg<br>",
            "<b>Geração de Clusters : </b>", action$list_data[[2]][2], "seg<br>",
            "<b>Simulação de Monte Carlo: </b>", action$list_data[[2]][3], "seg<br>",
            "<b>Clusters Sgnificativos: </b>", action$list_data[[2]][4], "seg<br>",
            "<b>Total: </b>", action$list_data[[2]][5], "seg<br><br>")
      
    }
    else
    {
    paste("<b>Tempo de execução<br></b>",
          "<b>Matriz de Distância : </b>", action$list_data[[2]][1], "seg<br>",
          "<b>Matriz de Index : </b>", action$list_data[[2]][2], "seg<br>",
          "<b>Geração de Clusters : </b>", action$list_data[[2]][3], "seg<br>",
          "<b>Simulação de Monte Carlo: </b>", action$list_data[[2]][4], "seg<br>",
          "<b>Clusters Sgnificativos: </b>", action$list_data[[2]][5], "seg<br>",
          "<b>Total: </b>", action$list_data[[2]][6], "seg<br><br>")
    }

  })
  
  output$sig <- renderText({
    if (is.null(action$list_data)) return()
    paste("<b>Resultado</b><br>",
      "<b>Número de clusters significativos : </b>", length(action$list_data[[1]]), "<br>")
  })
  
  output$mapa <- renderLeaflet({
      if (is.null(action$list_data)) return()
      showSignificantClustersInfo(action$list_data[[1]],relacaoAlimentadorId)
  })

}

shinyApp(ui,server)