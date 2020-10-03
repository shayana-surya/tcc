require(rgdal)
require(sp)
require(RColorBrewer)
require(leaflet)
require(sf)
require(ggplot2)
require(shiny)
require(DT)


source('main.r')
source('shinydata.r')
source(file="functions.r")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                      @import url('//fonts.googleapis.com/css?family-Lobster|Cabin:400,700');
                      
                      h1 {
                        font-family:'Lobster',cursive;
                        font-weight:500;
                        font-height:1.1;
                      }
                      h3{
                        font-family:'Lobster',cursive;
                        font-weight:300;
                        line-height:1.1;
                      }
                      
                      p{
                       color:yellow;
                       fon
                       t-size 30px;
                      }
                      
                      body{
                      background-color:#ecfff5;
                      color: #00a651;
                      }
                      
                      tbody{
                      background-color:#ecfff5;
                      color: #00a651;
                      }
                      
                      tabPanel{
                      color: #00a651;
                      background-color:#ecfff5;
                      }
                      ")
               
    )
  ),
  navbarPage(title = tags$img(src="https://seeklogo.com/images/C/CEMIG-logo-42E5BC7D36-seeklogo.com.png", height = 30, width = 90),
             tabPanel("Sobre o trabalho",
                      mainPanel(
                        tags$h4("ANÁLISE DO CONSUMO DE ENERGIA POR MEIO DE ALIMENTADORES"),
                        #tags$h2("Texto h2"),
                        tags$h5("A estatística Scan tem sido amplamente utilizados para identificar aglomerações anômalas em um ponto no espaço. Neste trabalho propomos a utilização do método Scan a partir de janelas circulares de varredura para a detectação de regiões nos quais houveram diferenças significativas de consumo de energia, no Estado de Minas Gerais, entre os anos de 2013 e 2018"),
                        #tags$h4("Texto h4"),
                        #tags$h5("Texto h5"),
                        #tags$h6("Texto h6")
                      )
                      
                      ),
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
             tabPanel("EDA",
                      sidebarLayout(
                        sidebarPanel(
                            selectInput("EDA2","1. Análise exploratória", choice = c("dados 2013" = 1, "dados 2018" =2, "Alimentadoras em comum (diferença de consumo entre anos)"=3, "Alimentadoras desconsideradas"=4),selected = 1),
                            conditionalPanel(
                              condition = "input.EDA2 != 4",
                              plotOutput("histPlot"),
                              h4("Sumário"),
                              verbatimTextOutput("sum"),
                            )
                        ),
                        mainPanel(
                           leafletOutput("plot2",height = "90vh")
                        )
                      )
                  ),
             tabPanel("Dados de Entrada",
                      sidebarLayout(
                        sidebarPanel(
                          tags$h4("Selecione os dados de entrada:"),
                          #selectInput("numero","1. Selecione o número de alimentadoras dentro do cluster", choice = c("3" = 1, "10" =2, "100" = 3),selected = 1),
                          selectInput("raio","1. Tamanho do raio", choice = c("1000" = 1, "5000" =2, "10000" = 3),selected = 1),
                          selectInput("simulacao","2. Número de simulações", choice = c("10" = 1, "100" =2, "1000" = 3),selected = 1),
                          actionButton("submit", "Submit")
                        ),
                        mainPanel(
                          leafletOutput("mapa",height = "90vh")
                        )
                      )
             
             ),
             tabPanel("Análise de Resultados",
                        mainPanel(
                          tableOutput("verossimilhanca"),
                          tableOutput("significancia")
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
  output$plot <- renderPlot({
    if(input$EDA1== 1)
      histMatrixSimul
    else
      histMatrixDist
  })
  output$histPlot <- renderPlot(showHist(input$EDA2,dados2013,dados2018,diffCemigData))
  output$plot2 <- renderLeaflet({showMapInfo(input$EDA2,dados2013,dados2018,list_data)})
  output$sum <- renderPrint({showSummary(input$EDA2,dados2013,dados2018,diffCemigData)})

  
  ######### QUARTA ABA ##################
  
  
  observeEvent(input$submit, {
    calculator(input$raio,input$simulacao)
  })
  
  
  output$mapa <- renderLeaflet({
    if(input$numeroMapa == 1){
      mapa}
    else if(input$numero == 2){
      mapa}
    else{
      mapa}
  })
  
  ######### QUINTA ABA ##################
  
  output$verossimilhanca <- renderTable(dados2013)
  output$significancia <- renderTable(dados2013)
}

shinyApp(ui,server)