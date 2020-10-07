require(rgdal)
require(sp)
require(RColorBrewer)
require(leaflet)
require(sf)
require(ggplot2)
require(shiny)
require(DT)


source('main.r')
#source('shinydata.r')
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
                        tags$h5("	Lorem ipsum imperdiet luctus aenean facilisis mauris ut metus eleifend, torquent conubia quam nisi lacinia aenean ac quisque, condimentum eros cras nullam adipiscing tortor mi dapibus. dolor nisi fames nec amet netus nostra feugiat tristique fringilla at inceptos mattis ornare venenatis, congue ultrices arcu egestas ullamcorper ornare euismod conubia proin venenatis nam lectus. quis scelerisque class quam etiam sapien dapibus purus cubilia tempor sapien nostra, malesuada varius arcu facilisis dapibus per malesuada suspendisse tortor. etiam vulputate enim rutrum orci venenatis dui tellus vehicula, lobortis aenean nibh ut mattis sapien nam ultricies, lacus erat ornare arcu rutrum tincidunt semper. 

	Molestie condimentum blandit pharetra erat sem rhoncus, id feugiat suspendisse pulvinar hac per curabitur, proin semper maecenas erat etiam. augue feugiat nibh litora quis vivamus potenti vehicula risus, hac nibh et ultricies pretium nam leo habitasse, suscipit iaculis quam mi hendrerit etiam cubilia. cras malesuada augue laoreet bibendum viverra malesuada ac blandit at platea malesuada, torquent etiam inceptos netus sociosqu ac dictumst curae cursus inceptos, ad lobortis libero id class congue aliquam sollicitudin conubia maecenas. posuere laoreet risus habitant aenean scelerisque hac donec taciti, nunc a mauris varius habitasse amet sollicitudin, dolor enim hac consectetur mi cubilia massa. 

	Auctor aptent enim, ut. "),
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
                          radioButtons("codigo", "Algoritmo a ser utilizado:",
                                       c("C++" = 1,
                                         "R" = 2)),
                          selectInput("raio","1. Tamanho do raio", choice = c("24000" = 24000, "34000" =34000, "44000" = 34000),selected = 24000),
                          selectInput("alpha","2. Nível de Significância", choice = c("1%" = 0.01, "5%" =0.05, "10%" = 0.1),selected = 2),
                          selectInput("simulacao","3. Número de simulações", choice = c("10" = 10, "100" =200, "1000" = 1000),selected = 1),
                          actionButton("submit", "Submit")
                          #conditionalPanel(
                          #  condition = "!is.null(action$significativosRaio_Rcpp) && !is.null(action$significativosRaio_R)",
                          #plotOutput("histPlotSig")
                          #)
                        ),
                        mainPanel(
                          leafletOutput("mapa",height = "90vh")
                        )
                      )
             
             ),
             tabPanel("Análise de Resultados",
                        mainPanel(
                          #tableOutput("verossimilhanca"),
                          #tableOutput("significancia")
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
  output$plot2 <- renderLeaflet({showMapInfo(input$EDA2,dados2013,dados2018,list_data)})
  output$sum <- renderPrint({showSummary(input$EDA2,dados2013,dados2018,diffCemigData)})

  
  ######### QUARTA ABA ##################
  
  action <- reactiveValues(data = NULL)
  
  observeEvent(input$submit, {
    if(input$codigo == 1)
      action$list_data_Rcpp <- calculator_Rcpp(input$raio,input$simulacao,diffCemigData,input$alpha)
    else
      action$list_data_R <-calculator_R(input$raio,input$simulacao,diffCemigData,input$alpha)
  })
  

  #  output$histPlotSig <- renderPlot({
  #    if(input$codigo == 1)
  #    {
  #      if (is.null(action$significativosRaio_Rcpp)) return()
  #      histogramaSignificantCluster(action$list_data_Rcpp)
  #    }
  #    else{
  #      if (is.null(action$significantClusters_R)) return()
  #      histogramaSignificantCluster(action$list_data_R)
  #    }
  #})
  
    output$mapa <- renderLeaflet({
    
    if(input$codigo == 1)
    {
      if (is.null(action$list_data_Rcpp)) return()
      showSignificantClustersInfo(action$list_data_Rcpp,relacaoAlimentadorId,input$raio)
    }
      else{
        if (is.null(action$list_data_R)) return()
      showSignificantClustersInfo(action$list_data_R,relacaoAlimentadorId,input$raio)
    }})
  
  ######### QUINTA ABA ##################
  
  #output$verossimilhanca <- renderTable(dados2013)
  #output$significancia <- renderTable(dados2013)
}

shinyApp(ui,server)