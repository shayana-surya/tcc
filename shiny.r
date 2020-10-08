require(rgdal)
require(sp)
require(RColorBrewer)
require(leaflet)
require(sf)
require(ggplot2)
require(shiny)
require(DT)
require(shinycssloaders)
require(shinybusy)

source('inicialization.r')

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
                          selectInput("flag","1. Dados a serem utilizado", choice = c("Consumo de 2013" = 1, "Consumo de 2018" =2, "Diferença entre os anos" = 3),selected = 3),
                          radioButtons("codigo", "2. Algoritmo a ser utilizado:",
                                       c("R" = 1,
                                         "C++" = 2),inline = TRUE),
                          radioButtons("window", "3. Janela de varredura:",
                                       c("Por Raio" = 1,
                                         "Por K elementos" = 2),inline = TRUE),
                          conditionalPanel(
                            condition = "input.window == 1",
                            selectInput("raio","4. Tamanho do raio", choice = c("55 km" = 55000, "65 km" = 65000, "75 km" = 75000),selected = 55000),
                          ),
                          conditionalPanel(
                            condition = "input.window == 2",
                            selectInput("k","4. Número K de elementos", choice = c("150" = 150, "250" =250, "500" = 500),selected = 150),
                          ),
                          selectInput("alpha","5. Nível de Significância", choice = c("1%" = 0.01, "5%" =0.05, "10%" = 0.1),selected = 0.05),
                          selectInput("simulacao","6. Número de simulações", choice = c("99" = 99, "499" =499, "999" = 999),selected = 99),
                          actionButton("submit", "Submit")
                          #conditionalPanel(
                          #  condition = "!is.null(action$significativosRaio_Rcpp) && !is.null(action$significativosRaio_R)",
                          #plotOutput("histPlotSig")
                          #)
                        ),
                        mainPanel(
                          leafletOutput("mapa",height = "90vh") %>% withSpinner(color="#0dc5c1")
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
    show_modal_spinner(spin = "fading-circle",
                       text = "Por favor, aguarde...")
    if(input$window == 1)
    {
      if(input$codigo == 1)
        action$list_data <- calculator_R(Shayinput$raio,input$simulacao,diffCemigData,input$alpha,input$flag)
      
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
    
    if(input$window == 1)
    {
     if (is.null(action$list_data)) return()
     showSignificantClustersInfo(action$list_data,relacaoAlimentadorId,input$raio)
    }
    else
    {
        if (is.null(action$list_data)) return()
    # showSignificantClustersInfo_k(action$list_data_R,relacaoAlimentadorId,input$raio)
    }
    })
  
  ######### QUINTA ABA ##################
  
  #output$verossimilhanca <- renderTable(dados2013)
  #output$significancia <- renderTable(dados2013)
}

shinyApp(ui,server)