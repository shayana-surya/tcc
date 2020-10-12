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
                      color: #00a651;
                      text-align: justify;
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
                        HTML("
                          <p>Lorem ipsum erat aenean sapien eros mauris maecenas ut interdum bibendum cras dictumst nibh etiam id, habitasse lacinia tristique ligula potenti quisque volutpat ut litora bibendum mollis justo sagittis curae. leo nostra nam est tempus enim erat class, quisque iaculis vitae mattis ligula porttitor leo porta, habitasse mollis maecenas at sit volutpat. libero quis quam ligula ante hendrerit inceptos scelerisque tristique, arcu venenatis lectus malesuada vehicula adipiscing proin nibh, est faucibus netus quam pretium velit vestibulum. euismod sem eleifend sed volutpat condimentum placerat eros nec rutrum, platea vulputate semper etiam donec ipsum etiam nulla tempus, aenean auctor dapibus taciti sapien phasellus ultricies duis.</p>
                          <p>Quis cursus magna rutrum tincidunt tristique lorem fringilla curabitur, taciti cubilia sapien ornare sodales pharetra duis, suscipit integer potenti himenaeos felis et etiam. malesuada sociosqu sem commodo molestie platea phasellus tellus nibh metus dictumst tempor, magna euismod posuere lobortis himenaeos quisque aenean pulvinar ut quam, nisl sed tincidunt hac primis elit nam ultrices habitasse amet. inceptos curabitur euismod nostra aptent potenti dui id amet, praesent sed nam ut sagittis porta himenaeos aliquam primis, augue erat ornare adipiscing donec hac lacus. tincidunt metus adipiscing vivamus tempor commodo quisque a senectus, scelerisque ut nulla conubia facilisis erat cursus, odio metus class praesent id vehicula libero. </p>  
                          <p>Aliquam dui sollicitudin quam interdum orci lobortis posuere cubilia iaculis, auctor venenatis ut rhoncus varius in convallis odio, nisl dapibus aenean sociosqu quisque varius in turpis. est praesent integer habitasse euismod augue ut imperdiet sollicitudin, senectus eget aptent nullam vehicula curae curabitur potenti, ut nisl luctus ante aliquam fermentum lobortis. justo sapien at lobortis cursus rutrum ultricies non donec phasellus curabitur, risus feugiat sociosqu laoreet suscipit donec egestas leo diam aliquet, tempor a fames orci nibh fermentum posuere auctor ipsum. class tincidunt at et vivamus tempor non vehicula, dictumst potenti odio faucibus lectus eros. </p>  
                        ")
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
                              plotOutput("boxsplot")
                            )
                            , height=1),
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
                        ),
                      mainPanel(
                        tags$h4("Informações gerais"),
                        HTML("
                          <p> A partir dos dados selecionados é possível calcular, em tempo real e a partir dos parâmetros informados, os clusters significativos. Os valores de resultado serão disponibilizado na Aba 'Análise de Resultados'</p>
                          <p> 1. Dados a serem utilizado: Aqui deve ser selecionado os dados de entrada que deseja analisar, estes podem ser os dados de consumo de 2013, consumo de 2018 ou a diferença de consumo entre os dois anos</p>
                          <p> 2. Algoritmo a ser utilizado: Para analisar o tempo de execução, disponibilizamos o código em duas linguagens diferentes: o R e o C++ (Rcpp). Por ser uma linguagem de mais baixo nível, é esperado que o tempo de execução seja menor no algoritmo em C++. Vale resaltar que não são todas as funções que foram isoladas em C++ e sim aquelas que consideramos as mais importantes: o cálculo de distância, a geração de clusters e a simulação de monte carlo. </p>
                          <p> 3. Janela de varredura: Duas diferentes abordagens foram disponibilizadas; a primeira utiliza um raio fixo e cada cluster contem números de alimentadores diferentes desde que a distancia entre eles seja menor ou igual ao raio. Já utilizando K, limitados o número de alimentadores dentro do cluster, isso signifca q surgirão clusters de raios diferentes porém com número de elementos iguais.</p>
                          <p> 4. Número K de elementos/Número K de elementos: Caso opte por realizar o calculo por Raio, o tamanho no raio poderá ser selecionado no item 4; Porém se deseja utilizar K a opção disponibilizará a seleção do número de elementos K dentro do cluster.</p>
                          <p> 5. Nível de Significância: Os níveis de significância mais comuns forám disponibilizados, sendo eles 1%,5% e 10%. Este valor está associado com o nível de confiança de nosso teste; A um alfa de 10% temos um nível de 90% de confiança; para 5% , 95% de confiança e assim sucessivamente. </p>
                          <p> 6. Número de simulações: Por fim, temos um número de simulações, este parâmetro impacta diretamente no tempo de execução do programa. Quanto mais o número de simulações, mais confiável será o resultado gerado porém, mais tempo levará para rodar o algoritmo.</p>
                          ")
                        )
                      )
                     ),
                     tabPanel("Análise de Resultados",
                              sidebarLayout(
                                sidebarPanel(
                                  tags$h4("Entradas escolhidas"),
                                  textOutput("option1"),
                                  textOutput("option2"),
                                  textOutput("option3"),
                                  textOutput("option4"),
                                  textOutput("option5"),
                                  textOutput("option6"),
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
  
  output$option2 <- renderText({
    if (is.null(action$list_data)) return()
    if (input$codigo == 1)
      "Linguagem utilizada: R;"
    else if (input$codigo == 2)
      "Linguagem utilizada: C++;"
  })
  
  output$option3 <- renderText({
    if (is.null(action$list_data)) return()
    if (input$window == 1)
      "Janela de varredura por Raio fixo;"
    else if (input$window == 2)
      "Janela de varredura por K elementos fixo;"
  })
  
  output$option4 <- renderText({
    if (is.null(action$list_data)) return()
    if (input$window == 1){
      if(input$raio == 150)
        "Raio utilizado: 55km;"
      if(input$raio == 250)
        "Raio utilizado: 65km;"
      if(input$raio == 500)
        "Raio utilizado: 75km;"
    }

    else if (input$window == 2)
      if(input$k == 55000)
        "K utilizado: 55km;"
    if(input$k == 65000)
      "K utilizado: 65km;"
    if(input$k == 75000)
      "K utilizado: 75km;"
  })
  
  output$option5 <- renderText({
    if (is.null(action$list_data)) return()
    if (input$alpha == 0.01)
      "Nível de significancia de 1%;"
    else if (input$alpha == 0.05)
      "Nível de significancia de 5%;"
    else if (input$alpha == 0.1)
      "Nível de significancia de 10%;"
  })
  
  output$option6 <- renderText({
    if (is.null(action$list_data)) return()
    if (input$simulacao == 99)
      "99 simulações de Monte Carlo;"
    else if (input$simulacao == 499)
      "499 simulações de Monte Carlo;"
    else if (input$simulacao == 999)
      "999 simulações de Monte Carlo;"
  })
  
  
  output$mapa <- renderLeaflet({
      if (is.null(action$list_data)) return()
      showSignificantClustersInfo(action$list_data,relacaoAlimentadorId,input$raio)
  })

}

shinyApp(ui,server)