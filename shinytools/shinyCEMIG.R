library(shiny)
library(shinyWidgets)
library(shinyalert)

ui <- fluidPage(
  useShinyalert(),
  titlePanel("TESTE"),
  sidebarLayout( position = "left",
    sidebarPanel(
      fluidRow(
        column(style='border: 1px solid black', width= 4,"Conteudo"),
        column(style='border: 1px solid black', width= 4,"Conteudo"), 
        column(style='border: 1px solid black', width= 4,"Conteudo")
      ),
      
      fluidRow(
        column(style='border: 1px solid black', width= 4,"Conteudo"),
        column(style='border: 1px solid black', width= 4,"Conteudo"), 
        column(style='border: 1px solid black', width= 4,"Conteudo")
      ),
      
      actionButton("btn","Mostrar mensagem"),
      h5("Dados de input"),
      textInput("idEntradaTexto","Informe um texto"),
      numericInput("idEntradaNumerica","Informe um numero",0,min = 0, max = 100,step = 5),
      dateInput("idEntradaData","Informe a data",format="dd-mm-yyyy",language = "pt", autoclose = TRUE),
      radioButtons("radioId","Selecione o sue sexo", choices = c("Masculino","Feminino"), inline = FALSE),
      radioButtons("radioId2","Selecione o sue sexo", list("Opcao 1" = 1,"Opcao 2" = 2,"Opcao 3" = 3),selected = 2, inline = TRUE),
      actionButton("idBotao","Clique no botao", icon("refresh")),
      sliderInput("idSlider","Informe a quantidade de dados da amostra:",min=0, max = 1000,step = 10,value = 500),
      checkboxGroupInput("idCheckBox","Selecione os produtos",list("Macarrao" = 1,"Leite" = 2,"Cafe" = 3,"Pão" = 4)),
      sliderInput("SliderId","Selecione a quantidade de numeros", min=100, max=1000,value = 500)
    ),
    mainPanel(
      column(
        width =7,
        tags$b("Progresso vinculado ao slider"),br(),
        progressBar("pb1",value = 50),
        sliderInput("up1","update",min=0,max=100,value=50),br(),br(),
        progressBar("pb2",value=0,total=100,title ="",display_pct=TRUE),
        actionGroupButtons("go","Processar dados")
      ),
      h5("Elementos de saida"),
      textOutput("idSaidaTexto"),
      verbatimTextOutput("idSaida", placeholder = TRUE),
      verbatimTextOutput("idSaidaData"),
      verbatimTextOutput("idOpcaoSelecionada"),
      verbatimTextOutput("idOpcaoSelecionada2"),
      tags$img(src="cemig-logo.png", height = 50, width = 200),
      textOutput("idSaida2"),
      textOutput("idSaidaSlider"),
      actionButton("SalvaProdutos","Clique"),
      plotOutput("graficoHist"),
      
      tags$code("if x > 10 Then"),
      tags$p("Aqui será um paragrafo"),
      tags$h1("Texto h1"),
      tags$h2("Texto h2"),
      tags$h3("Texto h3"),
      tags$h4("Texto h4"),
      tags$h5("Texto h5"),
      tags$h6("Texto h6"),
      tags$strong("Negrito"),
      tags$br(),
      tags$em(tags$u("Italico e sublinhado")),
      tags$u("Sublinhado"),
      tags$label("aqui"),
      tags$p("TESTES COM ESTIO (STYLE)",style = "color:blue"),
      HTML('<p> <h1> teste com tag HTML </p> <h1>'),
      tags$div(style = "background-color: #008FFF;text-align:center",
               tags$p("texto que esta dentro da div")
               ),
      tags$label("Infome o seu problema"),
      tags$textarea("idTextArea",row=3,cols=5)
      
      
    )
  )
)

server <- function(input,output,session){
  observeEvent(input$up1, {
    updateProgressBar(
      session = session,
      id = "pb1",
      value = input$up1
    )
  });
  observeEvent(input$go,{
    for(i in 1:100){
      updateProgressBar(
        session = session,
        id="pb2",
        value = i,total = 100,
        title = paste("Progresso")
      )
    Sys.sleep(0.1)
  }
  });
  
  observeEvent(input$btn,{
    shinyalert(title = "Mensagem de erro!", type="error")
  });
  
  output$idSaidaTexto <- renderText({input$idEntradaTexto});
  output$idSaida <- renderText({input$idEntradaNumerica});
  output$idSaidaData <- renderText({as.character(input$idEntradaData)});
  output$idOpcaoSelecionada <- renderText({input$radioId});
  output$idOpcaoSelecionada2 <- renderText({input$radioId2});
  observeEvent(input$idBotao,{
    # todo o codigo que estiver dentro do ovserve event sera executado quando o botao for executado
    output$idSaida2 <- renderText("Mensagem que sera exibida quando o botao for pressionado")
  });
  output$idSaidaSlider <- renderText(({input$idSlider}));
  observeEvent(input$SalvaProdutos, {
    opcoesSelecionadas <- as.data.frame(input$idCheckBox);
    print(opcoesSelecionadas);
  });
  output$graficoHist <- renderPlot({hist(sample(input$SliderId))});
}

shinyApp(ui = ui,server = server)
