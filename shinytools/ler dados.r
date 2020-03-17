library("shiny")
ui <- fluidPage(
  titlePanel("Upload de arquivos"),
  sidebarLayout(
    sidebarPanel(
      fileInput('idArquivo','Selecione o seu arquivo', accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','csv','.tsv')),
      tags$hr(),
      checkboxInput('header','Header',TRUE),
      radioButtons('sep','Separador de colunas',c("Virgula" =',',"Ponto e vircula"=';',"Tab"='\t'),',')
    ),
    mainPanel(
      actionButton("idBotao","Ler o arquivo")
    )
  )
)
options(shiny.maxRequestSize=9*1024^2)
server <- function(input,output){
    observeEvent(input$idBotao,{
    print(input$header)
    arquivo <- read.csv(input$idArquivo$datapath,header = input$header,sep=input$sep)
    #MyData <- read.csv(ifile="c:/TheDataIWantToReadIn,csv","header=TRUE, sep=",")
    print(arquivo)
  })
}

shinyApp(ui, server)