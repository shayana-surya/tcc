library(shiny)
library(shinyalert)
library("readxl")
library(shinydashboard)
library(shinyjs)

ui <- navbarPage(title = "ShinyJS",
  tabPanel("Inicio",
    shinyjs::useShinyjs(),
    sidebarLayout(
      sidebarPanel(width = 3,id="sidebar",
        tags$style(type="text/css",".alinha {display:flex;align-items: center; justify-content: center;}"),
                           
        fileInput('file1','arq1', accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv','.xlsx'),width =400),
        fileInput('file2','arq1', accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv','.xlsx'),width =400),
        checkboxInput("checkClonado","Mostrar div oculta",FALSE),
                  
        shinyjs::hidden(
          div(id = "divOculta",
            fileInput('file4','Arquivo de dados', accept = c('text/csv','text/comma-separated-values','text/tab-separated-values','text/plain','.csv','.tsv','.xlsx'),width =400)
          )
        ),
                  
        tags$div(class="alinha",
          actionButton("do","executar",class="btn-primary")
        )
      ),
      mainPanel(class="alinha",
        tags$h1("Conteudo qualquer")
      )
    )
  ),
  
  tabPanel("Analises",
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        div(id='msgInicial', class="alinha",
          tags$h3("Faça o envio dos arquivos para acessar essa pagina")
        ),
        div(id='divAnalises',class="codFundo",
          fluidRow(
            infoBox("Novas compras",10 * 2, icon = icon("credit-card"))
          )
        )
      )
    )
  )
)

server <- function(input,output,session){
  ### oculta div
   shinyjs::hide(id='Sidebar')
  shinyjs::hide(id='divAnalises')
  
  ### desabilita botao
  observe({
    shinyjs::toggleState("do",!is.null(input$file1) && !is.null(input$file2))
  })
  
  ###arquivo oculto toggle altera propriedadeS
  shinyjs::onclick("checkClonado",
    shinyjs::toggle(id="divOculta", anim =TRUE)
  )
  
  shinyjs::onclick("do",
    shinyjs::toggle(id = "divAnalises", anim =TRUE)
  )
  
  observeEvent(input$do,{
    shinyjs::hide(id="msgInicial", anim =TRUE)
  })
}

shinyApp(ui,server)