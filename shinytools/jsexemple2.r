library(shiny)
library(shinyalert)
library("readxl")
library(shinydashboard)
library(shinyjs)
#https://daattali.com/shiny/shinyjs-basic/


ui <- fluidPage(
  useShinyalert(),
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(list(.big = "font-size:2em")),
  div(id="divConteudo",
      h2("Shinyjs"),
      checkboxInput("big","Aumentar fonte",FALSE),
      textInput("idName","Name",""),
      a(id = "toggleAdvanced","Monstrar/ocultar campos", href = "#"),
      shinyjs::hidden(
        div(id="conteudoOpcional",
            numericInput("idIdade","Idade",0),
            textInput("idCidade","Cidade","")
        )
      ),
      sliderInput("idSlider","Renda:",
                  min = 100, max = 1000,
                  value = 500),
      actionButton("submit","Enviar"),
      actionButton("reset","Resetar dados")
      )
)

server <- function(input,output){
  observe({
    shinyjs::toggleState("submit",input$idName != "")
  })
  shinyjs:: onclick("toggleAdvanced",
                    shinyjs::toggle(id = "conteudoOpcional", anim = TRUE))
  shinyjs::onclick("idSlider",
                   shinyalert(title = (input$idSlider), type = "info", timer = 3000)
  )
  observe({
    shinyjs::toggleClass("divConteudo","big",input$big)
  })
  observeEvent(input$submit,{
    shinyjs::alert("obrigado por se cadastrar!")
  })
  observeEvent(input$reset,{
    shinyjs::reset("divConteudo")
  })
}

shinyApp(ui,server)