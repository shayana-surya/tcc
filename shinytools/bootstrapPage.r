library(shinythemes)
library(shiny)
library(shinyalert)
library(shinyjs)
library("readxl")
library(shinydashboard)


ui <-bootstrapPage(
  HTML('<button type="button" class="btn btn-primary">Primary</button
  <button type="button" class="btn btn-secondary">Secondary</button>
  <button type="button" class="btn btn-sucess">Sucess</button>
  <button type="button" class="btn btn-danger">Danger</button>
  <button type="button" class="btn btn-warning">Warning</button>
  <button type="button" class="btn btn-info">Info</button>
  <button type="button" class="btn btn-light">Light</button>
  <button type="button" class="btn btn-dark">Dark</button>
  <button type="button" class="btn btn-link">Link</button>',
    title = NULL, responsive = TRUE, theme=NULL
  )
     
)

server <- function(input,output){
  
}

shinyApp(ui,server)

