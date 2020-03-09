library(shinythemes)
library(shiny)

ui <-fluidPage(
  #shinythemes::themeSelector(), 
  theme = shinytheme("cerulean"),
  includeCSS("style.css"),
  headerPanel("New Application"),
  
  sidebarPanel(
    sliderInput("obs","Number of observations:",
                  min = 1, max = 1000,value = 500),
    textInput("txt","Text Input:","text here"),
    sliderInput("slider","slider input:",1,100,30),
    actionButton("action","Button"),
    actionButton("action2","Button2", class = "btn-primary")
    
  ),

      mainPanel(
        tabsetPanel(
          tabPanel("Tab 1"),
          tabPanel("Tab 2"),
        plotOutput("distPlot")
        
        )
      )
  )
  server <- function(input,output){
    
  }
  
  shinyApp(ui,server)
  
  