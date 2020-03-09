library(shiny)
data(iris)

ui <- fluidPage(
    tags$head(
      tags$style(HTML("
                      @import url('//fonts.googleapis.com/css?family-Lobster|Cabin:400,700');
                      
                      h1 {
                        font-family:'Lobster',cursive;
                        font-weight:500;
                        font-height:1.1;
                        color:#48ca3b;
                      }
                      h3{
                        font-family:'Lobster',cursive;
                        font-weight:300;
                        line-height:1.1;
                        color:blue;
                      }
                      
                      p{
                       color:yellow;
                       fon
                       t-size 30px;
                      }
                      body{
                      background-color:pink;
                      }
                      
                      navbar{
                      background:blue;
                      }
                      ")
        
      )
    ),
  navbarPage(title = "Exemplo NavBar",
                  tabPanel("Sobre a Pagina",
                           h4("Exemplo de utilizacao da NavBar")
                  ),
                 
                 tabPanel("Dados",tableOutput("data")),
                 
                 tabPanel("Grafico",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("b","Selecione o numero de dados", min=5,max=20,value=10)
                            ),
                            mainPanel(
                              plotOutput("plot")
                            )
                          )
                  ),
                 navbarMenu("Mais opcoes",
                            tabPanel("Menu A",verbatimTextOutput("summary")),
                            tabPanel("Menu B",
                                     tags$h1("DADOS SOBRE A PAGINA")
                            )),
                tabPanel("Iris DataSet",
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("var","1. Selecione a variavel do iris dataset", choice = c("Sepal.Length" = 1, "Sepal.width" =2, "Petal.Length" = 3, "Petal.width" = 4),selected = 2),
                      br(),
                      sliderInput("bins","2. Selecione a quantidade de dados",min=5,max=25,value=15),
                      br(),
                      radioButtons("color","3. Selecione a cor do histograma", choices=c("Green","Red","Yellow"), selected = "Green")
                    ),
                    mainPanel(
                      tabsetPanel(type="tab",
                                  tabPanel("summary"),
                                  tabPanel("structure"),
                                  tabPanel("Data", tableOutput("data2")),
                                  tabPanel("Grafico", plotOutput("myhist")),
                                  tabPanel("Tags", tags$h1("teste tag"))
                      )
                    )
                  )
                )
    )
  )

server <- function(input,output,session) {
  output$data <- renderTable({
    mtcars
  })
  
  output$plot <- renderPlot({
    hist(mtcars$mpg,col="blue",breaks=input$b)
  })
  
  output$summary <- renderPrint({
    summary(mtcars)
  })
  
  output$data2 <- renderTable({
    colm <- as.numeric(input$var)
    iris[colm]
  })
  output$myhist <- renderPlot({
    colm <- as.numeric(input$var)
    hist(iris[,colm],breaks=seq(0,max(iris[,colm]),l=input$bins+1), col=input$color, main="IRIS", xlab=names(iris[colm]),xlim=c(0,max(iris[,colm])))
  })
}

shinyApp(ui,server)