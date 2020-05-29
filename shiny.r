library(shiny)
source('D:/Documentos/tcc/main.r')
source('D:/Documentos/tcc/shinydata.r')

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
                      
                      tabPanel{
                      color: #00a651;
                      }
                      ")
               
    )
  ),
  navbarPage(title = tags$img(src="https://seeklogo.com/images/C/CEMIG-logo-42E5BC7D36-seeklogo.com.png", height = 30, width = 90),
             tabPanel("Sobre a Pagina"),
             tabPanel("Dados",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("var","1. Selecione a tabela que deseja vizualizar", choice = c("dados2013" = 1, "dados2018" =2, "diffCemig" = 3, "excluidas" = 4,"matriz Distancia" = 5, "clusters" = 6),selected = 3)
                  ),
                  mainPanel(
                      tableOutput("data")
                  )
                )
             ),
             tabPanel("EDA",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("b","Selecione o numero de dados", min=5,max=20,value=10)
                        ),
                        mainPanel(
                          plotOutput("plot")
                        )
                      )
                  ),
             tabPanel("Mapa",
                      sidebarLayout(
                        sidebarPanel(
                          "Teste"
                        ),
                        mainPanel(
                          leafletOutput("mapa",height = "90vh")
                        )
                      )
             
             )
            )
      )

server <- function(input,output,session) {
  output$data <- renderTable({
    if(input$var== 1){
      dados2013}
    else if(input$var == 2){
      dados2018}
    else if(input$var == 3){
      diffCemigData}
    else if(input$var == 4){
      excluidas}
    else if(input$var == 5){
      dados2013}
    else{
      dados2013}
  })
  
  output$plot <- renderPlot({
    hist(mtcars$mpg,col="blue",breaks=input$b)
  })
  
  output$mapa <- renderLeaflet(mapa)
  
}

shinyApp(ui,server)