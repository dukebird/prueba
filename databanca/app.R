#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(shinycssloaders)

databanca <- readRDS(file = "databanca_knn.rds")

databanca$edad <- as.double(databanca$edad)
databanca$antiguedad_cliente <- as.double(databanca$antiguedad_cliente)

var_doubles <- NULL
var_characters <- NULL
var_integers <- NULL

text_about <- "Aplicativo creado por los alumnos de la 
Maestria de Ciencia de los datos de la 
Universidad Ricardo Palma, Mayo 2020"

for (i in colnames(databanca)){
  if(typeof(databanca[[i]])=="double"){
    
    var_doubles <- append(var_doubles,i)
    
  }
  if(typeof(databanca[[i]])=="character"){
    
    var_characters <- append(var_characters,i)
    
  }
  if(typeof(databanca[[i]])=="integer"){
    
    var_integers <- append(var_integers,i)
    
  }
}

# Define UI for application that draws a histogram

ui <- fluidPage(
   
   # Application title
   tags$img(src='logo2.png',height=150),
   actionButton('show_about', 'Sobre este aplicativo...'),
   titlePanel("Analisis Data Banca Personal"),
   #tags$img(src='logo2.png',height=50,width=50),

   
   fluidRow(
     column(6,  pickerInput(
       inputId = "idvariableintegers",
       label = "Escoja Una Variable Entera a Analizar", 
       choices = var_integers,
       multiple = F
     )),
     column(6, withSpinner(plotOutput("graficoUnivariadoIntegers")))
   ),
   
   fluidRow(
     column(6,  pickerInput(
       inputId = "idvariablecharacteres",
       label = "Escoja Una Variable Categorica a Analizar", 
       choices = var_characters,
       multiple = F
     )),
     column(6, withSpinner(plotOutput("graficoUnivariadoCharacteres")))
   ),
   
   fluidRow(
     column(4,  pickerInput(
       inputId = "idvariabledoubles",
       label = "Escoja Una Variable Continua a Analizar", 
       choices = var_doubles,
       multiple = F
     ),
      switchInput(inputId = "idswitch", value = TRUE,
                  label = "Log10")
     ),
     column(4, withSpinner(plotOutput("graficoUnivariadoContinua"))),
     column(4, withSpinner(plotOutput("graficoBoxPlotContinua")))
   )
   
   
   
   )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$show_about, {
    showModal(modalDialog(text_about, title = 'Sobre este aplicativo'))
  })
  
  output$graficoUnivariadoIntegers <- renderPlot (res=96,{
    
    
     valr = input$idvariableintegers
      
      p <- databanca %>%
              ggplot(aes(x= factor(eval(parse(text=valr))),fill=factor(eval(parse(text=valr)))))+
              geom_bar()+
              theme(axis.text.x = element_text(angle = -60, 
                                               vjust = 1, 
                                               hjust = 0))+
              labs(x = valr , y = "Cantidad de Casos")+
              theme(legend.position = "none")+
              scale_y_continuous(labels = scales::comma_format())

      p
    
  })
   
  output$graficoUnivariadoCharacteres <- renderPlot (res=96,{
    
      valr = input$idvariablecharacteres
      
      databanca %>%
        ggplot(aes(x= eval(parse(text=valr)),fill=factor(eval(parse(text=valr)))))+
        geom_bar()+
        theme(axis.text.x = element_text(angle = -60, 
                                         vjust = 1, 
                                         hjust = 0))+
        labs(x = valr , y = "Cantidad de Casos")+
        theme(legend.position = "none")+
        scale_y_continuous(labels = scales::comma_format())
        

  })
  
  output$graficoUnivariadoContinua <- renderPlot (res=96,{
    
    valr = input$idvariabledoubles  
    
    p <- databanca %>%
            ggplot(aes(x= eval(parse(text=valr))))+
            geom_histogram(fill = "midnightblue", alpha = 0.7)+
            scale_y_continuous(labels = scales::comma_format())+
            labs(x = valr , y = "Cantidad de Casos")
    
    if (input$idswitch == 1) {
      
      p + scale_x_log10(labels = scales::comma_format())
      
    } else {
      
      p + scale_x_continuous(labels = scales::comma_format())
      
    }
    
      
    
  })
  
  output$graficoBoxPlotContinua <- renderPlot (res=96,{
    
    valr = input$idvariabledoubles  
    
    p <- databanca %>%
          ggplot(aes(x=valr,y= eval(parse(text=valr))))+
          geom_boxplot(fill = "red", alpha = 0.7)+
          labs(y = NULL)
    
    if (input$idswitch == 1) {
      
      p + scale_y_log10(labels = scales::comma_format())
      
    } else {
      
      p + scale_y_continuous(labels = scales::comma_format())
      
    }
    
      
  })
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)
