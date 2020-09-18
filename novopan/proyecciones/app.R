#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(shinyWidgets)
library(fpp3)
library(ggbeeswarm)

ventanovo2 <- read_delim("ventanovopan3.csv", delim = ";") %>%
  set_names(c("year","mes","zona_venta","grupo_articulo","clase_color","espesor","mt3")) %>%
  mutate_if(is.character, as.factor)

con_fechas <- read_delim("con_fechas.csv", delim = ";") %>%
  mutate_if(is.character, as.factor)

ventanovo2$year <- as.character(ventanovo2$year)
ventanovo2$mes <- as.character(ventanovo2$mes)
ventanovo2$espesor <- as.factor(ventanovo2$espesor)

con_fechas$year <- as.character(con_fechas$year)
con_fechas$mes <- as.character(con_fechas$mes)

ventanovo2$fecha <- paste(ventanovo2$year,ventanovo2$mes)
con_fechas$fecha <- paste(con_fechas$year,con_fechas$mes)

con_fechas <- con_fechas %>%
  mutate(Mes = yearmonth(fecha)) %>%
  select(Mes)

server <- function(input, output, session) {
  
  rval_proyeccion <- reactive({
    con1 <- ventanovo2 %>%
      filter(zona_venta %in% input$in_zonaventa)%>%
      mutate(Mes = yearmonth(fecha)) %>%
      group_by(Mes) %>%
      summarise(venta_total = sum(mt3))
    
    con_final <- con_fechas %>%
      left_join(con1, by = "Mes") %>%
      mutate(venta_total = replace_na(venta_total,0)) %>%
      mutate(Mes = yearmonth(Mes))
    
    novo_ts <- con_final %>% as_tsibble(index=Mes)

   })
  
  rval_proyeccion2 <- reactive({
    con1 <- ventanovo2 %>%
      filter(grupo_articulo %in% input$in_grupo) %>%
      filter(clase_color %in% input$in_color) %>%
      filter(espesor %in% input$in_espesor) %>%
      mutate(Mes = yearmonth(fecha)) %>%
      group_by(Mes, zona_venta) %>%
      summarise(venta_total = sum(mt3)) %>%
      mutate(
        zona_venta = reorder(zona_venta, venta_total)
      )
    

  })
  
  output$proyeccion <- renderPlot(res = 96, {
    
    validate(
      need(
        input$in_zonaventa != "", 
        "Selecciona una zona de venta!"
      )
    )
    
    fit_arima <-
      rval_proyeccion() %>%
      model(novo_arima = ARIMA(venta_total))
    
    fit_arima %>%
      forecast(h = 8) %>%
      autoplot(rval_proyeccion())+
      theme(legend.position = "none")
    
    
    })
  
  output$tablero <- renderTable({
    
    validate(
      need(
        input$in_zonaventa != "", 
        ""
      )
    )
    
    fit_arima <-
      rval_proyeccion() %>%
      model(novo_arima = ARIMA(venta_total))
    
    fc <- fit_arima %>%
      forecast(h = 8)
    
    Mes <- format(fc$Mes, "%Y-%b")
    Proyeccion <- fc$venta_total
    
    cuadro <- data.frame(Mes, Proyeccion)
    
    cuadro

  })
  
  output$proyecciontotal <- renderPlot(res = 96,{
    
    #con1 <- ventanovo2 %>%
    #  mutate(Mes = yearmonth(fecha)) %>%
    #  group_by(Mes) %>%
    #  summarise(venta_total = sum(mt3))
    
    #con_final <- con_fechas %>%
    #  left_join(con1, by = "Mes") %>%
    #  mutate(venta_total = replace_na(venta_total,0)) %>%
    #  mutate(Mes = yearmonth(Mes))
    
    #novo_ts <- con_final %>% as_tsibble(index=Mes)
    
    #fit_arima <-
    #  novo_ts %>%
    #  model(novo_arima = ARIMA(venta_total))
    
    fit_arima <- readRDS(file = "my_fit_arima.rds") 
    novo_ts <- readRDS(file = "my_novo_ts.rds") 
    
    fit_arima %>%
      forecast(h = 8) %>%
      autoplot(novo_ts)+
      theme(legend.position = "none")

  })
  
  output$tablerototal <- renderTable({
    
    # con1 <- ventanovo2 %>%
    #   mutate(Mes = yearmonth(fecha)) %>%
    #   group_by(Mes) %>%
    #   summarise(venta_total = sum(mt3))
    # 
    # con_final <- con_fechas %>%
    #   left_join(con1, by = "Mes") %>%
    #   mutate(venta_total = replace_na(venta_total,0)) %>%
    #   mutate(Mes = yearmonth(Mes))
    # 
    # novo_ts <- con_final %>% as_tsibble(index=Mes)
    # 
    # fit_arima <-
    #   novo_ts %>%
    #   model(novo_arima = ARIMA(venta_total))
    # 
    
    fit_arima <- readRDS(file = "my_fit_arima.rds") 
    
    fc <- fit_arima %>%
     forecast(h = 8)
     
     Mes <- format(fc$Mes, "%Y-%b")
     Proyeccion <- fc$venta_total
     
     cuadro <- data.frame(Mes, Proyeccion)
     
     cuadro
 
  })
  
  output$diagramacaja <- renderPlot (res = 96, {
    
    validate(
      need(input$in_grupo != "",""),
      need(input$in_color != "",""),
      need(input$in_espesor != "","")
    )
    
    rval_proyeccion2() %>%
      ggplot(aes(x=zona_venta, y=venta_total, 
                 color=zona_venta, fill = zona_venta))+
      geom_boxplot(alpha = 0.2, outlier.colour = NA)+
      geom_quasirandom()+
      labs(
        x = "ZONA DE VENTA", y = "VENTA EN MT3",
        title = "VENTA EN MT3 POR ZONA"
        
      )+ 
      theme(legend.position = "none")+
      theme(axis.text.x = element_text(angle = -30, 
                                   vjust = 1, 
                                   hjust = 0))
    
    
  })
  
}

ui <- fluidPage(
  titlePanel("Proyeccion de Ventas Novopan"),
  
  fluidRow(
    column(6, plotOutput("proyecciontotal")),
    column(6, tableOutput("tablerototal"))
  ),
  fluidRow(
    column(2, 
      pickerInput(
        inputId = "in_zonaventa",
        label = "Escoja Zona de Venta", 
        choices = levels(ventanovo2$zona_venta),
        multiple = TRUE
      )
    ),
    column(8, plotOutput("proyeccion")),
    column(2, tableOutput("tablero"))
  ),
  fluidRow(
    column(3, 
           pickerInput(
             inputId = "in_grupo",
             label = "Escoja Producto", 
             choices = levels(ventanovo2$grupo_articulo),
             multiple = T
           ),
           pickerInput(
             inputId = "in_color",
             label = "Escoja Color", 
             choices = levels(ventanovo2$clase_color),
             multiple = T
           ),
           pickerInput(
             inputId = "in_espesor",
             label = "Escoja Espesor", 
             choices = levels(ventanovo2$espesor),
             multiple = T
           )),
           column(9, plotOutput("diagramacaja"))
    )
  )


shinyApp(ui, server)