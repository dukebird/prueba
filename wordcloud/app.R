#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(wordcloud2)
library(tm)

create_wordcloud <- function(data, num_words = 100, background = "white") {
  
  # If a dataframe is provided, make sure it has the required columns
  if (is.data.frame(data)) {
    if (!"word" %in% names(data) || !"freq" %in% names(data)) {
      stop("Invalid data: expecting two columns named 'word' and 'freq'")
    }
  }
  
  # If text is provided, convert it to a dataframe of word frequencies
  if (is.character(data)) {
    corpus <- Corpus(VectorSource(data))
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    data <- sort(rowSums(tdm), decreasing = TRUE)
    data <- data.frame(word = names(data), freq = as.numeric(data))
  }
  
  # Make sure a proper num_words is provided
  if (!is.numeric(num_words) || num_words < 3) {
    num_words <- 3
  }  
  
  # Grab the top n most common words
  data <- head(data, n = num_words)
  if (nrow(data) == 0) {
    return(NULL)
  }
  
  wordcloud2(data, backgroundColor = background)
}

artofwar <- "En la ciudad de Los Reyes, el quince de Julio de mil ochocientos 
veintiuno. Reunidos en este Excmo. Ayuntamiento los senores que lo componen, 
con el Excmo. e Ilmo. Senor Arzobispo de esta santa Iglesia Metropolitana, 
prelados de los conventos religiosos, titulos de Castilla y varios vecinos 
de esta Capital, con el objeto de dar cumplimiento a lo prevenido en el 
oficio del Excmo. Senor General en jefe del ejercito Libertador del Peru, 
Don Jose de San Martin, el dia de ayer, cuyo tenor se ha leido, he impuesto 
de su contenido reducido a que las personas de conocida probidad, luces y 
patriotismo que habita en esta Capital, expresen si la opinion general se 
halla decidida por la Independencia, cuyo voto le sirviese de norte al 
expresado Sr. General para proceder a la jura de ella. 
Todos los Srs. concurrentes , por si y satisfechos, de la opinion de los 
habitantes de la Capital, dijeron: Que la voluntad general esta decidida 
por la Independencia del Peru de la dominacion Espanola y de cualquiera otra 
extrajera y que para que se proceda a la sancion por medio del 
correspondiente juramento, se conteste con copia certificada de esta acta 
al mismo Excmo. y firmaron los Srs.: El Conde de San Isidro- Bartolome, 
Arzobispo de Lima, Francisco Javier de Zarate- El Conde de la Vega de Ren- 
El Conde de las Lagunas-Toribio Rodriguez-Javier de Luna Pizarro-Jose de la 
Riva Aguero-El marquez de Villa fuerte"


ui <- fluidPage(
  h1("Word Cloud"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "source",
        label = "Origen de la palabras",
        choices = c(
          "Independencia del Peru" = "book",
          "Usa tus propias palabras" = "own",
          "Sube tu archivo" = "file"
        )
      ),
      conditionalPanel(
        condition = "input.source == 'own'",
        textAreaInput("text", "Enter text", rows = 7)
      ),
      conditionalPanel(
        condition = "input.source == 'file'",
        fileInput("file", "Select a file")
      ),
      numericInput("num", "Numero Maximo de palabras",
                   value = 100, min = 5),
      
      # Add a "draw" button to the app
      actionButton(inputId = "draw", label = "Draw!")
    ),
    mainPanel(
      wordcloud2Output("cloud")
    )
  )
)

server <- function(input, output) {
  data_source <- reactive({
    if (input$source == "book") {
      data <- artofwar
    } else if (input$source == "own") {
      data <- input$text
    } else if (input$source == "file") {
      data <- input_file()
    }
    return(data)
  })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })
  
  output$cloud <- renderWordcloud2({
    # Add the draw button as a dependency to
    # cause the word cloud to re-render on click
    input$draw
    isolate({
      create_wordcloud(data_source(), num_words = input$num,
                       background = input$col)
    })
  })
}

shinyApp(ui = ui, server = server)