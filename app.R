#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(tidytext)
library(stringi)
library(shinythemes)

#textpred

# Define UI for application that draws a histogram
ui <- fluidPage(theme= shinytheme("superhero"),
                
                includeCSS("www/mods.css"),
                
   
   # Application title
   titlePanel("Initial Text Prediction App"),
   
   # Sidebar with a slider input for number of bins 
   navlistPanel(widths=c(2,10),
                   tabPanel("App",
                            textInput("ti","Enter your text here", width='90%'),
                            br(),
                            tableOutput("textpred"),
                            textOutput("Wordcount"),
                            textOutput("titoks")      
                            ),
                   tabPanel("Details")

      )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # load ngram data add progress indicator
  ngramvals <- reactiveValues()
  
  ngramvals$four <- read_csv('data/4gram.csv')
  ngramvals$three <- read_csv('data/3gram.csv')
  ngramvals$two <- read_csv('data/2gram.csv')
  
   
  output$Wordcount <- renderText({
    nwords <- stri_count_words(input$ti)
    
    nwords
    
  })
  output$titoks <- renderText({
      nwords <- stri_count_words(input$ti)
      if (nwords <2)
          return()
      else if (nwords<3){
          titokens <- unlist(unnest_tokens(as_data_frame(input$ti),
                                           input = value, output = gram, token='ngrams', n=2))
      }
      else{
          titokens <- unlist(unnest_tokens(as_data_frame(input$ti),
                                           input = value, output = gram, token='ngrams', n=3))
          ln <- length(titokens)
          titokens[ln]
      }
      
  })
  output$textpred <- renderTable({
      x_test <- data.frame(x=c("This","Is","Test"))
      
      x_test <- t(x_test)
      
      x_test
      
  }, colnames = FALSE, bordered = TRUE, width='90%', striped = TRUE)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

