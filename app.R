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
  
    init <- 0
    
  # load ngram data add progress indicator
  ngramvals <- reactiveValues()
  
  observeEvent(init ==0,{
      withProgress(message = 'Loading Dictionaries', value = 0, {
          
  ngramvals$four <- read_csv('data/4gram.csv')
  incProgress(1/3, detail = paste("Doing part", 2))
  
  ngramvals$three <- read_csv('data/3gram.csv')
  incProgress(2/3, detail = paste("Doing part", 3))
  ngramvals$two <- read_csv('data/2gram.csv')
  #incProgress(3/3, detail = paste("Doing part", 3))
  init <- 1
      })
  })
   
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
  
  inptoks <- reactive({
      nwords <- stri_count_words(input$ti)
      if (nwords <2)
          return()
      else if (nwords<3){
          titokens <- unlist(unnest_tokens(as_data_frame(input$ti),
                                           input = value, output = gram, token='ngrams', n=2))
          outtoks <- titokens
      }
      else{
          titokens <- unlist(unnest_tokens(as_data_frame(input$ti),
                                           input = value, output = gram, token='ngrams', n=3))
          ln <- length(titokens)
          outtoks <- titokens[ln]
      }
      outtoks
  })
  output$textpred <- renderTable({
      
      # if (length(unlist(strsplit(inptoks(), split=" ")))<2){
      #     return()
      #     }

      fourg_test <- function(inputstring){
          inputsplit <- strsplit(inputstring, split=" ")
          out.str <- unlist(inputsplit)

          l <- length(out.str)
          w1 <- out.str[l-2]
          w2 <- out.str[l-1]
          w3 <- out.str[l]

          tgtdf <- ngramvals$four
            str(tgtdf)
          out.df <- tgtdf %>%
              filter(word1==w1, word2==w2, word3==w3) %>%
              arrange(desc(n))%>%
              filter(row_number() <=3) %>%
              select(word4)


      }
        
      
      
      OutData <- fourg_test(inptoks())
      
      t(OutData)
      #head(ngramvals$four,10)
      
      #as.character(inptoks())
      
  }, colnames = FALSE, bordered = TRUE, width='90%', striped = TRUE)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

