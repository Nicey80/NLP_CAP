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
                            "After entering a minimum of three words the app 
                            will attempt to predict the next word",
                            tableOutput("textpred")#,
                            #textOutput("Wordcount"),
                            #textOutput("titoks")      
                            ),
                   tabPanel("Details",
                            h4('Origins of the App'),
                            "This App was built as part of the John Hopkins Data Science Capstone project
                            using data supplied from Swiftkey. The app is designed to predict the next word
                            that a user will type (after typing a minumum of three words",
                            br(),
                            h5('Data Modelling'),
                            "The app used the tidytext approach to create the necessary ngram models
                            to use in a prediction algorithm. This was chosen in part due to the developers
                            affinity for using tidyverse packages and in part through user testing of a 
                            number of other packages it seemed to perform well when it came to generating the 
                            models. To optimise performance only 60% of the original datasets (news, twitter & 
                            blogs) were used",
                            "A useful text explaining the tidy text approach can be found here",
                            a('https://www.tidytextmining.com/tidytext.html'),
                            br(),
                            h5('Using the App'),
                            "The app itself is simple to use just type in the white box where indicated and 
                            the app will start to predict the next word after a minimum of three words have 
                            been entered",
                            br(),
                            h5('The code beneath'),
                            "The main code of the app can be found below and can be copied for portability.
                            The ngram models can also be downloaded to quickly get your model up and running.",
                            code('fourg_test <- function(inputstring){
                                inputsplit <- strsplit(inputstring, split=" ")
                                out.str <- unlist(inputsplit)
                                
                                l <- length(out.str)
                                w1 <- out.str[l-2]
                                w2 <- out.str[l-1]
                                w3 <- out.str[l]
                                
                                tgtdf <- ngramvals$four
                                #str(tgtdf)
                                out.df <- tgtdf %>%
                                    filter(word1==w1, word2==w2, word3==w3) %>%
                                    arrange(desc(n))%>%
                                    filter(row_number() <=3) %>%
                                    select(word4) %>%
                                    as_tibble()
                            }'))

      ),
   hr(),
   br(),
   img(src='images/Footer2.png', class="center",style="display: block; margin-left: auto; margin-right: auto;", width='189px')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    init <- 0
    
  # load ngram data add progress indicator
  ngramvals <- reactiveValues()
  
  observeEvent(init ==0,{
      withProgress(message = 'Loading Dictionaries', value = 0, {
  
          incProgress(0/3, detail = paste("Loading part", 1))        
  ngramvals$four <- read_csv('data/4gram.csv')
  incProgress(1/3, detail = paste("Loading part", 2))
  
  ngramvals$three <- read_csv('data/3gram.csv')
  incProgress(2/3, detail = paste("Loading part", 3))
  ngramvals$two <- read_csv('data/2gram.csv')
  #incProgress(3/3, detail = paste("Doing part", 3))
  init <- 1
      })
  })
  
  fourg_test <- function(inputstring){
      inputsplit <- strsplit(inputstring, split=" ")
      out.str <- unlist(inputsplit)
      
      l <- length(out.str)
      w1 <- out.str[l-2]
      w2 <- out.str[l-1]
      w3 <- out.str[l]
      
      tgtdf <- ngramvals$four
      #str(tgtdf)
      out.df <- tgtdf %>%
          filter(word1==w1, word2==w2, word3==w3) %>%
          arrange(desc(n))%>%
          filter(row_number() <=3) %>%
          select(word4) %>%
          as_tibble()
  }
  
  threeg_test <- function(inputstring){
      inputsplit <- strsplit(inputstring, split=" ")
      out.str <- unlist(inputsplit)
      
      l <- length(out.str)
      w1 <- out.str[l-1]
      w2 <- out.str[l]
      
      tgtdf <- ngramvals$three
      #str(tgtdf)
      out.df <- tgtdf %>%
          filter(word1==w1, word2==w2) %>%
          arrange(desc(n))%>%
          filter(row_number() <=3) %>%
          select(word3)%>%
          as_tibble()
  }
  
  
  twog_test <- function(inputstring){
      inputsplit <- strsplit(inputstring, split=" ")
      out.str <- unlist(inputsplit)
      
      l <- length(out.str)
      w1 <- out.str[l]
      
      tgtdf <- ngramvals$two
      #str(tgtdf)
      out.df <- tgtdf %>%
          filter(word1==w1) %>%
          arrange(desc(n))%>%
          filter(row_number() <=3) %>%
          select(word2)%>%
          as_tibble()
  }
  
  model_wrapper <- function(inputstring){
      OutData <- fourg_test(inptoks())
      str(OutData)
      length(OutData)
      if(nrow(OutData)==0){
          OutData <- threeg_test(inptoks())
          str(OutData)
      }

      if(nrow(OutData)==0){
          OutData <- twog_test(inptoks())
          str(OutData)
      }

      if(nrow(OutData)==0){
          OutData <- data.frame(x="The specified phrase is not contained within the loaded dictionaries")
      }

      OutData
  }
  
  
  
  output$Wordcount <- renderText({
    nwords <- stri_count_words(input$ti)
    
    nwords
    
  })
  output$titoks <- renderText({
      nwords <- stri_count_words(input$ti)
      if (nwords <2 | length(input$ti==0))
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
      
      nwords <- stri_count_words(input$ti)
      if (nwords <3)# | length(input$ti==0))
          return()
      
      OutData <- model_wrapper(inptoks())
      
      t(OutData)
      #head(ngramvals$four,10)
      
      #as.character(inptoks())
      
  }, colnames = FALSE, bordered = TRUE, width='90%', striped = TRUE)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

