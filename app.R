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
   
   # Application title
   titlePanel("Initial Text Prediction App"),
   
   # Sidebar with a slider input for number of bins 
   navlistPanel(widths=c(2,10),
                   tabPanel("App",
                            textInput("ti","Enter your text here"),
                            textOutput("textpred"),
                            textOutput("titoks")      
                            
                            
                            
                            
                            ),
                   tabPanel("Details")
                   
         #sliderInput("bins",
                     # "Number of bins:",
                     # min = 1,
                     # max = 50,
                     # value = 30)
      )#,
      
      # Show a plot of the generated distribution
      # mainPanel(
      #     textInput("ti","Enter your text here"),
      #     textOutput("textpred"),
      #     textOutput("titoks")
      #    #plotOutput("distPlot")
    #  )
   #)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$titoks <- renderText({
    nwords <- stri_count_words(input$ti)
    
    nwords
    
  }) 
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

