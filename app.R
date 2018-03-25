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
library(data.table)
#library(rhandsontable)

# Define UI for application that draws a histogram
ui <- fluidPage(theme= shinytheme("superhero"),
                
                includeCSS("www/mods.css"),
                
   
   # Application title
   titlePanel("Swiftkey Text Prediction App"),
   
   # Sidebar with a slider input for number of bins 
   navlistPanel(widths=c(2,10),
                   tabPanel("App",
                            textInput("ti","Enter your text here", width='90%'),
                            #br(),
                            uiOutput("butts", width='90%'),
                            br(),
                            br(), 
                            
                            "After entering a minimum of three words the app 
                            will attempt to predict the next word. 
                            You can then click on the word to update the text string"#,
                            #tableOutput("textpred")#,
                            #box(width=10,
                            #rHandsontableOutput("textpred2")#)#,
                            #textOutput("hot")      
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
                            "A sample of the main code of the app can be found below and can be copied for portability.
                            The full code and ngram models can also be downloaded to quickly get your model up and running.",
                            br(),                            
                            code('fourg_test <- function(inputstring){'),br(),
                            code('     inputsplit <- strsplit(inputstring, split=" ")'),br(),
                            code('     out.str <- unlist(inputsplit)'),br(),
                            br(),    
                            code('     l <- length(out.str)'),br(),
                            code('     w1 <- out.str[l-2]'),br(),
                            code('     w2 <- out.str[l-1]'),br(),
                            code('     w3 <- out.str[l]'),br(),
                             br(),   
                            code('     tgtdf <- ngramvals$four'),br(),
                             br(),  
                            code('    out.df <- tgtdf %>%'),br(),
                            code('        filter(word1==w1, word2==w2, word3==w3) %>%'),br(),
                            code('        arrange(desc(n))%>%'),br(),
                            code('        filter(row_number() <=3) %>%'),br(),
                            code('        select(word4) %>%'),br(),
                            code('        as_tibble()'),br(),
                            code('}'),
                            br(),
                            "Download the main code here",br(),
                            downloadButton("downloadCode", "Download Code"),br(),
                            
                            
                            "Download the ngram models here", br(),
                            downloadButton("downloadData", "Download Data"))
                

                            
      ),
   hr(),
   br(),
   img(src='images/Footer2.png', class="center",style="display: block; margin-left: auto; margin-right: auto;", width='189px')
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    init <- 0
    
  # load ngram data add progress indicator
  ngramvals <- reactiveValues()
  
  observeEvent(init ==0,{
      withProgress(message = 'Loading Dictionaries', value = 0, {
  
          incProgress(0/3, detail = paste("Loading part", 1))        
  ngramvals$four <- fread('data/4gram_sngl.csv')
  setkey(ngramvals$four,word)
  incProgress(1/3, detail = paste("Loading part", 2))
  
  ngramvals$three <- fread('data/3gram_sngl.csv')
  setkey(ngramvals$three,word)
  incProgress(2/3, detail = paste("Loading part", 3))
  ngramvals$two <- fread('data/2gram.csv')
  setkey(ngramvals$two,word1)
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
      
      wd <- paste(w1,w2,w3,sep="_")
      
      tgtdf <- ngramvals$four
      str(tgtdf)
      out.df <- tgtdf[wd,nomatch=0][order(-n)][,word4]
      tl <- length(out.df)
      if(tl>=3){
          out.df[1:3]
      }else if(tl==1|tl==2){
          out.df[1:tl]
      }else {out.df}
      
              # filter(word==wd) %>%
          # arrange(desc(n))%>%
          # filter(row_number() <=3) %>%
          # select(word4) %>%
          # as_tibble()
  }
  
  threeg_test <- function(inputstring){
      inputsplit <- strsplit(inputstring, split=" ")
      out.str <- unlist(inputsplit)
      
      l <- length(out.str)
      w1 <- out.str[l-1]
      w2 <- out.str[l]
      
      wd <- paste(w1,w2,sep="_")
      
      tgtdf <- ngramvals$three
      #str(tgtdf)
      out.df <- tgtdf[wd,nomatch=0][order(-n)][,word3]
      tl <- length(out.df)
      if(tl>=3){
          out.df[1:3]
      }else if(tl==1|tl==2){
          out.df[1:tl]
      }else {out.df}
     
          # tgtdf %>%
          # filter(word==wd) %>%
          # arrange(desc(n))%>%
          # filter(row_number() <=3) %>%
          # select(word3)%>%
          # as_tibble()
  }
  
  
  twog_test <- function(inputstring){
      inputsplit <- strsplit(inputstring, split=" ")
      out.str <- unlist(inputsplit)
      
      l <- length(out.str)
      w1 <- out.str[l]
      
      tgtdf <- ngramvals$two
      #str(tgtdf)
      out.df <- tgtdf[w1,nomatch=0][order(-n)][,word2][1:3]
      #
      
      tl <- length(out.df)
      if(tl>=3){
          out.df[1:3]
      }else if(tl==1|tl==2){
          out.df[1:tl]
      }else {out.df}
      
      #
      
      
          # filter(word1==w1) %>%
          # arrange(desc(n))%>%
          # filter(row_number() <=3) %>%
          # select(word2)%>%
          # as_tibble()
  }
  
  model_wrapper <- function(inputstring){
      OutData <- fourg_test(inptoks())
      #str(OutData)
      #length(OutData)
      if(length(OutData)==0){
          OutData <- threeg_test(inptoks())
          #str(OutData)
      }

      if(length(OutData)==0){
          OutData <- twog_test(inptoks())
          #str(OutData)
      }

      if(length(OutData)==0){
          OutData <- data.frame(x="The specified phrase is not contained within the loaded dictionaries")
      }

      OutData
  }
  
  
  
  # output$Wordcount <- renderText({
  #   nwords <- stri_count_words(input$ti)
  #   
  #   nwords
  #   
  # })
  # output$titoks <- renderText({
  #     nwords <- stri_count_words(input$ti)
  #     if (nwords <2 | length(input$ti==0))
  #         return()
  #     else if (nwords<3){
  #         titokens <- unlist(unnest_tokens(as_data_frame(input$ti),
  #                                          input = value, output = gram, token='ngrams', n=2))
  #     }
  #     else{
  #         titokens <- unlist(unnest_tokens(as_data_frame(input$ti),
  #                                          input = value, output = gram, token='ngrams', n=3))
  #         ln <- length(titokens)
  #         titokens[ln]
  #     }
  #     
  # })
  
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
  # output$textpred <- renderTable({
  #     
  #     nwords <- stri_count_words(input$ti)
  #     if (nwords <3)# | length(input$ti==0))
  #         return()
  #     
  #     OutData <- model_wrapper(inptoks())
  #     
  #     t(OutData)
  #     #head(ngramvals$four,10)
  #     
  #     #as.character(inptoks())
  #     
  # }, colnames = FALSE, bordered = TRUE, width='90%', striped = TRUE)
  

  output$downloadData <- downloadHandler(
      filename="grammodel.zip",
      content = function(file){
          file.copy("./data/grammodels.zip",file)
      }
      
  )
  
  output$downloadCode <- downloadHandler(
      filename = "code.txt",
      content = function(file){
          file.copy("./data/servercode.txt",file)
      }
  )
  
  observeEvent(input$W1,{
      x <- input$ti
      OutData <- model_wrapper(inptoks())
      selected_word <- OutData[1]
      updateTextInput(session,'ti',value=paste(x,selected_word))

  })
  
  observeEvent(input$W2,{
      x <- input$ti
      OutData <- model_wrapper(inptoks())
      selected_word <- OutData[2]
      updateTextInput(session,'ti',value=paste(x,selected_word))
      
  })
  
  observeEvent(input$W3,{
      x <- input$ti
      OutData <- model_wrapper(inptoks())
      selected_word <- OutData[3]
      updateTextInput(session,'ti',value=paste(x,selected_word))
      
  })
  
  output$butts <- renderUI({
      nwords <- stri_count_words(input$ti)
      if (nwords <3)# | length(input$ti==0))
          return()
      
      OutData <- model_wrapper(inptoks())
      #print(OutData)
      
      if (length(OutData)==3){
          l1 <- OutData[1]
          l2 <- OutData[2]
          l3 <- OutData[3]
          
          tagList(
            column(width=11,      
                actionButton("W1",l1, width='33%', icon = icon('angle-double-up')),
                actionButton("W2",l2, width='33%', icon = icon('angle-double-up')),
                actionButton("W3",l3, width='33%', icon = icon('angle-double-up')))
                )
      } else if (length(OutData)==2){
          l1 <- OutData[1]
          l2 <- OutData[2]
          #l3 <- OutData[3,1]
          
          tagList(
              column(width=11,      
              actionButton("W1",l1, width='49%', icon = icon('angle-double-up')),
              actionButton("W2",l2, width='49%', icon = icon('angle-double-up')))#,
              #actionButton("W3",l3, width='33%', icon = icon('angle-double-up')))
          )
      } else if (length(OutData)==1 & OutData[1]!='The specified phrase is not contained within the loaded dictionaries'){
          l1 <- OutData[1]
          #l2 <- OutData[2,1]
          #l3 <- OutData[3,1]
          
          tagList(
              column(width=11,      
                     actionButton("W1",l1, width='98%', icon = icon('angle-double-up')))#,
                     #actionButton("W2",l2, width='50%', icon = icon('angle-double-up')))#,
              #actionButton("W3",l3, width='33%', icon = icon('angle-double-up')))
          )
      } else {
          tagList(
              column(width=11,
                     actionButton("W4","The specified phrase is not contained within the loaded dictionaries",
                     width='98%', icon=icon('ban',class='glyphred')))
          )
      }
      
  })
  
  session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)

