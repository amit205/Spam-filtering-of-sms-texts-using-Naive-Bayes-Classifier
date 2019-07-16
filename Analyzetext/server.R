#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(gutenbergr)
library(stringr)
library(tidytext)
library(dplyr)
library(syuzhet)
library(ggplot2)
library(tm)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  book <- reactive({gutenberg_download(input$download)})
  output$results <- renderTable({
    table <- gutenberg_works(str_detect(title, input$book))
    table
  }
  )
  output$book <- renderTable({
    
    head(book(),20)
  })
  output$words <- renderTable({
    words <- book() %>%
      mutate(text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = "")) %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words)%>%
      count(word, sort = TRUE)
    head(words,20)
  })
  output$Sentiment <- renderPlot({ 
    #Load data as corpus
    #VectorSource() creates character vectors
    mydata <- Corpus(VectorSource(book()))
    # convert to lower case
    mydata <- tm_map(mydata, content_transformer(tolower))
    
    #remove ?????????????????? what would be emojis
    mydata<-tm_map(mydata, content_transformer(gsub), pattern="\\W",replace=" ")
    
    # remove URLs
    removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
    mydata <- tm_map(mydata, content_transformer(removeURL)
    )
    
    # remove anything other than English letters or space
    removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
    mydata <- tm_map(mydata, content_transformer(removeNumPunct))
    
    # remove stopwords
    mydata <- tm_map(mydata, removeWords, stopwords("english"))
    #u can create custom stop words using the code below.
    #myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),"use", "see", "used", "via", "amp")
    #mydata <- tm_map(mydata, removeWords, myStopwords)
    # remove extra whitespace
    mydata <- tm_map(mydata, stripWhitespace)
    # Remove numbers
    mydata <- tm_map(mydata, removeNumbers)
    # Remove punctuations
    mydata <- tm_map(mydata, removePunctuation)
    result <- get_nrc_sentiment(as.character(mydata))
    #change result from a list to a data frame and transpose it 
    result1<-data.frame(t(result))
    #rowSums computes column sums across rows for each level of a #grouping variable.
    new_result <- data.frame(rowSums(result1))
    new_result
    names(new_result)[1] <- "count"
    new_result <- cbind("sentiment" = rownames(new_result), new_result)
    rownames(new_result) <- NULL
    
    #plot the first 8 rows,the distinct emotions
    qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("Sentiments")
    
    #plot the last 2 rows ,positive and negative
 #   qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("Sentiments")
    
    
    })
    
  
})
