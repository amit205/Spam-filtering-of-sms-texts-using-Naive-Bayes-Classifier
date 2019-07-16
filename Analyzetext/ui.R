#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Analyze text from Gutenbergr project"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = 'book', label = 'Enter the name of the book from GutenbergR project', value = 'Pride and Prejudice'),
      radioButtons("typeInput", label = "Book, Author, ID or Topic",
                   choices = c("Book", "Author", "ID", "Topic"),
                   selected = "Book"),
      textInput(inputId = 'download', label = 'Enter Gutenbergr id', value = '1342'),
      
      plotOutput('Sentiment')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("results"),
      br(), br(),
      tableOutput('book'),
      tableOutput('words')
       
    )
  )
))
