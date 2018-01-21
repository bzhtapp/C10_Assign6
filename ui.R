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
  titlePanel("Text Modeling"),
  
  # Show a plot of the generated distribution
  mainPanel(
    textInput(inputId="txt_phrase",
              "Enter start of a phrase", width='400px', value="I wish you a merry"),
    actionButton(inputId="btn_predict", label = "Predict next word"),
    headerPanel("Predicted next word is: "),
     textOutput("txt_prediction"),
    headerPanel("Top n-gram matches are: "),
      tableOutput("tbl_matches")
  )
)
)
