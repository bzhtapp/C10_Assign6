#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(dplyr)

discount = 0.4 #Discount factor for never n-gram next in training set
train <- NULL
ng = 5 # Number of n-gram sizes, from 1 to ...
urlBase = "https://github.com/bzhtapp/C10_Assign6/raw/master/"
max = 5
source("tpredict.R")

# Load top ngram document features
for (i in 1:ng)
{
  file <- paste("df_topfeatures_", i, "gram.RData", sep="")
  url <- paste(urlBase, file,sep="")
  
  if (!file.exists(file))
  {
    download.file(url, file)
  }
  
  load(file)
  
  df_topfeatures_gram$discount_tf <- ( df_topfeatures_gram$count - discount ) / sum(df_topfeatures_gram$count)
  
  df_topfeatures_gram <- cbind(df_topfeatures_gram, 
                               str_split(rownames(df_topfeatures_gram), "_", simplify="TRUE"))
  
  if (is.null(train))
  {
    train <- list(df_topfeatures_gram)
  }
  else{
    train[[i]] <- df_topfeatures_gram
  }
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  
  observeEvent(input$btn_predict, {
    
    x <- tpredict(input$txt_phrase, train)
    
    if ( dim(x)[1] > 0)
    {
      prediction <- str_split(rownames(x)[1], "_", simplify="TRUE")
      output$txt_prediction <- renderText(prediction[length(prediction)])
    }
    else{
      output$txt_prediction <- renderText("I don't know.  Try to rephrase")
    }
    
    n <- max
    
    if (dim(x)[1] < n)
    {
      n <- dim(x)[1]
    }
    
    output$tbl_matches <- renderTable(rownames(x[1:n,,drop=FALSE]))
  })
  

})
