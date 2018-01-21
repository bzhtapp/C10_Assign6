# This function will predict the next word given phrase x
# The function uses Katz Backoff method
# Inputs:
#   tokens - token object
#   train - list of n-gram document feature matrix training set used for prediction.  
#     List needs to be sequence {dfm_ng1, dfm_ng2, ..}
# Output:
#   string object of the word to be predicted given phrase x

library(tm)

source("getTokens.R")

tpredict <- function (x, train)
{
  if (class(x) != "tokens") {x <- getTokens(x)}
  
  ng <- length(train)
  dfm <- train[[ng]]
  
  #Reduce tokens to just the last (ng - 1) tokens
  tokens <- str_split(x, "/")
  
  #pad tokens to the left until it is one less than ng using first token as pad
  while (length(tokens) - 1 < ng)
  {
    tokens <- c(tokens[1], tokens)
  }
  
  n <- length(tokens)
  tokens <- tokens [(n - ng + 2): n]
  n <- length(tokens)
  
  predicator_start <- dim(dfm)[2] - ng + 1 #starts at first predictor word
  predicator_end <- dim(dfm)[2] - 1 #end before last word, last word is the prediction
  
  # Missing probability mass
  mpm <- 1 - sum(dfm$discount_tf)
  
  #Match ngram with tokens in sequential order
  for (i in predicator_start:predicator_end)
  {
    dfm <- dfm[ dfm[, i] == tokens[ i - predicator_start + 1], ]
  }
  
  dfm <- dfm[ ,  "discount_tf", drop=FALSE]
  
  if (ng <= 2)
  {
    #Recursive terminate condition, terminate at n-gram 2
    return (dfm)
  }
  
  dfm_minus_1 <- tpredict(x, train[1:(ng-1)]) #get dfm of next n-gram size smaller  
  
  if (dim(dfm_minus_1)[1] == 0)
  {
    return (dfm)
  }
  
  dfm_minus_1$discount_tf <- mpm * dfm_minus_1$discount_tf
  
  output <- rbind(dfm, dfm_minus_1, drop=FALSE)
  
  return(output)
}

