# This function takes a string object and returns tokens

library(stringr)
library(stringi)
library(quanteda)

getTokens <- function (x)
{
  # Clean phrase x
  sngl_quot_rx = "[`]"
  dbl_quot_rx = "[\"]"
  # consolidate different apostroph variants.
  x <- gsub(dbl_quot_rx, "\"", x)
  x <- gsub(sngl_quot_rx, "'", x)
  # remove hashtags
  x <- gsub("[[:blank:]]#[^[:blank:]]*", " ", x, perl = TRUE)
  # remove URLs
  x <- gsub("(https?)?://[^[:blank:]]*", " ", x, ignore.case = TRUE, perl = TRUE)
  # remove irrelevant chars (like @$%&*, surrounding quotation marks etc)
  x <- gsub("[^[:alnum:]'.?!]", " ", x, perl = TRUE)
  # remove surrouting apostrophs
  x <- gsub("[[:blank:]]'([[:alnum:][:blank:]]+)'[[:blank:]]", "\\1", x, perl = TRUE)
  # remove sourounding whitespace
  x <- stri_trim_both(x)
  # condense multiple spaces to one
  x <- gsub("[[:blank:]]{2,}", " ", x, perl = TRUE)
  
  # Assumption is that model would have removed all stop words on training set.
  # So the same set of stop word have to be removed from phrase x
  my_stopwords <- stopwords("english")[1:120] %>% 
    c(letters) %>% 
    c("to", "in", "on", "with", "at", "not", "from", "so", "all", "for", "by", "just", "said", "about", "said", "one", "get") %>%
    c( "rt", "lol", "im", "st", "u.s", "p.m", "a.m", "mr", "dr", 
       "ll", "ur", "omg", "co", "oh", "ha", "haha", "ha", "la")
  
  #Making general corpus
  scorpus <- corpus(x)
  tokens <- tokens(scorpus, remove_punct = TRUE, remove_numbers = TRUE)
  tokens <- tokens_tolower(tokens) %>% tokens_remove(my_stopwords)
  
  return (tokens)
}
