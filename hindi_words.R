###### Create list of top hindi words #######
## some of these might be 'stop words.' might want to reference the bigrams to see which terms we want to prioritize the translation of.
## would be great if we could easily ammend this list with the translation for each term -- that would allow me to easily replace each character with the translation within the plots

library(tidyverse)
library(stringr)

source("text_analysis_functions.R") # contains function for creating word lists


noRT <- read.csv("twitter_merged_noRT.csv", stringsAsFactors = FALSE) %>%
  distinct()


#filter for hindi characters
hindi_tweets <- noRT %>% 
  filter(
  str_detect(text, "[\u0900-\u097F]+"))


#create word list
hindi_terms <- prepare_text(hindi_tweets) %>% 
  filter(
    str_detect(word, "[\u0900-\u097F]+")) # filter out non-hindi words



