################################
## Code for translating hindi ##
################################

library(tidyverse)
library(stringr)

noRT <- read.csv("twitter_merged_noRT.csv", stringsAsFactors = FALSE) %>%
  distinct()


## extracting hindi texts ####

hindi <- noRT %>% 
  filter(
    str_detect(text, '[\u0900-\u097F]+')
  ) %>% 
  select(text)

# write this^ to a txt file to send to Steve for google translation
# write(paste(hindi$text, collapse = "||"), "hindi_tweets.txt")




## Read in the translated text

hindi_trans <- read.csv("hindi_tweets_translated.txt", sep = "||", header = FALSE, stringsAsFactors = FALSE)

