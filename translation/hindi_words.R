#### first (failed) attempt at translating text
# approach was to isolate each hindi word and then to translate each of them individually, then replace each hindi word with its english translation
# this failed, likely b/c hindi has some modifier characters that got dropped during issolation, so when we replaced the terms, the results was lots of gibberish 

library(tidyverse)
library(stringr)
library(tidytext)

source("text_analysis_functions.R") # contains function for creating word lists


noRT <- read.csv("twitter_merged_noRT.csv", stringsAsFactors = FALSE) %>%
  distinct()


##### First need to create a word list to be translated #####

#filter for hindi characters
hindi_tweets <- noRT %>% 
  filter(
  str_detect(text, "[\u0900-\u097F]+"))


#create word list
hindi_terms <- prepare_text(hindi_tweets) %>% 
  filter(
    str_detect(word, "[\u0900-\u097F]+")) # filter out non-hindi words

#write.csv(hindi_terms, "hindi_terms.csv")

#### read in translation from Steve ####

hindi_trans <- read_csv("hindi-translated.csv")


#### Join translated text to original hindi ####

X1 <- 1:nrow(hindi_terms)
hindi_terms$X1 <- X1

translations <- left_join(hindi_terms, hindi_trans, by = 'X1')

# select only hindi and english, and clean the translation
trans <- translations %>% 
  select(-n, -X1) %>% 
  mutate(word.y = str_replace_all(word.y, '"', ""))
#rename columns
names(trans) <- c("hindi","en")


#### test the transtlation ####

#select only text from india
test <- noRT %>% 
  filter(is_india == 1)

# replicate dataframe
# i did this because my approach for translating is by looping, and so I have to contiously overwrite the dataframe and therform cant mutate an entirely new column based on the translation. replicating the df then allows us to compare the translated and untranslated versions
new <- test


  
for(i in 1:nrow(trans)) {
new <- new %>%
  mutate(
  text = str_replace_all(new$text, pattern = trans$hindi[i], replacement = trans$en[i]))
}
  

foo <- data.frame(test$text, new$text)
  
  
  
  
  
  
  


translate_hindi <- function(data) {
  
  for(i in 1:nrow(trans)) {
    data <- data %>%
      mutate(
        text = str_replace_all(data$text, pattern = trans$hindi[i], replacement = trans$en[i]))
  }
  
  return(data)
}













