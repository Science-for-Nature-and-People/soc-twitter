################################
## Code for translating hindi ##
################################
# The goal is to filter out hindi tweets then translate the tweets using google translate 
# then attach translated tweets to the hindi tweets for comparison 

# libraries used
library(tidyverse)
library(xlsx)
library(readxl)

# LOCATION OF MASTER FILES
path <- '/home/shares/soilcarbon/Twitter/Merged_v2'

# read in data frame
noRT <- read.csv(file.path(path, 'twitter_merged_noRT_v2.csv'), stringsAsFactors = FALSE) %>%
  distinct()


# extracting only hindi text from data frame 
hindi <- noRT %>% 
  filter(
    str_detect(text, '[\u0900-\u097F]+')
  ) %>% 
  select(text)


# export data frame to excel for translation by google (requires: library(xlsx))
write.xlsx(hindi, file = "tweets_hi.xlsx",
           sheetName = "tweets_hi", append = FALSE)

#########################################################
### process to translate tweets from hindi to english ###
#########################################################
# 1) Upload excel document to google translate (https://translate.google.com/?hl=en&tab=TT)
# IMPORTANT: scroll to bottom of page and copy up from bottom (google does a live translation of the page)
# 2) Copy and paste result (it is in html) into a new excel file
# 3) Use this (https://www.extendoffice.com/documents/excel/1139-excel-unmerge-cells-and-fill.html) 
#    resource to unmerge and duplicate the tweet numbers (I used the script module)
# 4) Title the columns (I used numbers and text) then save excel document
# 5) Import the long-format excel document to the working directory


# Read in the translated text (NEED CORRECT FILE NAME, requires: library(readxl))
hindi_translated <- as.data.frame(read_excel("tweets_hi_en.xlsx"), stringsAsFactors = FALSE)

# convert from long to wide data (NOTE column names are 'number' then 'en_text')
english <- aggregate(hindi_translated[,"en_text"], 
                     list(hindi_translated[,"number"]), 
                     function(x) paste0(unique(x), collapse = ""))
names(english) <- c("number", "en_text")

# index by 'number' in descending order 
english$number <- as.numeric(as.character(unlist(english$number)))
english <- english[order(english$number),]

# set index to match 'number' column then drop 'number' column
rownames(english) <- english$number
rownames(english) <- NULL

# merge english and hindi  df's
tweets_hi_en <- cbind(hindi, english) 
names(tweets_hi_en) <- c("hi_text", "number", "en_text")

# grab subset of noRT with just hindi tweets
tweets_hi <- noRT %>% 
  filter(
    str_detect(text, '[\u0900-\u097F]+')
  )

# merge english tweets into hindi tweets
tweets_merged <- cbind(tweets_hi, tweets_hi_en)
tweets_translated <- tweets_merged[, c("provenance","created_at","user_id",
                                       "screen_name","hi_text","en_text",
                                       "source","favorite_count","retweet_count",
                                       "hashtags","place_name","country_code",
                                       "query","is_retweet","UID",
                                       "country","hits")]

# Export the subset of hindi tweets with the english translation column next to hindi column
write.csv(tweets_translated, "hindi_tweets_to_english.csv")
  
  

