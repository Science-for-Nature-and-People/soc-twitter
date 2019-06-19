################################
## Code for translating hindi ##
################################
# the goal here was to create a .txt file of all the tweets that contain hindi, and then to translate the entire .txt file via google translate, then import back into R and replace the hindi tweets with their english translation
# unfortunately google takes liberties with its syntax and did not comform to translating within/between delimiters ('||") so when I relayed the translated text back into a df, the tweets were misaligned...
# not sure how to proceed from here


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
 write(paste(hindi$text, collapse = "||"), "hindi_tweets.txt")




### Read in the translated text ###
 
## read.csv doesnt work b/c sep must be single character not the '||'
# hindi_trans <- read.csv("hindi_tweets_translated.txt", sep = "||", header = FALSE, stringsAsFactors = FALSE)


## next try readLines()
translation <- readLines("hindi_tweets_translated.txt")

#This give us a charcter value that isn't correctly separated. must recombine and then separate:

#turn into dataframe
trans_df <- as.data.frame(translation)

#spread into single row
one_row <- as.data.frame(matrix(trans_df$translation, nrow = 1, ncol = nrow(trans_df)))

#combine into singe string
one_string <- unite(one_row, 'new', 1:ncol(one_row), sep = " ")

# reseparate based on '||'
long <- separate(one_string, new, into = paste0("row", 1:nrow(hindi)), sep = '\\|\\|')

# gather into tidy format
tidy_trans <- gather(long, 'row', 'text')

#compare

combined_trans <- data.frame(tidy_trans$text, hindi$text, filler = 1:465)

