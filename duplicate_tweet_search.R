# Looking into duplicate tweets #


# libraries used
library(tidyverse)
library(lubridate)
library(dplyr)


# TEST DF:
#df <- data.frame(numbers = c(1,2,3,4),
#                 colors = c('blue', 'blue', 'yellow', 'green'),
#                 dates = c(as_datetime(1511870400), as_datetime(1511870400), as_datetime(1511870401), as_datetime(1511870400)))
#df_sub <- df[df$dates %in% df$dates[duplicated(df$dates)] & df$colors %in% df$colors[duplicated(df$colors)],]


# LOCATION OF MASTER FILES
path <- '/home/shares/soilcarbon/Twitter/'


# read in data frame
noRT <- read.csv(paste(path, '/Merged_v2/twitter_merged_noRT_v2.csv', sep = ""), stringsAsFactors = FALSE) %>%
  distinct()

noRT_knox <- read.csv('/home/knox/github/soc-twitter/Merged_v2/twitter_merged_noRT_v2.csv', stringsAsFactors = FALSE)

dup_search <- noRT_knox %>% 
  filter(created_at >= as.Date("2019-07-02 09:40:30 UTC") & created_at <= as.Date("2019-07-11 17:24:30 UTC"))


plsNO <- dup_search %>% 
  distinct(as_datetime(created_at), text, .keep_all = TRUE)

# find duplicate tweets based on date and text
duplicate_tweets <- dup_search[(dup_search$text %in% dup_search$text[duplicated(dup_search$text)]) &
                           (dup_search$created_at %in% dup_search$created_at[duplicated(as_datetime(dup_search$created_at))]),] 

duplicate_tweets1 <- plsNO[(plsNO$text %in% plsNO$text[duplicated(plsNO$text)]) &
                             (plsNO$created_at %in% plsNO$created_at[duplicated(as_datetime(plsNO$created_at))]),] 

dup_sub <- subset(duplicate_tweets, select = -c(hashtags, place_name, country_code, query, is_retweet, UID, country, hits))


uni_sub <- dup_sub[!duplicated(dup_sub),]

testing <- dup_sub[!(dup_sub %in% uni_sub), ]
test <- setdiff(dup_sub, uni_sub)



