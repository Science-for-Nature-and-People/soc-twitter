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


# find duplicate tweets based on date and text
duplicate_tweets <- noRT[(noRT$text %in% noRT$text[duplicated(noRT$text)]) &
                           (noRT$created_at %in% noRT$created_at[duplicated(noRT$created_at)]),] 

dup_sub <- subset(duplicate_tweets, select = -c(hashtags, place_name, country_code, query, is_retweet, UID, country, hits))


uni_sub <- dup_sub[!duplicated(dup_sub),]

testing <- dup_sub[!(dup_sub %in% uni_sub), ]
test <- setdiff(dup_sub, uni_sub)



