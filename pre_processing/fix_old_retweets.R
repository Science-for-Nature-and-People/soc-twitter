# Replace old retweets that started with "RT @xxxx:" and ended with "..." because they were truncated by Twitter.
# THIS IS THE BEGINING OF VERSION 3 MASTER DATA FRAME

library(tidyverse)
library(lubridate)

#-- DO NOT run the write.csv lines when making edits to script --#

# Read in master data frame 
path <- '/home/shares/soilcarbon/Twitter' # LOCATION OF MASTER FILES
twitter_master <- read.csv(file.path(path, 'Merged_v2/twitter_merged_v2.csv'), stringsAsFactors = FALSE) 

#SPLIT MASTER DF into archive where is_retweet = NA and newer where is_retweet = T or F
# Flag retweets in master data frame that came from archived (purchased) data (2017-04-01 to 2017-10-10)
twitter_archive <- twitter_master %>% 
  mutate(created_at = ymd_hms(created_at)) %>%  
  filter(is.na(is_retweet)) %>% 
  mutate(is_retweet = str_detect(text, "^RT @\\w+:"))

twitter_newer <- twitter_master %>% 
  mutate(created_at = ymd_hms(created_at)) %>% 
  filter(!is.na(is_retweet))

twitter_merged <- rbind(twitter_archive, twitter_newer)

# SLIT MSATER DF into old retweet format (has "RT @xxxx:") and new rewtweet format (same as orginal tweet)
# Make DF that first drops "RT @xxxx:" then groups by first 100 characters of tweets
old_RT_format <- twitter_merged %>% 
  filter(str_detect(text, "^RT @\\w+:")) %>% 
  # another filter start with and end with 
  mutate(text = str_replace_all(text, "^RT @\\w+:", "") %>% str_trim()) %>% 
  mutate(short_text = str_sub(text, 1, 100)) %>% 
  group_by(short_text) %>% 
  add_tally() %>% 
  arrange(desc(text))

new_RT_format <- twitter_merged %>% 
  filter(!(str_detect(text, "^RT @\\w+:")))

# temp DF that takes the first (which should put the longest tweet (original tweet) first)
temp <- old_RT_format %>% 
  slice(1)

# Make final DF that takes the original tweet and replaces the old_RT_format retweet text  
old_RT_format <- old_RT_format %>%
  select(short_text) %>% 
  left_join(temp, "short_text") %>% 
  ungroup() %>% 
  select(-short_text, -n)

twitter_merged_v3 <- rbind(old_RT_format, new_RT_format)

twitter_merged_noRT_v3 <- twitter_merged_v3 %>% 
  filter(is_retweet == FALSE)

# write out final data frame in Merged_v3 directory
write.csv(twitter_merged_v3, paste(path, '/Merged_v3/', 'twitter_merged_v3.csv', sep = ""), row.names = FALSE)
write.csv(twitter_merged_noRT_v3, paste(path, '/Merged_v3/', 'twitter_merged_noRT_v3.csv', sep = ""), row.names = FALSE)
