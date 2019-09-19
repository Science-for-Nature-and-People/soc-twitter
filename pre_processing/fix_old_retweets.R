# Replace old retweets that started with "RT @xxxx:" and ended with "..." because they were truncated by Twitter.


library(tidyverse)
library(lubridate)
library(ggplot2)

#-- DO NOT run the write.csv lines when making edits to script --#

# Read in master data frame 
path <- '/home/shares/soilcarbon/Twitter' # LOCATION OF MASTER FILES
twitter_merged <- read.csv(file.path(path, 'Merged_v2/twitter_merged_v2.csv'), stringsAsFactors = FALSE) 


# Make DF that first drops "RT @xxxx:" then groups by first 100 characters of tweets
old <- twitter_merged %>% 
  mutate(created_at = ymd_hms(created_at)) %>% 
  mutate(created_at = round_date(created_at, unit = "day")) %>% 
  filter(created_at <= ymd("2017-12-01")) %>% #created_at >= ymd("2019-09-01")
  filter(is_india == FALSE) %>% 
  # another filter start with and end with 
  mutate(text = str_replace_all(text, "^RT @\\w+:", "") %>% str_trim()) %>% 
  mutate(short_text = str_sub(text, 1, 100)) %>% 
  group_by(short_text) %>% 
  add_tally() %>% 
  arrange(desc(text))

# temp DF that takes the first (which should put the longest tweet (original tweet) first)
temp <- old %>% 
  slice(1)

# Make final DF that takes the original tweet and replaces the old retweet text  
twitter_merged_v2 <- old %>%
  select(short_text) %>% 
  left_join(temp, "short_text") %>% 
  ungroup() %>% 
  select(-short_text, -n)


# write out final data frame
write.csv(RT, paste(path, '/Merged_v2/', 'twitter_merged_v2.csv', sep = ""), row.names = FALSE)
