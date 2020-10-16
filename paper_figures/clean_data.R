##### cleaning master data set for use in other scripts (specifically for manuscript figures)
## takes v4 master, cleans data, remove duplicates, filter before may-2020
## only need to run once
library(tidyverse)
source("text_analysis_functions.R")

### most recent merged data
noRT <- read.csv("/home/shares/soilcarbon/Twitter/Merged_v4/twitter_merged_noRT_v4.csv", stringsAsFactors = FALSE)
RT <- read.csv("/home/shares/soilcarbon/Twitter/Merged_v4/twitter_merged_v4.csv",stringsAsFactors = FALSE)


noRT_noindia <- clean_data(noRT, rm_pope = F, rm_india = T) %>% 
  #### get rid of weird overlapping duplicates
  group_by(created_at, screen_name, text) %>% 
  filter(retweet_count == max(retweet_count)) %>% 
  ungroup() %>% 
  distinct()  %>% 
  # end before may 2020
  filter(created_at < "2020-05-01")

noRT_india <- clean_data(noRT, rm_pope = F, rm_india = F) %>% 
  #### get rid of weird overlapping duplicates
  group_by(created_at, screen_name, text) %>% 
  filter(retweet_count == max(retweet_count)) %>% 
  ungroup() %>% 
  distinct()  %>% 
  # end before may 2020
  filter(created_at < "2020-05-01")


RT_noindia <- clean_data(RT, rm_pope = F, rm_india = T) %>% 
  #### get rid of weird overlapping duplicates
  group_by(created_at, screen_name, text) %>% 
  filter(retweet_count == max(retweet_count)) %>% 
  ungroup() %>% 
  distinct()  %>% 
  # end before may 2020
  filter(created_at < "2020-05-01")

RT_india <- clean_data(RT, rm_pope = F, rm_india = F) %>% 
  #### get rid of weird overlapping duplicates
  group_by(created_at, screen_name, text) %>% 
  filter(retweet_count == max(retweet_count)) %>% 
  ungroup() %>% 
  distinct()  %>% 
  # end before may 2020
  filter(created_at < "2020-05-01")



######


write_csv(noRT_noindia, "/home/shares/soilcarbon/Twitter/cleaned_data/noRT_clean.csv")
write_csv(RT_noindia, "/home/shares/soilcarbon/Twitter/cleaned_data/RT_clean.csv")

write_csv(noRT_india, "/home/shares/soilcarbon/Twitter/cleaned_data/noRT_clean_india.csv")
write_csv(RT_india, "/home/shares/soilcarbon/Twitter/cleaned_data/RT_clean_india.csv")



















