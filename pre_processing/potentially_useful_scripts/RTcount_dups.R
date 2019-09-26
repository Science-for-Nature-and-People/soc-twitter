# Trying to find out if merged data bases keep the tweets with the latest (highest number) retweet count

library(tidyverse)
library(lubridate)
library(stringr)

my_path <- '/home/knox/R/SNAP_WIP/'
old_twitter_merged <- read.csv(file.path(my_path, 'twitter_merged_v2.csv'), stringsAsFactors = FALSE) 

df1 <- old_twitter_merged %>% 
  mutate(created_at = ymd_hms(created_at)) %>% 
  filter(created_at >= '2019-08-20 00:00:00' & created_at  <= '2019-08-20 01:00:00') %>% 
  distinct(created_at, .keep_all = TRUE)
df2 <- old_twitter_merged %>% 
  mutate(created_at = ymd_hms(created_at)) %>% 
  filter(created_at >= '2019-08-20 00:00:00' & created_at  <= '2019-08-20 01:00:00') %>% 
  distinct(created_at, .keep_all = TRUE) %>% 
  mutate(favorite_count = favorite_count + 5)

df3 <- df1 %>% 
  bind_rows(df2)

df <- df3 %>% 
  distinct(created_at, text, .keep_all = TRUE)

path <- '/home/shares/soilcarbon/Twitter/Merged_v2'
twitter_merged <- read.csv(file.path(path, 'twitter_merged_v2.csv'), stringsAsFactors = FALSE)

new_df <- twitter_merged %>% 
  mutate(created_at = ymd_hms(created_at)) %>% 
  filter(created_at >= '2019-08-20 00:00:00' & created_at  <= '2019-08-21 00:00:00') %>% 
  distinct(created_at, .keep_all = TRUE)


testing1 <- read.csv('/home/shares/soilcarbon/Twitter/API_csv/fixed_december_28_18.csv',  
                    stringsAsFactors = FALSE) %>% 
  filter(str_detect(text, "come network and learn the"))

testing2 <- read.csv('/home/shares/soilcarbon/Twitter/API_csv/fixed_december_3_18.csv',  
                    stringsAsFactors = FALSE) %>% 
  filter(str_detect(text, "come network and learn the"))

testing <- bind_rows(testing1, testing2)

test <- twitter_merged %>% 
  filter(str_detect(text, "come network and learn the"))
