# Remove duplicate tweets in RT and noRT master data frames #


# libraries used
library(tidyverse)
library(lubridate)
library(dplyr)


# LOCATION OF MASTER FILES
path <- '/home/shares/soilcarbon/Twitter/'


# read in data frames
RT_dups <- read.csv(paste(path, '/Merged_v3/twitter_merged_v3.csv', sep = ""), stringsAsFactors = FALSE)
noRT_dups <- read.csv(paste(path, '/Merged_v3/twitter_merged_noRT_v3.csv', sep = ""), stringsAsFactors = FALSE)


# remove duplicates based on have identical variables listed below
RT <- RT_dups %>% 
  distinct(created_at, user_id, screen_name, text, source, .keep_all = TRUE)
noRT <- noRT_dups %>% 
  distinct(created_at, user_id, screen_name, text, source, .keep_all = TRUE)

# write out data frames 
write.csv(RT, paste(path, '/Merged_v3/', 'twitter_merged_v3.csv', sep = ""), row.names = FALSE)
write.csv(noRT, paste(path, '/Merged_v3/', 'twitter_merged_noRT_v3.csv', sep = ""), row.names = FALSE)

