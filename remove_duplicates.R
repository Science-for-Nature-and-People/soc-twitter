# Looking into duplicate tweets #


# libraries used
library(tidyverse)
library(lubridate)
library(dplyr)


# LOCATION OF MASTER FILES
path <- '/home/shares/soilcarbon/Twitter/'


# read in data frames
RT_dups <- read.csv(paste(path, '/Merged_v2/twitter_merged_v2.csv', sep = ""), stringsAsFactors = FALSE)
noRT_dups <- read.csv(paste(path, '/Merged_v2/twitter_merged_noRT_v2.csv', sep = ""), stringsAsFactors = FALSE)


# remove duplicates based on 'created_at' and 'text' columns
RT <- RT_dups %>% 
  distinct(created_at, text, .keep_all = TRUE)
noRT <- noRT_dups %>% 
  distinct(created_at, text, .keep_all = TRUE)


# write out data frames 
write.csv(RT, paste(path, '/Merged_v2/', 'twitter_merged_v2.csv', sep = ""), row.names = FALSE)
write.csv(noRT, paste(path, '/Merged_v2/', 'twitter_merged_noRT_v2.csv', sep = ""), row.names = FALSE)

