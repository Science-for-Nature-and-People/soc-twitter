#### generate data for network analysis on content creators

library(rtweet)
library(tidyverse)
source("../../text_analysis_functions.R")

noRT <- read.csv("/home/shares/soilcarbon/Twitter/Merged_v3/twitter_merged_noRT_v3.csv", stringsAsFactors = FALSE)



top_RT <- noRT_clean %>% 
  group_by(screen_name) %>% 
  dplyr::summarise(
    total_RT = sum(retweet_count)
  )

