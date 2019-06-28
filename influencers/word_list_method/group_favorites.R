##################################
#     'group' favorites          #
##################################


### This script:
#   1)uses the function find_group_prop() identify the proportion of which user groups retweeted any given tweet, 
#   2) creates wordclouds showing the content of tweets that appeal most to each specific user group, seperated by 'category' (soil, rangeland, forest)


library(tidyverse)
library(stringr)
library(rtweet)
library(wordcloud)
library(tidytext)

source("../text_analysis_functions.R") # for word cloud creator
source("find_group_prop.R")

twitter_merged <- read.csv("/home/shares/soilcarbon/Twitter/Merged_v2/twitter_merged_v2.csv", stringsAsFactors = FALSE) %>%
  distinct()
twitter_merged_noRT <- read.csv("/home/shares/soilcarbon/Twitter/Merged_v2/twitter_merged_noRT_v2.csv", stringsAsFactors = FALSE) %>%
  distinct() %>%
  arrange(-retweet_count)


##### creating a column to flag tweets from or about India ####
RT <- flag_india(twitter_merged)

### repeate for noRT
noRT <- flag_india(twitter_merged_noRT)




###############################################################
#~~~~~~~~~~~~~~~~~~~~~~  Begin Analysis  ~~~~~~~~~~~~~~~~~~~~~#
###############################################################

#### ~~~~~~~~~~~  noIndia  ~~~~~~~~~####

####~~~~~~ Top 100 unfiltered  ~~~~~####

#### answering the question, which tweets are most liked by which user group 
non_india_list <- lapply(1:100, find_group_prop, y = 0)
non_india_df <- bind_rows(non_india_list, .id = "ID")

### Box plots comparing the distribution of which user groups retweeted each of the top 100 non-india related tweets
boxplot(prop_like ~ group, data = non_india_df,
        xlab = "User Group",
        ylab = "Proportion of RTs", 
        main = "Proportion of RTs by user group top 100 non-India related tweets")

## filter for which group most liked each tweet from the top 50 most retweeted (not filtered for any category)
group_favorite <- non_india_df %>% 
  group_by(ID) %>% 
  filter(prop_like == max(prop_like) &
           prop_like > 0) %>% # added this b/c there is at least 1 tweet that has no 'likes' by anyone and therefor has all groups as 'max'
  ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
environmentalist <- group_favorite %>% 
  filter(group == "envnmtal") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og) # add new column called 'text' to work with the create_wordcloud function

create_wordcloud(environmentalist)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
government <- group_favorite %>% 
  filter(group == "govt") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(government)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
farmer <- group_favorite %>% 
  filter(group == "farmer") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(farmer)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
scientist <- group_favorite %>% 
  filter(group == "scientist") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(scientist)

#####~~~~~~~~~  top 50 tweets,  'soil' ~~~~~~~~~~~~####
non_india_soil <- lapply(1:50, find_group_prop,y = 0, category = "soil")
soil_df <- bind_rows(non_india_soil, .id = "ID")

boxplot(prop_like ~ group, data = soil_df,
        xlab = "User Group",
        ylab = "Proportion of RTs", 
        main = "Proportion of RTs by user group top 50 non-India related tweets about 'soil' ")

## filter for which group most liked each tweet from the top 50 most retweeted (not filtered for any category)
group_fav_soil <- soil_df %>% 
  group_by(ID) %>% 
  filter(prop_like == max(prop_like) &
           prop_like > 0) %>% # added this b/c there is at least 1 tweet that has no likes by anyone and therefor has all groups as 'max'
  ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
envt_soil <- group_fav_soil %>% 
  filter(group == "envnmtal") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og) # add new column called 'text' to work with the create_wordcloud function

create_wordcloud(envt_soil)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
govt_soil <- group_fav_soil %>% 
  filter(group == "govt") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(govt_soil)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
farmer_soil <- group_fav_soil %>% 
  filter(group == "farmer") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(farmer_soil)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
scientist_soil <- group_fav_soil %>% 
  filter(group == "scientist") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(scientist_soil)





#####~~~~~~~~~  top 10 tweets,  'rangeland' ~~~~~~~~~~~~~####
#after filtering for no_india and "rangeland" only 9 tweets have any RTs (the 10th at least has some favorites so i kept it)
non_india_range <- lapply(1:10, find_group_prop, y = 0, category = "rangeland")
range_df <- bind_rows(non_india_range, .id = "ID")

boxplot(prop_like ~ group, data = range_df,
        xlab = "User Group",
        ylab = "Proportion of RTs", 
        main = "Proportion of RTs by user group top 10 non-India related tweets about 'rangeland' ")

## filter for which group most liked each tweet from the top 50 most retweeted (not filtered for any category)
group_fav_range <- range_df %>% 
  group_by(ID) %>% 
  filter(prop_like == max(prop_like) &
           prop_like > 0) %>% # added this b/c there is at least 1 tweet that has no likes by anyone and therefor has all groups as 'max'
  ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
envt_range<- group_fav_range %>% 
  filter(group == "envnmtal") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og) # add new column called 'text' to work with the create_wordcloud function
#
create_wordcloud(envt_range)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
govt_range <- group_fav_range %>% 
  filter(group == "govt") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(govt_range)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
farmer_range <- group_fav_range %>% 
  filter(group == "farmer") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(farmer_range)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
scientist_range <- group_fav_range %>% 
  filter(group == "scientist") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(scientist_range)


#### ~~~~~~~~~~~  All tweets  ~~~~~~~~~####

####~~~~~~ Top 100 unfiltered  ~~~~~####

#### answering the question, which tweets are most liked by which user group 
full_list <- lapply(1:100, find_group_prop)
full_df <- bind_rows(full_list, .id = "ID")

### Box plots comparing the distribution of which user groups retweeted each of the top 100 non-india related tweets
boxplot(prop_like ~ group, data = full_df,
        xlab = "User Group",
        ylab = "Proportion of RTs", 
        main = "Proportion of RTs by user group for top 100 tweets")

## filter for which group most liked each tweet from the top 50 most retweeted (not filtered for any category)
group_favorite_full <- full_df %>% 
  group_by(ID) %>% 
  filter(prop_like == max(prop_like) &
           prop_like > 0) %>% # added this b/c there is at least 1 tweet that has no likes by anyone and therefor has all groups as 'max'
  ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
envt_full <- group_favorite_full %>% 
  filter(group == "envnmtal") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og) # add new column called 'text' to work with the create_wordcloud function

create_wordcloud(envt_full)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
govt_full <- group_favorite_full %>% 
  filter(group == "govt") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(govt_full)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
farmer_full <- group_favorite_full %>% 
  filter(group == "farmer") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(farmer_full)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
scientist_full <- group_favorite_full %>% 
  filter(group == "scientist") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(scientist_full)










####~~~~~~ top 50 , 'soil' ~~~~~####

full_soil <- lapply(1:10, find_group_prop, category = "soil")
full_soil_df <- bind_rows(full_soil, .id = "ID")

boxplot(prop_like ~ group, data = full_soil_df,
        xlab = "User Group",
        ylab = "Proportion of RTs", 
        main = "Proportion of RTs by user group top 50 tweets about 'soil' ")

## filter for which group most liked each tweet from the top 50 most retweeted (not filtered for any category)
group_fav_soil <- soil_df %>% 
  group_by(ID) %>% 
  filter(prop_like == max(prop_like) &
           prop_like > 0) %>% # added this b/c there is at least 1 tweet that has no likes by anyone and therefor has all groups as 'max'
  ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
envt_soil <- group_fav_soil %>% 
  filter(group == "envnmtal") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og) # add new column called 'text' to work with the create_wordcloud function

create_wordcloud(envt_soil)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
govt_soil <- group_fav_soil %>% 
  filter(group == "govt") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(govt_soil)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
farmer_soil <- group_fav_soil %>% 
  filter(group == "farmer") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(farmer_soil)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
scientist_soil <- group_fav_soil %>% 
  filter(group == "scientist") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(scientist_soil)



####~~~~~~ top 50 , 'rangeland' ~~~~~####
foo <- lapply(1:10, find_group_prop, category = "rangeland")
bar <- bind_rows(foo, .id = "ID")

full_range <- lapply(1:10, find_group_prop, category = "rangeland")
full_range_df <- bind_rows(full_range, .id = "ID")

boxplot(prop_like ~ group, data = full_range_df,
        xlab = "User Group",
        ylab = "Proportion of RTs", 
        main = "Proportion of RTs by user group top 10 non-India related tweets about 'rangeland' ")

## filter for which group most liked each tweet from the top 50 most retweeted (not filtered for any category)
group_fav_range <- full_range_df %>% 
  group_by(ID) %>% 
  filter(prop_like == max(prop_like) &
           prop_like > 0) %>% # added this b/c there is at least 1 tweet that has no likes by anyone and therefor has all groups as 'max'
  ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
envt_range<- group_fav_range %>% 
  filter(group == "envnmtal") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og) # add new column called 'text' to work with the create_wordcloud function
#
create_wordcloud(envt_range)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
govt_range <- group_fav_range %>% 
  filter(group == "govt") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(govt_range)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
farmer_range <- group_fav_range %>% 
  filter(group == "farmer") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(farmer_range)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
scientist_range <- group_fav_range %>% 
  filter(group == "scientist") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(scientist_range)



























