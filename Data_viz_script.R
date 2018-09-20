##################################################
# Twitter Influencers regarding Soil Health     ##
#                                               ##
# Script 2: Data analysis and Viz               ##   
#                                               ##
# For SNAPP - Soil Organic Carbon Working Group ##
# Author: Margaux Sleckman                      ##
# Contact: scicomp@nceas.ucsb.edu               ##
##################################################


# Packages/wd ####

# Packages needed:
library(tidyverse)
library(devtools)
library(dplyr)
library(tidytext)
library(stringr)
library(magrittr)
# install.packages("data.table")
library(data.table)
library(lubridate)

### Input files #####

## Read csv files from data processing scripts
twitter_merged <- read.csv("twitter_merged.csv", stringsAsFactors = FALSE)
twitter_merged_noRT <- read.csv("twitter_merged_noRT.csv", stringsAsFactors = FALSE)

# If you do not have those files in your repository, please run the data processing script:
# source("raw_data_processing.R")
  
### Data viz/Analysis of dataset retweets and favorites #####

# Verify variable types
str(twitter_merged)
str(twitter_merged_noRT)

# Retweet_count/Favorite_count grouping - w/RT
top_user <- twitter_merged %>% 
  group_by(screen_name) %>% 
  summarise(total_tweets = n(), retweet_count = sum(retweet_count), fav_count = sum(favorite_count)) %>% 
  arrange(- total_tweets) %>% 
  head(1000)
head(top_user)
# View(top_user)

# Retweet_count/Favorite_count grouping - noRT
top_user_noRT <- twitter_merged_noRT %>% 
  group_by(screen_name) %>% 
  summarise(total_tweets = n(), retweet_count = sum(retweet_count), fav_count = sum(favorite_count)) %>% 
  arrange(- total_tweets) %>% 
  head(1000)
head(top_user_noRT)
# View(top_user_noRT) 

### bar plots 

# ggsave(p, file = "myplot_name.png", dpi = 300, width=7, height=5)

# w/RTs top 20
ggplot(head(filter(top_user, top_user$retweet_count >= 5), 20), aes(screen_name, retweet_count)) + 
  geom_bar(stat="identity", fill = 'blue', size=1 )+
  coord_flip()+
  theme_classic()

ggplot(head(filter(top_user, top_user$fav_count >= 5), 20), aes(screen_name, fav_count)) + 
  geom_bar(stat="identity", fill = 'firebrick', size=1) +
  coord_flip()+
  theme_classic()

# W/out RT

ggplot(head(filter(top_user_noRT, top_user_noRT$retweet_count >= 5), 20), aes(screen_name, retweet_count)) + 
  geom_bar(stat="identity", fill='firebrick', size=1)+
  coord_flip()+
  theme_classic()

ggplot(head(filter(top_user_noRT, top_user_noRT$fav_count >= 5), 20), aes(screen_name, fav_count)) + 
  geom_bar(stat="identity", fill = 'blue', size=1 )+
  coord_flip()+
  theme_classic()

# Group_by query word data of provenance API 
# number of tweets per  query word

query_df <- twitter_merged
query_df_2 <- twitter_merged_noRT

query_count_df <- twitter_merged %>%   # filter(query != 'NA') %>% 
  group_by(query = tolower(query)) %>% 
  count() 

query_count_df_noRT <- twitter_merged_noRT %>%  
  # filter(query != 'NA') %>%
  group_by(query = tolower(query)) %>% 
  count()

ggplot(query_count_df, aes(x=query, y=n))+
  geom_bar(stat = "identity", fill = "firebrick")+
  theme(axis.text.y = element_text(colour="grey20", size=12),
        axis.text.x = element_text(colour="grey20", size=12),
        element_blank(),
        element_rect())+ 
  coord_flip()

ggplot(query_count_df_noRT, aes(x=query, y=n))+
  geom_bar(stat = "identity", fill = "skyblue")+
  theme(axis.text.y = element_text(colour="grey20", size=12),
        axis.text.x = element_text(colour="grey20", size=12),
        element_blank(),
        element_rect())+ 
  coord_flip()

# time series:
str(twitter_merged) # check the class of "created_at

# general plot over time of tweets

# ts_plot(twitter_merged)
# ts_plot(twitter_merged_noRT)

# Timeseries by query word 
query_df <- twitter_merged %>% 
  group_by(query = tolower(query))

head(query_df)

query_df_2 <- twitter_merged_noRT %>% 
  group_by(query = tolower(query))

ts_plot(query_df)
ts_plot(query_df_2) #n = the number of tweets on that day 

# Group by country
tweets_country <- twitter_merged %>% 
  group_by(country = country) %>% 
  summarise(tweets_count = n())%>% 
  arrange(- tweets_count) %>% 
  head(20)

tweets_country

# Group by place based on the place_name column of the data (much more varied in terms of results)
tweets_place <- twitter_merged %>% 
  group_by(place = place_name) %>% 
  summarise(tweets_count = n())%>% 
  arrange(- tweets_count) %>% 
  head(20)
tweets_place_2 <- twitter_merged_noRT %>% 
  group_by(place = place_name) %>% 
  summarise(tweets_count = n())%>% 
  arrange(- tweets_count) %>% 
  head(20)

tweets_place
tweets_place_2