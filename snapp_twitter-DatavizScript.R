##################################################
# Twitter Influencers regarding Soil Health     ##
#                                               ##
# Script 2: Data analysis and Viz               ##   
#                                               ##
# For SNAPP - Soil Organic Carbon Working Group ##
# Author: Margaux Sleckman                      ##
# Contact: scicomp@nceas.ucsb.edu               ##
##################################################

#soc-twitter on SNAPP version !!!

## Task ####
# Find the main influencers regarding soil health on Twitter
# Look into:
# Number of retweets per Tweet
# Most retweeted person
# Most followers
##

# Packages/wd ####

# Packages needed:
library(tidyverse)
library(jsonlite)
library(streamR)
library(devtools)
library(googledrive)
library(ndjson)
library(dplyr)
library(tidytext)
library(stringr)
library(rtweet)
library(rjson)
#install.packages("skimr")
library(skimr)
#install.packages("janitor")
library(janitor)
library(gtools)
library(magrittr)
# install.packages("data.table")
library(data.table)
library(lubridate)
#install.packages("ids")
library(ids)



### Input files #####

## Read csv files from data processing scripts
twitter_merged <- read.csv("twitter_merged.csv", stringsAsFactors = FALSE)
twitter_merged_noRT <- read.csv("twitter_merged_noRT.csv", stringsAsFactors = FALSE)

# If you do not have those files in your repository, please run the data processing script:
# source("snapp_twitter-script2.R")
  
  
### Data viz/Analysis of dataset retweets and favorites #####

# Retweet_count/Favorite_count grouping - w/RT
top_user <- twitter_merged %>% 
  group_by(screen_name) %>% 
  summarise(total_tweets = n(), retweet_count = sum(retweet_count), fav_count = sum(favorite_count)) %>% 
  arrange(- total_tweets) %>% 
  head(1000)
View(top_user)

# Retweet_count/Favorite_count grouping - noRT
top_user_noRT <- twitter_merged_noRT %>% 
  group_by(screen_name) %>% 
  summarise(total_tweets = n(), retweet_count = sum(retweet_count), fav_count = sum(favorite_count)) %>% 
  arrange(- total_tweets) %>% 
  head(1000)
View(top_user_noRT) 

### bar plots 

# <--- !!! Add saving the plot to file !!! --->
# ggsave(p, file = "myplot_name.png", dpi = 300, width=7, height=5)

# w/RTs top 20
ggplot(head(top_user, 20), aes(screen_name, retweet_count)) + 
  geom_bar(stat="identity", fill = 'blue', size=1 )+
  coord_flip()+
  theme_classic()

ggplot(head(top_user, 20), aes(screen_name, fav_count)) + 
  geom_bar(stat="identity", fill = 'firebrick', size=1 )+
  geom_bar(stat="identity", fill = 'blue', size=1) +
  coord_flip() +
  theme_classic()

ggplot(head(top_user, 20), aes(screen_name, fav_count)) + 
  geom_bar(stat="identity", fill = 'firebrick', size=1) +
  coord_flip()+
  theme_classic()

# <--- ??? Maybe filter out the one < 5 ??? --->
# W/out RT
ggplot(head(top_user_noRT,20), aes(screen_name, retweet_count)) + 
  geom_bar(stat="identity", fill='firebrick', size=1)+
  coord_flip()+
  theme_classic()

ggplot(head(top_user_noRT, 20), aes(screen_name, fav_count)) + 
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
str(twitter_merged)
# general plot over time of tweets
ts_plot(twitter_merged)
ts_plot(twitter_merged_noRT)

# <--- ??? Are 0s NAs or real zeros on those plots ??? --->


# Timeseries by query word 
query_df <- twitter_merged %>% 
  group_by(query = tolower(query))

query_df_2[, grep("2017", query_df_2$created_at)]

View(query_df)

query_df_2 <- twitter_merged_noRT %>% 
  group_by(query = tolower(query))

ts_plot(query_df)
ts_plot(query_df_2) #n = the number of tweets on that day 

# Group by country
tweets_country <- twitter_merged %>% 
  group_by(country = country_code) %>% 
  summarise(tweets_count = n())%>% 
  arrange(- tweets_count) %>% 
  head(20)

View(tweets_country)




