######################################################################
# This script imports twitter data from a JSON file into Dataframe  ##
#                                                                   ##
# For SNAPP - Soil Organic Carbon Working group                     ##
#                                                                   ##
# Authors: Margaux Sleckman and Julien Brun - NCEAS/UCSB            ##
# Contact: scicomp@nceas.ucsb.edu                                   ##
# ####################################################################


### Definitions
# JSON definitions - from twitter developer page:
# https://developer.twitter.com/en/docs/tweets/data-dictionary/overview/intro-to-tweet-json
# Tweet - Also referred to as a ‘Status’ object, has many ‘root-level’ attributes, parent of other objects.
# User - Twitter Account level metadata. Will include any available account-level enrichments, such as Profile geo and Klout.
# Entities - Contains object arrays of #hashtags, @mentions, $symbols, URLs, and media.
# Extended Entities - Contains up to four native photos, or one video or animated GIF.
# Places - Parent to ‘coordinates’ object.
#
# It seems we have more this `GNIP` format in our files: http://support.gnip.com/sources/twitter/data_format.html


#  install.packages(c("ndjson","rtweet","tidytext","streamR","rtweet"))

### Libraries 
library(tidyverse)
library(ndjson)
library(streamR)
# library(devtools)
# # library(googledrive)
 

# # library(dplyr)
# library(tidytext)
# library(stringr)
# # library(rtweet)

### CONSTANTS ---- 

server_path <- "/home/shares/soilcarbon/Twitter"
twitter_file <- "twitter.json"
  

#### Full twitter.data.json to dataframe
twitter.data.full <- stream_in(file.path(server_path,twitter_file)) # WORKS

    ## other trials
    # twitter.data2<-fromJSON(file= "twitter.json") # list, not parsed
    # twitter.data3<-as.data.frame(twitter.data2)
    # twitter.data.1<-parse_stream("twitter.json") # creates null dataframe


# Notes: (1) Ensure path is linked to the soil-carbon twitter file; (2) avoid opening dataframe - 3480 columns, 96553 obs.
test1 <- sample_n(twitter.data.full, 10)
View(test1)

## removed all NA rows 
# Note: all NA rows are like this in JSON: recommend using for analysis 
twitter_noNA <- twitter.data.full %>% 
  filter(!is.na(twitter.data.full))
sample_1 <- sample_n(twitter_noNA, 10)

### 1. Wrangle dataframe - remove unecessary columns
#With full df: twitter.data.full - subset of important items  
tw.full_simplified <- twitter.data.full %>% 
  select(actor.displayName, actor.favoritesCount, actor.followersCount,
         actor.friendsCount, actor.image, actor.link, actor.location.displayName, actor.location.objectType,
         actor.objectType, actor.postedTime, actor.preferredUsername,
         actor.statusesCount, actor.summary, actor.twitterTimeZone,
         body, favoritesCount, generator.link, gnip.matching_rules.0.id,
         gnip.urls.0.expanded_url_description, gnip.urls.0.expanded_url_title,
         link, object.objectType, object.postedTime, object.summary,objectType, postedTime,
         retweetCount, twitter_entities.hashtags,twitter_entities.user_mentions,
         twitter_filter_level,twitter_lang, long_object.body, object.actor.displayName,
         object.actor.favoritesCount, object.actor.followersCount, object.actor.friendsCount)

### 2.Twitter.data df to smaller dfs according to column name
#object
twitter.object.full<-twitter.data.full %>% 
  select(starts_with("object"))
#actor
twitter.actor.full<-twitter.data.full %>% 
  select(starts_with("actor"))
#object
twitter.generator.full<-twitter.data.full %>% 
  select(starts_with("generator"))
#provider
twitter.provider.full<-twitter.data.full %>% 
  select(starts_with("provider"))
#twitter
twitter.twi.full<-twitter.data.full %>% 
  select(starts_with("twitter"))
#long object
twitter.lo.full<-twitter.data.full %>% 
  select(starts_with("long_object"))
#gnip 
twitter.gnip.full<-twitter.data.full %>% 
  select(starts_with("gnip"))
twitter.info.full<-twitter.data.full %>% 
  select(starts_with("info"))
#only column of class character
twitter.data_chr<-twitter.data.full %>% 
  select(which(sapply(., is.character)))

#All other columns that are not part of a particular group  
twitter.else.full<-twitter.data.full %>% 
  select(-starts_with("gnip"), -starts_with("long_object"), -starts_with("twitter"),
         -starts_with("provider."), -starts_with("generator"), -starts_with("actor"), -starts_with("object"))
twitter.character.full<-twitter.data.full %>% 
  

###Verifying NAs total in each subDF
#object
unique(twitter.object.full)
sum(is.na(twitter.object.full$object.id)) 
#: number: 23479
#actor
unique(twitter.actor.full)
sum(is.na(twitter.actor.full$actor.displayName))
#object
unique(twitter.generator.full)
sum(is.na(twitter.generator.full$generator.displayName))
#provider
unique(twitter.provider.full)
sum(is.na(twitter.provider.full$provider.displayName))

#####################################
# random 
# select_if(has_class("character"))
###rtweet attempts
# twitter.data.rtweet<-parse_stream("twitter.json")
# View(twitter.data.rtweet)                     
#####################################
# Notes 03/30/18
# 2379 rows that are NA for all columns. 
# JSON CODE TESTED UNSUCCESSFULLY
# twitter.data<-fromJSON("twitter.json", simplifyVector= FALSE, simplifyDataFrame=TRUE)
# read_json("twitter.json")
# all.equal(twitter.data, fromJSON(toJSON(twitter.data))) --> not work parse error
# DOWNLOAD FROM TEAMDRIVE _ unsuccessful
# download.file("https://drive.google.com/open?id=0B2bffCSnw4I4UlJOei1qdnQ4RXBhaXJQZlVKbXRXX1RqbjlB", "twitter.json")
# --> not working. turns data json into a HTML file 
#####################################################
# From Steven 03/19/2018: He is saying he is getting very different data strucutre with these different ways to 
# read in the Twitter data; to be checked
#
# tweet2 <- ndjson::stream_in(twitter.data)
# tweet3 <- rjson::fromJSON(file=twitter.data) # Reads into strangely short object
# tweet4 <- jsonlite::stream_in(file(twitter.data)) # Passes into weird data frame format
# tweet2 <- ndjson::stream_in(twitter.data) --> works (MS)
# tweet3 <- rjson::fromJSON(file=twitter.data) # Reads into strangely short object --> does not work
# tweet4 <- jsonlite::stream_in(file(twitter.data)) # Passes into weird data frame format --> works
# like tweet2 if you do not use "file". same format as ndjson


