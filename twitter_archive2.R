# ##################################
# This scipt imports twitter data
# from a JSON file into Dataframe ##
# ##################################

# install.packages("ndjson","rtweet","tidytext","streamR","rtweet")

## Libraries ##
library(tidyverse)
library(jsonlite)
library(streamR)
library(devtools)
library(googledrive)
#library(ndjson)
library(dplyr)
library(tidytext)
library(rtweet)
library(rjson)

### import json data 
#Put twitter.json into you wd
# setwd("~/github/lter-wg-scicomp/sci-comp-SNAPP-soil_carbon")
# Full twitter.data parsed into a dataframe: 

#short json dataframe 
twitter.data <- stream_in("twitter.json")

twitter.data2<-fromJSON(file= "twitter.json")
twitter.data.2<-as.data.frame(twitter.data2)

#, simplifyVector = TRUE, simplifyDataFrame = simplifyVector,
#                      simplifyMatrix = simplifyVector, flatten = FALSE)

twitter.data.1<-parse_stream("twitter.json")


#Full json to dataframe 
twitter.data.full<-stream_in("/home/shares/soilcarbon/Twitter/twitter.json")
# Notes:
# Ensure path is linked to the soil-carbon twitter file
# 3500 variables, 90000 obs.

### Wrangle dataframe - remove unecessary columns such as those with "NA" or objec id.
class(twitter.data)
names(twitter.data)
str(twitter.data)




### import json data 

# setwd("~/github/sci-comp-SNAPP-soil_carbon/soc-twitter")


### Full twitter.data parsed into a dataframe: 
#short json dataframe 
# twitter.data <- stream_in("/home/shares/soilcarbon/Twitter/twitter.json")
# twitter.data.1<-stream_in("twitter.json") # use wit jsonlite
# fromJSON(twitter.data)
# rtweet::parse_stream("twitter1.json")
### Full json to dataframe 
twitter.data.full<-stream_in("/home/shares/soilcarbon/Twitter/twitter.json")
# Notes:
# Ensure path is linked to the soil-carbon twitter file
# 3500 variables, 90000 obs.


### Wrangle dataframe - remove unecessary columns such as those with "NA" or objec id.
class(twitter.data.full)
names(twitter.data.full)
str(twitter.data.full)
test1<-sample_n(twitter.data.full, 10)
### Simplified DF based on info that seemed most used (MODIFY select() inputs as needed)
twitter_data_simplified <- twitter.data %>% 
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
###Twitter.data df to smaller dfs according to column name
#object
twitter.data_object<-twitter.data %>% 
  select(starts_with("object"))
#actor
twitter.data_actor<-twitter.data %>% 
  select(starts_with("actor"))
#object
twitter.data_generator<-twitter.data %>% 
  select(starts_with("generator"))
#provider
twitter.data_provider<-twitter.data %>% 
  select(starts_with("provider"))
#twitter
twitter.data_twitter<-twitter.data %>% 
  select(starts_with("twitter"))
#long object
twitter.data_twitter<-twitter.data %>% 
  select(starts_with("long_object"))
#gnip 
twitter.data_gnip<-twitter.data %>% 
  select(starts_with("gnip"))
#All other columns that are not part of a particulay group  
twitter.data_else<-twitter.data %>% 
  select(-starts_with("gnip"), -starts_with("long_object"), -starts_with("twitter"),
         -starts_with("provider."), -starts_with("generator"), -starts_with("actor"), -starts_with("object"))

###With full df: twitter.data.full
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

##Twitter.data df to smaller dfs according to column name

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
twitter.data_chr<-twitter.data %>% 
  select(which(sapply(., is.character)))

twitter.data_chr<-twitter.data.full %>% 
  select(which(sapply(., is.character)))

#All other columns that are not part of a particulay group  

twitter.else.full<-twitter.data.full %>% 
  select(-starts_with("gnip"), -starts_with("long_object"), -starts_with("twitter"),
         -starts_with("provider."), -starts_with("generator"), -starts_with("actor"), -starts_with("object"))
twitter.character.full<-twitter.data.full %>% 
  
###just class character columns: (to do) 
twitter.data5 <- twitter.data %>% sum(is.na(,1:165))

  
#subset of important items for each dataframe 
###Twitter.data df to smaller dfs according to column name
#object
twitter.object.full<-twitter.data.full %>% 
  select(starts_with("object"))
unique(twitter.object.full)
sum(is.na(twitter.object.full$object.id))
#actor
twitter.actor.full<-twitter.data.full %>% 
  select(starts_with("actor"))
unique(twitter.actor.full)
sum(is.na(twitter.actor.full$actor.displayName))
#object
twitter.generator.full<-twitter.data.full %>% 
  select(starts_with("generator"))
unique(twitter.generator.full)
sum(is.na(twitter.generator.full$generator.displayName))
#provider
twitter.provider.full<-twitter.data.full %>% 
  select(starts_with("provider"))
unique(twitter.provider.full)
sum(is.na(twitter.provider.full$provider.displayName))
##### NOTE: simply specifies the twitter data that it is twitter data 
#twitter
 twitter.twi.full<-twitter.data.full %>% 
  select(starts_with("twitter"))
#long object
twitter.lo.full<-twitter.data.full %>% 
  select(starts_with("long_object"))
#gnip 
twitter.gnip.full<-twitter.data.full %>% 
  select(starts_with("gnip"))
#All other columns that are not part of a particulay group  
twitter.else.full>fll<-twitter.data.full %>% 
  select(-starts_with("gnip"), -starts_with("long_object"), -starts_with("twitter"),
         -starts_with("provider."), -starts_with("generator"), -starts_with("actor"), -starts_with("object"))
twitter.character.full<-twitter.data.full 
na.omit(twitter.data.full)
####
##filter all NA rows 
full.no.na<-na.omit(twitter.data.full)
View(full.no.na)  #removes all NAs (TO EDIT)

# no.na=is.na(twitter.data.full$actor.displayName)
# which(no.na==c("TRUE"))

##just class character columns: 
twitter.character<-twitter.data.full %>% 
  select(which(sapply(., is.character)))

# select_if(has_class("character"))

###rtweet attempts
# twitter.data.rtweet<-parse_stream("twitter.json")
# View(twitter.data.rtweet)                     


#####################################
# Notes: 2379 rows that are NA for all columns. 

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


