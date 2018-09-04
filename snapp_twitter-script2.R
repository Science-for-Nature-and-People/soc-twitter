##################################################
# Twitter Influencers regarding Soil Health     ##
#                                               ##
# For SNAPP - Soil Organic Carbon Working Group ##
# Author: Margaux Sleckman                      ##
# Contact: scicomp@nceas.ucsb.edu               ##
##################################################

##Task##
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

setwd("/home/sleckman/R/soc-twitter1")
# setwd("/home/shares/soilcarbon/Twitter/")
getwd()

###########READING_DATA##################################
### Reading json(ARC)/API twitter datasets ####

## ARC        #####
snapp_twitterdata <- stream_in("/home/shares/soilcarbon/Twitter/twitter.json")

#' The \code{stream_in} and \code{stream_out} functions implement line-by-line processing
#' of JSON data over a \code{\link{connection}}, such as a socket, url, file or pipe. JSON
#' streaming requires the \href{http://ndjson.org}{ndjson} format, which slightly differs
#' from \code{\link{fromJSON}} and \code{\link{toJSON}}, see details.
#' 
#' Notes: termed twitter.json as Archived (ARC) dataset in script
#' (1) Ensure path is linked to the soil-carbon twitter file
#' (2) VERY LARGE DF: avoid opening - 3480 columns, 96553 obs.

test <- sample_n(snapp_twitterdata, 10)
test2 <- head(snapp_twitterdata, 10)

## remove all NA rows  
snapp_twitterdata <- snapp_twitterdata %>% 
  filter(!is.na(snapp_twitterdata))

test3 <- head(snapp_twitterdata, 10)
sampleA <- sample_n(snapp_twitterdata, 100)
View(rbind(test2, test3)) # verify NA rows are removed

# write.csv(sampleA, file = "./sampleA.csv")

#API  ####

#' Run bash script on API files in /home/shares/soilcarbon/Twitter/rTweet/ before running. 
#' Used personal folder to retrieve fixed_datasets. substitute wd() to local

list.files(path="soc-twitter/", pattern="^fixed_", full.names=TRUE)

twitter_API <- do.call(rbind,
                       lapply(list.files(path="soc-twitter/",
                                         pattern="^fixed_",
                                         full.names=TRUE), function(x) {read.csv(x, stringsAsFactors =FALSE)}))

head(unique(twitter_API$query))
head(unique(twitter_API$created_at))
class(twitter_API$created_at)
# twitter_API$created_at <- as_datetime(twitter_API$created_at, format = "%Y-%m-%d %H:%M:%S")

str(twitter_API)

# Is there a unique ID in dataset? resolved --> made new uid: ####
length(twitter_API$status_id)
length(which(!duplicated(twitter_API$status_id)))
length(twitter_API$user_id)
length(which(!duplicated(twitter_API$user_id)))
length(twitter_API$query)
length(which(duplicated(twitter_API$query)))

### For Export
 # write.csv(snapp_twitterdata,
 #          file = "/home/sleckman/R/soc-twitter1/twitterdata-excel.csv")

### Understanding Archived df: Splitting header categories

object <- snapp_twitterdata %>% 
  select(starts_with("object"))
actor <- snapp_twitterdata %>% 
  select(starts_with("actor"))
generator <- snapp_twitterdata %>% 
  select(starts_with("generator"))
provider <- snapp_twitterdata %>% 
  select(starts_with("provider"))
twitter <- snapp_twitterdata %>% 
  select(starts_with("twitter"))
long_object <- snapp_twitterdata %>% 
  select(starts_with("long_object"))
gnip <- snapp_twitterdata %>% 
  select(starts_with("gnip"))
info <- snapp_twitterdata %>% 
  select(starts_with("info"))
hashtags <- snapp_twitterdata %>% 
  select(actor.preferredUsername, body, contains("hashtag"))
#only columns of class character
all_char <- snapp_twitterdata %>% 
  select(which(sapply(., is.character)))

# Find relevant columns from the archives to match to API dataset: 
columns <- as.character(names(snapp_twitterdata))  
grep(pattern = "Reply", columns, value = TRUE)
grep(pattern = "key", columns, value = TRUE) # --> no key word
grep(pattern = "status_id", columns, value = TRUE)
grep("quote|status|id", columns, value = TRUE)
grep("retweet", columns, value = TRUE)
grep("favorite", columns, value = TRUE)
hashtags <- grep("hashtags", columns, value = TRUE)
unique(snapp_twitterdata$twitter_entities.hashtags)
hashtags[10,10]


dim(snapp_twitterdata[,grep("hashtag", colnames(snapp_twitterdata))])

View(snapp_twitterdata[,grep("hashtag", colnames(snapp_twitterdata))])

snapp_twitterdata[,snapp_twitterdata[,grep("hashtag", colnames(snapp_twitterdata))]]
head(snapp_twitterdata[,grep("hashtag", colnames(snapp_twitterdata))])

lapply(snapp_twitterdata[,grep("hashtag", colnames(snapp_twitterdata))],
       function(x){length(x)}) 

### Understanding Favorites and Retweet Counts 
#In Twitter_API:
retweet_API <- select(.data = twitter_API,
                      grep("retweet", names(twitter_API), value = TRUE))
which(anyNA(retweet_API))

favorite_API <- select(.data = twitter_API,
                       grep("favorite", names(twitter_API), value = TRUE))
max(favorite_API$favorite_count, na.rm = T)
max(retweet_API$retweet_count, na.rm = T)

#In Archived data:
favorites_ARC <- snapp_twitterdata %>% 
  select(grep("favorite",names(snapp_twitterdata), value = TRUE))
names(favorites)

retweets_ARC <- snapp_twitterdata %>% 
  select(grep("retweet",names(snapp_twitterdata), value = TRUE))

length(is.na(favorites_ARC$actor.favoritesCount))
length(is.na(retweets_ARC$retweetCount))

head(snapp_twitterdata$retweetCount)
head(twitter_API$retweet_count)

# Retweet count from both dfs  seem to match
head(twitter_API$favorite_count)
head(snapp_twitterdata$object.favoritesCount)
#best match with favorite_count API --> object.favoritecount

### ARC + API URLS
      # [1] "gnip.urls.0.expanded_status"                                                                         
      # [2] "gnip.urls.0.expanded_url"                                                                            
      # [3] "gnip.urls.0.expanded_url_description"                                                                
      # [4] "gnip.urls.0.expanded_url_title"                                                                      
      # [5] "gnip.urls.0.url"

### Focus on URLS - made specific dataset with all url-related column names from Archive, liked tweet and username 
#Select body and url so that we can determine how the different urls are linked to the tweet. 

urls <- snapp_twitterdata %>%
  select(actor.preferredUsername, body, grep("url", names(snapp_twitterdata), value = TRUE))
# NOTE: A lot of url columns in snapp_twitter
# not as many in API

# Look at sum of NA per column in ARC to see what column has the most NAs 
urls <- rbind(urls, as.data.frame(
  lapply(urls[-1:-2], function(x){
    sum(is.na(x))})))

sample_n(urls[1:10], 10)

## new dataframes to merge: 
str(snapp_twitterdata$twitter_entities.hashtags.0.text) # works
str(snapp_twitterdata$twitter_entities.hashtags.3.text)
str(snapp_twitterdata$twitter_entities.hashtags.2.text)
str(snapp_twitterdata$twitter_entities.hashtags.1.text)


###################MERGING_TABLES####################################
### Merge tables
##Cleaning

#ARC: #####
snapp_twitterdata_merge <- snapp_twitterdata %>% 
  select(object.postedTime, actor.id,
         actor.preferredUsername, body, generator.displayName,
         object.favoritesCount, retweetCount, id) %>%
  mutate(query = NA) %>%  # to populated
  set_colnames(c("created_at", "user_id","screen_name",
                 "text","source","favorite_count","retweet_count", "id", "query")) %>% 
  separate(id, into=c("id_text", "id_num"), sep="5:") # done to later remove id_text

names(snapp_twitterdata_merge)
str(snapp_twitterdata_merge$query) # nothin in query

# Remove id:twitter.com in user id
snapp_twitterdata_merge$user_id<- str_remove(snapp_twitterdata_merge$user_id, "id:twitter.com:")
head(snapp_twitterdata_merge$user_id)

# Remove additional zeros in date 
snapp_twitterdata_merge$created_at<- str_remove(snapp_twitterdata_merge$created_at, ".000")
head(snapp_twitterdata_merge$created_at)
class(snapp_twitterdata_merge$created_at)
# Convert create_at date to date format
snapp_twitterdata_merge$created_at<- as_datetime(snapp_twitterdata_merge$created_at)

class(snapp_twitterdata_merge$id_num)
length(unique((snapp_twitterdata_merge$id_num)))
length(nchar(snapp_twitterdata_merge$id_num)) 
# --> several duplicates exist in the ID column for twitter --> decided to create new UID for the dataset that is merged (select(c())).

names(snapp_twitterdata_merge)
str(snapp_twitterdata_merge)


#API: #####
head(unique(twitter_API$hashtags))

twitter_API_merge <- twitter_API %>% 
  select(created_at, user_id, screen_name, text, source, favorite_count, retweet_count, query)

# Convert create_at date to date format
twitter_API_merge$created_at <- as_datetime(twitter_API_merge$created_at)

# Adjust dfs to common column class  
str(twitter_API_merge)
str(snapp_twitterdata_merge)

matching_querywords<-sample_n(snapp_twitterdata_merge, 100)
querywords<-unique(twitter_API_merge$query)

#--- Fill query column with query words

query_ARC <- str_match(snapp_twitterdata_merge$text, pattern = 'soil health|healthy soil|#soilhealth|#SoilHealth|#healthysoil|soil quality|soil fertility|#soilquality|#soilfertility|rangeland health|#rangelandhealth|healthy rangelands|#healthyrangelands') 

snapp_twitterdata_merge$query <- paste0(query_ARC) #, sep = "|", collapse = NULL)

View(head(snapp_twitterdata_merge$query))
View(snapp_twitterdata_merge$query[1:20, ])                                        
View(snapp_twitterdata_merge[1:20, ])
# head(snapp_twitterdata_merge$query)
# snapp_twitterdata_merge$hashtags <- str_extract(snapp_twitterdata_merge, startsWith(snapp_twitterdata_merge$text, "#"))
# 
# startsWith(snapp_twitterdata_merge$text, "#")
# head(snapp_twitterdata_merge$text, 5)

# to convert: 
twitter_API_merge$user_id <- as.character(twitter_API_merge$user_id) # char --> num
  # twitter_API_merge$favorite_count <- as.numeric(twitter_API_merge$favorite_count) # char --> num #
  # twitter_API_merge$retweet_count <- as.numeric(twitter_API_merge$retweet_count) # char --> num 

twitter_API$retweet_count[which(is.na(twitter_API_merge$retweet_count))]
class(twitter_API_merge$retweet_count)

##MERGE
#Created a provenance column and a unique id column
namelist <- list( API = twitter_API_merge, ARC = snapp_twitterdata_merge[,-8:-9]) # For merged (i.e. bind_rows), removed remove id column for Archived Dataset

twitter_merged <- bind_rows(namelist, .id = "provenance")
twitter_merged <- twitter_merged %>% mutate(UID = id(twitter_merged, drop = FALSE))

twitter_merged_noRT <- bind_rows(namelist, .id = "provenance") 
twitter_merged_noRT <- twitter_merged_noRT %>% 
  mutate(UID = id(twitter_merged_noRT, drop = FALSE)) %>% 
  filter(!str_detect(text, "^RT")) # ^ used to select only RT at start of text. subs with "starts_with()"
                                   #note: issue with adding piping code lines on newly created df ...(to fix). Made two pipes sequences for now 

# Confirm UIDs
which(duplicated(twitter_merged, fromLast = T))
length(unique((twitter_merged$UID))) #126063
length(twitter_merged$UID) # #129673 to verify...

str(twitter_merged_noRT)
skim(twitter_merged_noRT)
skim(twitter_merged)
length(which(is.na(snapp_twitterdata$object.favoritesCount))) # confirms that number of favorite NA same as NA in ARC

## Analysis of structure for with RT and no RT:
#w/ RT
str(twitter_merged)
skim(twitter_merged)
length(twitter_merged[which(is.na(twitter_merged$favorite_count)),]) # all favorite_count NAs originate from ARC 
twitter_merged[which(is.na(twitter_merged$retweet_count)),] 

#no_RT
str(twitter_merged_noRT)
skim(twitter_merged_noRT)
# What are the missing values for: user_id(19), favorite_count(27687), retweet_count(42) create_at(25): 
twitter_merged_noRT[which(is.na(twitter_merged_noRT$retweet_count)),]
twitter_merged_noRT[which(is.na(twitter_merged_noRT$favorite_count)),]
dim(twitter_merged_noRT[which(is.na(twitter_merged_noRT$favorite_count)),]) #provenance of missing values --> Arc 

### View dataset ready for analysis: 
View(twitter_merged)
View(twitter_merged_noRT)

write.csv(twitter_merged, file = "./soc-twitter/twitter_merged_sheet.csv", row.names = FALSE)

#################################################

#### PART 2: analyse dataset
merge_A <- sample_n(twitter_merged, 10)
merge_B <- sample_n(twitter_merged, 50)
merge_C <- sample_n(twitter_merged, 100)

length(twitter_merged$screen_name)
length(unique(twitter_merged$screen_name))
length(which(duplicated(twitter_merged$screen_name)))
# Almost half have duplicates, i.e. tweet more than once!

merge_1 <- sample_n(twitter_merged_noRT, 10)
merge_2 <- sample_n(twitter_merged_noRT, 50)
merge_3 <- sample_n(twitter_merged_noRT, 100)
#View(merge_3)
length(twitter_merged_noRT$screen_name) 
length(unique(twitter_merged_noRT$screen_name))
length(which(duplicated(twitter_merged_noRT$screen_name)))
# Almost half of users have have duplicates, i.e. tweet more than once!

# grouping by screenname! 

    # # View(sample_n(screenname_duplicates,10))
    # screenname_duplicates <- twitter_merged[which(duplicated(twitter_merged$screen_name)),]
    # screenname_unique <- twitter_merged[which(!duplicated(twitter_merged$screen_name)),]
    # screenname_duplicates_noRT <- twitter_merged_noRT[which(duplicated(twitter_merged_noRT$screen_name)),]
    # screenname_unique_noRT <- twitter_merged_noRT[which(!duplicated(twitter_merged_noRT$screen_name)),]

# Examples of screen names that tweeted multiple times:
grep("p1thywords", twitter_merged$screen_name, value = T) # 2
grep("ADAMA_CAN", twitter_merged$screen_name, value = T) # 5

#Retweet_count/Favorite_count grouping - w/RT
merge_C_retweets <- merge_C %>% 
  group_by(screen_name) %>% 
  summarise(retweet = sum(retweet_count)) %>% 
  arrange(- retweet) %>% 
  head(100)

merge_C_favorites <- merge_C %>% 
  group_by(screen_name) %>% 
  summarise(favorites = sum(favorite_count)) %>% 
  arrange(- favorites) %>% 
  head(100)

# Retweet_count/Favorite_count grouping - noRT
merge_3_retweets <- merge_3 %>% 
  group_by(screen_name) %>% 
  summarise(retweet = sum(retweet_count)) %>% 
  arrange(- retweet) %>% 
  head(15)
merge_3_retweets

merge_3_favorites <- merge_3 %>% 
  group_by(screen_name) %>% 
  summarise(favorites = sum(favorite_count)) %>% 
  arrange(- favorites) %>% 
  head(100)
merge_3_favorites

### bar plots 
# w/ RT
ggplot(head(merge_C_retweets, 20), aes(screen_name, retweet)) + 
  geom_bar(stat="identity", fill = 'blue', size=1 )+
  coord_flip()+
  theme_classic()
# Huge skew with RT of pope
ggplot(head(merge_C_favorites, 20), aes(screen_name, favorites)) + 
  geom_bar(stat="identity", fill='firebrick', size=1)+
  coord_flip()+
  theme_classic()

## No RT
ggplot(head(merge_3_retweets, 10), aes(screen_name, retweet)) + 
  geom_bar(stat="identity", fill = 'blue', size=1 )+
  coord_flip()+
  theme_classic()

ggplot(head(merge_3_favorites, 10), aes(screen_name, favorites)) + 
  geom_bar(stat="identity", fill='firebrick', size=1)+
  coord_flip()+
  theme_classic()

# Group_by query word data of provenance API 
# number of tweets per  query word

query_time <- ggplot(query_df, aes(x=created_at, y=text))+ 
  geom_line(aes(color=query))

query_time
query_df <- twitter_merged
query_df_2 <- twitter_merged_noRT

min(twitter_merged$created_at)

query_count_df <- twitter_merged %>% 
  filter(provenance == "API") %>% 
  group_by(query) %>% 
  count()

ts_wRT <- query_df %>% group_by(query)
ts_noRT <- query_df_2 %>% group_by(query)

par(mfrow(1,2))
ts_plot(ts_wRT)
ts_plot(ts_noRT)

max(ts_noRT$created_at)

plot(query_count_df, x=query, y=n)

ggplot(query_count_df, aes(x=query, y=n))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_bw()

# time series by word:
require()
ts::(query_count_df)
ts_plot(merge_C)

ts_plot(merge_3, filter = "soil health")
 
  geom_point()
names(merge_3)

ts <- merge_3 %>% 
  select(created_at, text)
require(tstools)
install.packages("tstools")
tstools::tsplot(ts)

### new column with keywords from text ###
keywords <- paste0(c("soil health", "healthy soil", "#soilhealth", "#healthysoil", 
                     "soil quality", "soil fertility", "#soilquality", "#soilfertility",
                     "rangeland health","#rangelandhealth","healthy rangelands",
                     "#healthyrangelands"), collapse = "|")

# merged_csv$keywords <- regmatches(keywords, merged_csv$text) 

twitter_merged$keywords <- str_extract_all(twitter_merged$text, pattern = regex(keywords, ignore_case = T))             
twitter_merged$keywords <- lapply(twitter_merged$keywords,function(x) if(identical(x,
                                                       character(0))) NA_character_ else x) %>% 
  as.character() %>% 
  str_remove_all('[c()"]')

twitter_merged_noRT$keywords <- str_extract_all(twitter_merged_noRT$text, pattern = regex(keywords, ignore_case = T))             
twitter_merged_noRT$keywords <- lapply(twitter_merged_noRT$keywords,function(x) if(identical(x,
                                                                               character(0))) NA_character_ else x) %>% 
  as.character() %>% 
  str_remove_all('[c()"]')
head(twitter_merged_noRT)

########################
# UNUSED CODE:
# 
# ### Remove id:twitter and create unique_id -- render to full plot after verification
# sample_merged <- sample_n(twitter_merged3, 10)
# # Unique ID _ see if can be not by text:
# head(sample_merged)
# data.table::setDT(sample_merged)[, id := .GRP, by=text]
# 
# # anyDuplicated(sample_merged$id)
# 
# # remove id
# sample_merged$user_id <- as.character(sample_merged$user_id)
# sample_merged$user_id <- str_remove(sample_merged$user_id, "id:twitter.com:")
# 
# View(sample_merged)
# dim(twitter_API)
# ##checking for unique id in datasets
# 
# sapply(twitter_API, function(x) length(unique(x)))
# find_uniqueid_ARC<-sapply(snapp_twitterdata, function(x) length(unique(x)))
# 
# # function to create UID - not working atm
# # idmaker <- function(x){
# #   max.val = x*100
# #   count <- nchar(as.character(max.val))                       # find out how many 'numbers' each ID will have after the letter
# #   size <- paste("%0",count,"d",sep="")                        # set the variable to be fed into 'sprintf' to ensure we have leading 0's
# #   lets <- toupper(sample(letters,x, replace=T))               # randomising the letters 
# #   nums <- sprintf(size,sample(1:max.val)[1�])                # randominsing the numbers, and ensuing they all have the same number of characters
# #   ids <- paste(lets,nums,sep="")                              # joining them together
# #   return(ids)
# # }
# 
# dim(snapp_twitterdata_merge)
# dim(twitter_API_merge)
# 
# #All other columns that are not part of a particular group  
# rest <- snapp_twitterdata %>% 
#   select(-starts_with("gnip"), -starts_with("long_object"),
#          -starts_with("twitter"),
#          -starts_with("provider."), -starts_with("generator"),
#          -starts_with("actor"), -starts_with("object"))
# 
# #### Run API twitter datasets 
# # What does the data look like: 
# skim(twitter_API)
# class(twitter_API$stat)
# 
# # reduce twitter_API size
# twitter_API_select <- twitter_API %>% 
#   select("screen_name", "is_retweet", "retweet_status_id",
#          "retweet_count", "mention_screen_name")
# 
# names(twitter_API)
# #object$posted
# names(actor)
# names(object)
# write.csv(twitter_API,
#           file="/home/sleckman/R/soc-twitter1/twitterAPI-excel.csv")
# 
# #   ###Verifying NAs total in each subDF
# #   #object
# #   unique(object)
# # sum(is.na(twitter.object.full$object.id)) 
# # #: number: 23479
# # #actor
# # unique(twitter.actor.full)
# # sum(is.na(twitter.actor.full$actor.displayName))
# # #object
# # unique(twitter.generator.full)
# # sum(is.na(twitter.generator.full$generator.displayName))
# # #provider
# # unique(twitter.provider.full)
# # sum(is.na(twitter.provider.full$provider.displayName))
# 
# categorical_frequency <- function(df){
#   # Select the character columns
#   cat_data <- Filter(is.character, df)
#   # Compute the unique values and frequencies
#   cat_freq_list <- map(cat_data, janitor::tabyl)
#   # Create a data frame out of that
#   cat_freq <- bind_rows(cat_freq_list, .id = "attribute")
#   names(cat_freq)[[2]] <- "category"
#   # add the total number of categories
#   cat_freq <- cat_freq %>% 
#     group_by(attribute) %>%
#     mutate(ntotal_cat = n()) %>%
#     ungroup() %>%
#     #remove the one that are unique for each row (here 90%), ie no categories
#     # Note: need to be improved regarding field with NAs
#     filter(ntotal_cat < nrow(df)*0.9)
#   
#   return(cat_freq)
# }
# catfreq <- categorical_frequency(twitter_API)
# 
# ###########################################
# # Identify duplicates in the names column
# 
# displaynameAPI<- twitter_API %>% 
#   select(screen_name)
# 
# dim(displaynameAPI)
# 
# displaynameArc <- snapp_twitterdata %>% 
#   select(actor.preferredUsername)
# 
# dataframe_tweet <- smartbind(displaynameAPI, displaynameArc)
# dim(dataframe_tweet)
# 
# match(displaynameAPI, displaynameArc)
# index <- as.factor(displaynameAPI) %in% as.factor(displaynameArc)
# 
# displaynames_df <- rbind(displaynameAPI, displaynameArc)
# 
# anyDuplicated(displaynameAPI)
# anyDuplicated(displaynameArc)
# # find similar screen names
# 
# grep(pattern = twitter_API$screen_name, x = displaynameArc)
# 
# names_unique <- as.data.frame(duplicated(twitter_API$screen_name,
#                                       snapp_twitterdata$actor.displayName)) 
#                             
# 
# duplicates <- which(duplicated(c(displaynameAPI, displaynameArc)))
# dataframe_tweet$col2 <- ifelse(displaynameAPI %in% displaynameArc, 1, 0)
# 
# displayname<-merge(displaynameArc, displaynameAPI)
# 
# #
# 
# random_id(10)
#   