##################################################
# Twitter Influencers regarding Soil Health     ##
#                                               ##
# For SNAPP - Soil Organic Carbon Working Group ##
# Author: Margaux Sleckman                      ##
# Contact: scicomp@nceas.ucsb.edu               ##
##################################################


#soc-twitter on SNAPP version !!!

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

test[1,1:5]

test <- sample_n(snapp_twitterdata, 10)
test2 <- head(snapp_twitterdata, 10)

## remove all NA rows  
snapp_twitterdata <- snapp_twitterdata %>% 
  filter(!is.na(body))
nrow(snapp_twitterdata)

test3 <- head(snapp_twitterdata, 10)
sampleA <- sample_n(snapp_twitterdata, 100)
View(rbind(test2, test3)) # verify NA rows are removed

#API  ####

#' Run bash script on API files in /home/shares/soilcarbon/Twitter/rTweet/ before running. 
#' Used personal folder to retrieve fixed_datasets. substitute wd() to local
setwd("/home/sleckman/R/soc-twitter1")
list.files(path="soc-twitter/", pattern="^fixed_", full.names=TRUE)

twitter_API <- do.call(rbind,
                       lapply(list.files(path="soc-twitter/",
                                         pattern="^fixed_",
                                         full.names=TRUE), function(x) {read.csv(x, stringsAsFactors =FALSE)}))

class(twitter_API$created_at) # will need to convert
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
dim(object)
actor <- snapp_twitterdata %>% 
  select(starts_with("actor"))
dim(actor)
generator <- snapp_twitterdata %>% 
  select(starts_with("generator"))
dim(generator)
provider <- snapp_twitterdata %>% 
  select(starts_with("provider"))
dim(provider)
twitter <- snapp_twitterdata %>% 
  select(starts_with("twitter"))
dim(twitter)
long_object <- snapp_twitterdata %>% 
  select(starts_with("long_object"))
dim(long_object)
gnip <- snapp_twitterdata %>% 
  select(starts_with("gnip"))
dim(gnip)
info <- snapp_twitterdata %>% 
  select(starts_with("info"))
dim(info)
hashtags <- snapp_twitterdata %>% 
  select(actor.preferredUsername, body, contains("hashtag"))
dim(hashtags)

#only columns of class character
all_char <- snapp_twitterdata %>% 
  select(which(sapply(., is.character)))
dim(all_char)

# Find relevant columns from the archives to match to API dataset: 
columns <- as.character(names(snapp_twitterdata))  
grep(pattern = "Reply", columns, value = TRUE)
grep(pattern = "key", columns, value = TRUE) # --> no key word
grep(pattern = "status_id", columns, value = TRUE)
grep("quote|status|id", columns, value = TRUE)
grep("retweet", columns, value = TRUE)
grep("favorite", columns, value = TRUE)
hashtags <- grep("hashtags", columns, value = TRUE)

# hashtags 

snapp_twitterdata[,snapp_twitterdata[,grep("hashtag", names(snapp_twitterdata))]]

lapply(snapp_twitterdata[,grep("hashtag", colnames(snapp_twitterdata))],
       function(x){length(x)})  # looking at length of each hashtag column

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

#Retweet columns that will be merged
head(snapp_twitterdata$retweetCount)
head(twitter_API$retweet_count)

# favorite column to merge
head(twitter_API$favorite_count)
head(snapp_twitterdata$object.favoritesCount)
    #best match with favorite_count API --> object.favoritecount

### ARC + API URLS

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

###################MERGING_TABLES####################################
### Merge tables
##Cleaning

#ARC: #####

# Create hashtag column for ARC/json derived data
snapp_twitterdata<- snapp_twitterdata %>% 
  unite("hashtag_text", grep("twitter_entities.hashtags.*text", names(snapp_twitterdata)), sep = "|")
snapp_twitterdata$hashtag_text <- gsub("NA", "", snapp_twitterdata$hashtag_text)
snapp_twitterdata$hashtag_text<-gsub("^\\|+|\\|+$|+", "", snapp_twitterdata$hashtag_text)
View(head(snapp_twitterdata$hashtag_text))

snapp_twitterdata_merge <- snapp_twitterdata %>% 
  select(object.postedTime, actor.id,
         actor.preferredUsername, body, generator.displayName,
         object.favoritesCount, retweetCount, hashtag_text, id) %>%
  mutate(query = NA) %>%  # to populated
  set_colnames(c("created_at", "user_id","screen_name",
                 "text","source","favorite_count","retweet_count", "hashtags", "id", "query")) %>% 
  separate(id, into=c("id_text", "id_num"), sep="5:") # done to later remove id_text

names(snapp_twitterdata_merge)
str(snapp_twitterdata_merge$query) # nothin in query

# Remove id:twitter.com in user id
snapp_twitterdata_merge$user_id<- str_remove(snapp_twitterdata_merge$user_id, "id:twitter.com:")
head(snapp_twitterdata_merge$user_id)

# Remove additional zeros in date 
snapp_twitterdata_merge$created_at<- str_remove(snapp_twitterdata_merge$created_at, ".000")
head(snapp_twitterdata_merge$created_at)

# Convert create_at date to date format
snapp_twitterdata_merge$created_at<- as_datetime(snapp_twitterdata_merge$created_at)
class(snapp_twitterdata_merge$created_at)

class(snapp_twitterdata_merge$id_num)
length(unique((snapp_twitterdata_merge$id_num)))
length(nchar(snapp_twitterdata_merge$id_num)) 
# --> several duplicates exist in the ID column for twitter --> decided to create new UID for the dataset that is merged (select(c())).

names(snapp_twitterdata_merge)
str(snapp_twitterdata_merge)

#API: #####
head(unique(twitter_API$hashtags)) #this will be matched with the hashtag column created in the ARC column

twitter_API_merge <- twitter_API %>% 
  select(created_at, user_id, screen_name, text, source, favorite_count, retweet_count, hashtags, query)
names(twitter_API_merge)
# Convert create_at date to date format
twitter_API_merge$created_at <- as_datetime(twitter_API_merge$created_at)

# Adjust dfs to common column class  
str(twitter_API_merge)
str(snapp_twitterdata_merge)

matching_querywords <- sample_n(snapp_twitterdata_merge, 100)
querywords <- unique(twitter_API_merge$query)

#--- Fill query column with query words

query_ARC <- str_match(
  snapp_twitterdata_merge$text,
  pattern = '"soil health"|"healthy soil"|#soilhealth|#SoilHealth|#healthysoil|"soil quality"|"soil fertility"|#soilquality|#soilfertility|"rangeland health"|#rangelandhealth|"healthy rangelands"|#healthyrangelands') 

snapp_twitterdata_merge$query <- paste0(query_ARC) #, sep = "|", collapse = NULL)

View(head(snapp_twitterdata_merge$query))
View(snapp_twitterdata_merge$query[1:20, ])                                        

# to convert: 
twitter_API_merge$user_id <- as.character(twitter_API_merge$user_id) # char --> num

twitter_API$retweet_count[which(is.na(twitter_API_merge$retweet_count))]
class(twitter_API_merge$retweet_count)

##########MERGE###################

#Created a provenance column and a unique id column
namelist <- list( API = twitter_API_merge, ARC = snapp_twitterdata_merge[,-8:-9]) # For merged (i.e. bind_rows), removed remove id column for Archived Dataset

twitter_merged <- bind_rows(namelist, .id = "provenance")
twitter_merged <- twitter_merged %>% mutate(UID = id(twitter_merged, drop = FALSE))

twitter_merged_noRT <- bind_rows(namelist, .id = "provenance") 
twitter_merged_noRT <- twitter_merged_noRT %>% 
  mutate(UID = id(twitter_merged_noRT, drop = FALSE)) %>% 
  filter(!str_detect(text, "^RT")) # ^ used to select only RT at start of text. subs with "starts_with()"
                                   #note: issue with adding piping code lines on newly created df ...(to fix). Made two pipes sequences for now 

names(twitter_merged)

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
dim(twitter_merged[which(is.na(twitter_merged$favorite_count)),]) #provenance of missing values --> Arc 

### View dataset ready for analysis: 
    #View(twitter_merged)
    #View(twitter_merged_noRT)

write.csv(twitter_merged, file = "./soc-twitter/twitter_merged_sheet.csv", row.names = FALSE)

############Analysis of dataset retweets and favorites##############

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

# Examples of screen names that tweeted multiple times:
grep("p1thywords", twitter_merged$screen_name, value = T) # 2
grep("ADAMA_CAN", twitter_merged$screen_name, value = T) # 5

#Retweet_count/Favorite_count grouping - w/RT
top_retweet_user <- twitter_merged %>% 
  group_by(screen_name) %>% 
  summarise(retweet = sum(retweet_count)) %>% 
  arrange(- retweet) %>% 
  head(20)
View(top_retweet_user)

top_favorite_user <- twitter_merged %>% 
  group_by(screen_name) %>% 
  summarise(favorites = sum(favorite_count)) %>% 
  arrange(- favorites) %>% 
  head(20)
View(top_favorite_user)

# Retweet_count/Favorite_count grouping - noRT
top_retweets_user_noRT <- twitter_merged_noRT %>% 
  group_by(screen_name) %>% 
  summarise(retweet = sum(retweet_count)) %>% 
  arrange(- retweet) %>% 
  head(20)
View(top_retweets_user_noRT) 

top_favorites_user_noRT <- twitter_merged_noRT %>% 
  group_by(screen_name) %>% 
  summarise(favorites = sum(favorite_count)) %>% 
  arrange(- favorites) %>% 
  head(20)
View(top_favorites_user_noRT)

### bar plots 
# w/ RT
ggplot(head(top_retweet_user, 20), aes(screen_name, retweet)) + 
  geom_bar(stat="identity", fill = 'blue', size=1 )+
  coord_flip()+
  theme_classic()

# Huge skew with RT of pope
ggplot(head(top_favorite_user, 20), aes(screen_name, favorites)) + 
  geom_bar(stat="identity", fill='firebrick', size=1)+
  coord_flip()+
  theme_classic()

## No RT
ggplot(head(top_retweets_user_noRT, 10), aes(screen_name, retweet)) + 
  geom_bar(stat="identity", fill = 'blue', size=1 )+
  coord_flip()+
  theme_classic()

ggplot(head(top_favorites_user_noRT, 10), aes(screen_name, favorites)) + 
  geom_bar(stat="identity", fill='firebrick', size=1)+
  coord_flip()+
  theme_classic()

# Group_by query word data of provenance API 
# number of tweets per  query word

query_time <- ggplot(twitter_merged, aes(x=created_at, y=text))+ 
  geom_line(aes(color=query))

query_time
query_df <- twitter_merged
query_df_2 <- twitter_merged_noRT

min(twitter_merged$created_at)
names(twitter_merged)

query_count_df <- twitter_merged %>% 
  filter(query != 'NA') %>% 
  group_by(query) %>% 
  count() 

View(query_count_df)

query_count_df_noRT <- twitter_merged_noRT %>% 
  filter(query != 'NA') %>%
  group_by(query) %>% 
  count()

ts_wRT <- query_df %>% group_by(query)
ts_noRT <- query_df_2 %>% group_by(query)


par(mfrow(1,2))
ts_plot_wRT <- ts_plot(ts_wRT, background = NULL)
ts_plot_noRT <-ts_plot(ts_noRT)
ts_plot_wRT
par(bg='transparent')
plot(ts_plot_wRT)
ts_plot_noRT %>% 

theme_classic(ts_plot_noRT)

max(ts_noRT$created_at)

plot(query_count_df, x=query, y=n)

ggplot(query_count_df, aes(x=query, y=n))+
  geom_bar(stat = "identity", fill = "firebrick")+
  theme(axis.text.y = element_text(colour="grey20", size=12),
          axis.text.x = element_text(colour="grey20", size=12),
        element_blank(),
        element_rect())+ 
  coord_flip()

ggplot(query_count_df_noRT, aes(x=query, y=n))+
  geom_bar(stat = "identity", fill = "skyblue")+
  theme(axis.text.y = element_text(colour="grey20", size=15))+ 
  coord_flip()

##################### ANALYSIS OF WORD USE################

### Create new column with keyword hits from tweet text

keywords <- paste0(c("soil health", "healthy soil", "#soilhealth", "#healthysoil", 
                     "soil quality", "soil fertility", "#soilquality", "#soilfertility",
                     "rangeland health","#rangelandhealth","healthy rangelands",
                     "#healthyrangelands"), collapse = "|")

# Function to replace `character(0)` with NAs as NULL values are dropped when flattening list
# inspired by: https://colinfay.me/purrr-set-na/
charnull_set <- function(x){
  p <- as_mapper(~identical(., character(0)))
  x[p(x)] <- NA
  return(x)
}

# Text bits to search through

# keywords
keywords <- paste0(c("soil health", "healthy soil", "#soilhealth", "#healthysoil", 
                     "soil quality", "soil fertility", "#soilquality", "#soilfertility",
                     "rangeland health","#rangelandhealth","healthy rangelands",
                     "#healthyrangelands"), collapse = "|")

## Store the matches as a new columns with words seprated by `;`
twitter_merged <- twitter_merged %>%
  mutate(hits = str_extract_all(text, pattern = regex(keywords, ignore_case=TRUE)) %>%  # Extract all the keywords
           map(~charnull_set(.x)) %>%   # Replace character(0) with NAs
           map_chr(~glue::glue_collapse(.x, sep = ";")) %>%   # collapse the multiple hits
           tolower) # all our keywords are lower case

twitter_merged_noRT <- twitter_merged_noRT %>%
  mutate(hits = str_extract_all(text, pattern = regex(keywords, ignore_case=TRUE)) %>%  # Extract all the keywords
           map(~charnull_set(.x)) %>%   # Replace character(0) with NAs
           map_chr(~glue::glue_collapse(.x, sep = ";")) %>%   # collapse the multiple hits
           tolower) # all our keywords are lower case

View(sample_n(twitter_merged_noRT,10))

########################
