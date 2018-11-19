##################################################
# Twitter Influencers regarding Soil Health     ##
#                                               ##
# Script 1: Data CLeaning/Processing            ##   
#                                               ##
# For SNAPP - Soil Organic Carbon Working Group ##
# Author: Margaux Sleckman                      ##
# Contact: scicomp@nceas.ucsb.edu               ##
##################################################

#soc-twitter on SNAPP version !!!

# Purpose of script:
# 1. read in R archival data from twitter twitter.json {found on aurora in: /home/shares/soilcarbon/Twitter/
# 2. read in R API data from twitter based on Twitter.R script {found on aurora in: /home/shares/soilcarbon/Twitter/
# 3. Understand both datasets (diff/sim) and gather most valuable information 
# 4. Clean datasets to enable a merge of both df
# 5. Merge (created a first dataset with all tweets (~130,000 tweets) and a second dataset without RT (~42,000 tweets))

# ARC: archived data derived from json file
# API: API data 

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
library(skimr)
library(janitor)
library(gtools)
library(magrittr)
library(data.table)
library(lubridate)
library(ids)
library(countrycode)

getwd()

########### I. READING_DATA #############################
# 1/ Reading json(ARC)/API twitter archival datasets ####
# a. Read in ARC    #####
snapp_twitterdata_raw <- stream_in("/home/shares/soilcarbon/Twitter/twitter.json")

#' The \code{stream_in} and \code{stream_out} functions implement line-by-line processing
#' of JSON data over a \code{\link{connection}}, such as a socket, url, file or pipe. JSON
#' streaming requires the \href{http://ndjson.org}{ndjson} format, which slightly differs
#' from \code{\link{fromJSON}} and \code{\link{toJSON}}, see details.
#' 
#' Notes: termed twitter.json as Archived (ARC) dataset in script
#' (1) Ensure path is linked to the soil-carbon twitter file
#' (2) VERY LARGE DF: avoid viewing - 3480 columns, 96553 obs.

## remove all NA rows  
snapp_twitterdata <- snapp_twitterdata_raw %>% 
  filter(!is.na(body))
  # nrow(snapp_twitterdata) #73074

# b. Read in API   ####

#' Run bash script on API files to remove Windows end of line ^M character
#' Used personal folder to create fixed_datasets. 

getwd() # verify your working directory ensure it is the github repo where fix_tweet.sh is stored
setwd("/home/nolasco/soc-twitter")

dir.create(path = "./API_csv", showWarnings = F)

# Copy the files from the shared directory to your repository
file.copy(list.files("/home/shares/soilcarbon/Twitter/rTweet/", "*.csv", full.names=T), "./API_csv/")

# run the bash script to remove EOL
system("sh fix_tweet.sh") # do not edit with RStudio, used CLI tools such as `vim`

# List the fixed files
list.files(path="./API_csv", pattern="^fixed_", full.names=TRUE)

# read the files in
twitter_API <- do.call(rbind,
                       lapply(list.files(path="./API_csv",
                                         pattern="^fixed_",
                                         full.names=TRUE), function(x) {read.csv(x, stringsAsFactors =FALSE)}))

lapply(list.files(path="./API_csv",
                  pattern="^fixed_",
                  full.names=TRUE), function(x) {read.csv(x, stringsAsFactors =FALSE)})

str(twitter_API) #check structure. mainly chr or num/int
class(twitter_API$created_at) # is.character() at the moment - will covert upon merge

#### 2/ Understanding the data ########
# a. Understanding Archived df (ARC): Splitting header categories ####

# Grouped the archive dataset into its json objects, upon which the initial dataset is nested. 
# Then run dim() to print to size of each group
# SEE http://support.gnip.com/sources/twitter/data_format.html for subgroup info

# Object: representing tweet being posted or shared
object <- snapp_twitterdata %>% 
  select(starts_with("object"))
dim(object)

# Actor: the Twitter User -  contains all metadata relevant to that user.
actor <- snapp_twitterdata %>% 
  select(starts_with("actor"))
dim(actor)

# generator: the utility used to post the Tweet - contains the utility name ("displayName") and a link ("link") for the source application generating the Tweet.
generator <- snapp_twitterdata %>% 
  select(starts_with("generator"))
dim(generator)

# Provider: the provider of the activity. This will contain an objectType ("service"), the name of the provider name, and a link to the provider's website ("link").
provider <- snapp_twitterdata %>% 
  select(starts_with("provider"))
dim(provider)

# Tweet entities: the entities object from Twitter's data format which contains lists of urls, mentions and hashtags. 
twitter_entities <- snapp_twitterdata %>% 
  select(starts_with("twitter_entities"))
dim(twitter_entities)

# Tweet extended entities: Twitter's native data format containing "media". This will be present for any Tweet where has data present in the "media" field, and will include multiple photos where present in the post.
twitter_extended_entities <- snapp_twitterdata %>% 
  select(starts_with("twitter_entities"))
dim(twitter_entities)

# Long_object: tweet long object (also refered to as extended tweet):
# If a Tweet body exceeds Tweet text length limit due to combination of hidden text and user-entered text, the root-level Tweet body will be truncated (as will its associated entities) and an additive extended_tweet object will be included in the payload and should be used to retrieve the full Tweet body and full set of associated entities.
long_object <- snapp_twitterdata %>% 
  select(starts_with("long_object"))
dim(long_object)

# gnip: data info associated with gnip (twitter's API data aggregation supporting service) 
# mainly codes
gnip <- snapp_twitterdata %>% 
  select(starts_with("gnip"))
dim(gnip)

# Info: activity and message details of tweet
info <- snapp_twitterdata %>% 
  select(starts_with("info"))
dim(info)

# hashtags: all column names that contain the word hashtag
hashtags <- snapp_twitterdata %>% 
  select(actor.preferredUsername, body, contains("hashtag"))
dim(hashtags)

# location: the Twitter "Place" where the tweet was created - objectType ("place"), displayName	(The full name of the place), 
location <- snapp_twitterdata %>% 
  select(starts_with("location"))
dim(location)
nrow(unique(location)) # 575 unique locations 
# note: specify location.displayname is equiv. to Place_name in the API dataset and provides us with more specific info about geog. location. 
# however, in comparison to country name, location.display name can be much more varied (i.e. 16 hits in "somewhere in Pacific ocean")

# geo: Point location where the Tweet was created.
geo <- snapp_twitterdata %>% 
  select(starts_with("geo"))
dim(geo)
nrow(unique(geo)) # 40 unique geo-coordinated

# All location related columns (for object, actor etc)
location_all <- snapp_twitterdata %>% 
  select(actor.preferredUsername, body, contains("location"))
dim(location)
# names(location)

#only columns of class 'character'
# (could look further into this for mapping analysis)
all_char <- snapp_twitterdata %>% 
  select(which(sapply(., is.character)))
dim(all_char)

### URLS - made specific dataset with all url-related column names from Archive, liked tweet and username 
#Select body and url so that we can determine how the different urls are linked to the tweet. 
urls <- snapp_twitterdata %>%
  select(actor.preferredUsername, body, grep("url", names(snapp_twitterdata), value = TRUE))
# NOTE: urls mainly present if in tweet, no column is a direct link the actual tweet

# Look at sum of NA per column in ARC to see what url column has the most NAs  - can be applied to other json group listed above
urls <- rbind(urls, as.data.frame(
  lapply(urls[-1:-2], function(x){
    sum(is.na(x))})))

# b. Find relevant columns from the ARC to match to API dataset: ####
  # names(twitter_API)
columns <- as.character(names(snapp_twitterdata))  
# grep(pattern = "Reply", columns, value = TRUE)
# grep(pattern = "key", columns, value = TRUE) # --> no key word
# grep(pattern = "status_id", columns, value = TRUE)
# grep("quote|status|id", columns, value = TRUE)
# grep("retweet", columns, value = TRUE)
# grep("favorite", columns, value = TRUE)
hashtags_columns <- grep("hashtags", columns, value = TRUE)
grep("location", columns, value = T)
unique(snapp_twitterdata$location.country_code)
grep('coor', names(twitter_API), value = T)
unique(twitter_API$geo_coords)
## hashtags: 
    # lapply(snapp_twitterdata[,grep("hashtag", colnames(snapp_twitterdata))],
    #        function(x){length(x)})  # looking at length of each hashtag column

# c. Understanding Favorites and Retweet Counts #### 
#In API:
retweet_API <- select(.data = twitter_API,
                      grep("retweet", names(twitter_API), value = TRUE))


dim(twitter_API)
# which(anyNA(retweet_API))
twitter_API

favorite_API <- select(.data = twitter_API,
                       grep("favorite", names(twitter_API), value = TRUE))

# which(anyNA(favorite_API))
# max(favorite_API$favorite_count, na.rm = T)
# max(retweet_API$retweet_count, na.rm = T)

#In Archived data:
favorites_ARC <- snapp_twitterdata %>% 
  select(grep("favorite",names(snapp_twitterdata), value = TRUE, ignore.case = T))
names(favorites_ARC) # seven columns with the word "favorite" in it:

retweets_ARC <- snapp_twitterdata %>% 
  select(grep("retweet",names(snapp_twitterdata), value = TRUE, ignore.case = T))
names(retweets_ARC) # ONE variable with the word "retweet in it. 

#Retweet columns that will be merged - check ballpark numbers
head(snapp_twitterdata$retweetCount)
head(twitter_API$retweet_count)

# favorite column to merge
head(twitter_API$favorite_count)
head(snapp_twitterdata$object.favoritesCount)
    #best match with favorite_count API --> object.favoritecount


################### II. Cleaning for merge ####################################

# 1/ ARC dataset cleaning for merge #####

# a. Create and clean hashtag column for ARC/json derived data - pull all hashtags from tweets in json file into one column 
# unite all content of all hashtag.text columns in ARC df. Separated with a `|`

snapp_twitterdata <- snapp_twitterdata %>%
  unite("hashtag_text", grep("twitter_entities.hashtags.*text", names(snapp_twitterdata)), sep = "|", remove = F)

sprintf(snapp_twitterdata$hashtag_text[45])

snapp_twitterdata$hashtag_text[45]

# Remove NA, and trailing `|`
gsub("\\|NA|NA|^\\|", "", snapp_twitterdata$hashtag_text)
snapp_twitterdata$hashtag_text <- gsub("\\|NA|NA|^\\|", "", snapp_twitterdata$hashtag_text)
sprintf(snapp_twitterdata$hashtag_text[45])

# verify hashtag edits
  # head(snapp_twitterdata_2$hashtag_text))
  # sample_hashtagtext <-sample_n(snapp_twitterdata, 10)
  # View(sample_hashtagtext$hashtag_text)

# b. ARC DF built for merged

snapp_twitterdata_merge <- snapp_twitterdata %>% 
  select(postedTime,
         actor.id,
         actor.preferredUsername,
         body,
         generator.displayName,
         object.favoritesCount,
         retweetCount,
         hashtag_text,
         location.displayName,
         location.country_code) %>%
  mutate(query = NA) %>%  # to be populated in next step
  set_colnames(c("created_at",
                 "user_id",
                 "screen_name",
                 "text",
                 "source",
                 "favorite_count",
                 "retweet_count",
                 "hashtags",
                 "place_name",
                 "country_code",
                 # "id",
                 "query")) 


# c. Remove id:twitter.com in user id
snapp_twitterdata_merge$user_id <- str_remove(snapp_twitterdata_merge$user_id, "id:twitter.com:")
# sprintf(head(snapp_twitterdata_merge$user_id)) 

# d. Dates
# Remove additional zeros in date 
max(snapp_twitterdata_merge$created_at) # fixed 2013 date issue

snapp_twitterdata_merge$created_at <- str_remove(snapp_twitterdata_merge$created_at, ".000")
# Convert create_at date to date format
snapp_twitterdata_merge$created_at <- as_datetime(snapp_twitterdata_merge$created_at)
class(snapp_twitterdata_merge$created_at)

# e. Query: 
# Populate Query column in with query words from Twitter.R (API script)
query_ARC <- str_match(
  snapp_twitterdata_merge$text,
  pattern = regex('soil health|healthy soil|#soilhealth|#SoilHealth|#healthysoil|soil quality|soil fertility|#soilquality|#soilfertility|rangeland health|#rangelandhealth|healthy rangelands|#healthyrangelands',
                  ignore_case=TRUE))

snapp_twitterdata_merge$query <- paste0(query_ARC)           ##, sep = "|", collapse = NULL)

sprintf(snapp_twitterdata_merge$query[70:80]) # Initial print, character NA . if overwritten, will display NA.

#Fix NA in query - turn character "NA' into logical NA
is.na(snapp_twitterdata_merge$query) <- snapp_twitterdata_merge$query == "NA"
head(snapp_twitterdata_merge$query)
sprintf(snapp_twitterdata_merge$query[70:80])

# f. country_code:
# is.na(snapp_twitterdata_merge$country_code) <- snapp_twitterdata_merge$country_code == NA
sprintf(snapp_twitterdata_merge$country_code[70:80])
sprintf(snapp_twitterdata_merge$country_code[1000:1020])

# Overall structure verification
  # names(snapp_twitterdata_merge)
  # str(snapp_twitterdata_merge)

# API dataset cleaning for merge: #####
head(unique(twitter_API$hashtags)) #this will be matched with the hashtag column created in the ARC column

# a. Df build for merge
twitter_API_merge <- twitter_API %>% 
  select(created_at,
         user_id,
         screen_name,
         text,
         source,
         favorite_count,
         retweet_count,
         hashtags,
         place_name,
         country_code,
         query)

# names(twitter_API_merge)

# b. Dates: 
# Convert create_at date to date format
twitter_API_merge$created_at <- as_datetime(twitter_API_merge$created_at)

# c. ID conversion: 
twitter_API_merge$user_id <- as.character(twitter_API_merge$user_id) # char --> num

# d. Verify both dfs have matching class  
str(twitter_API_merge)
str(snapp_twitterdata_merge)
  # matching_querywords <- sample_n(snapp_twitterdata_merge, 100)
  # querywords <- unique(twitter_API_merge$query)

  # twitter_API$retweet_count[which(is.na(twitter_API_merge$retweet_count))]
  # class(twitter_API_merge$retweet_count)

########## III. MERGE ###################

# a. Created a provenance column and a unique id column
namelist <- list( API = twitter_API_merge, ARC = snapp_twitterdata_merge) # For merged (i.e. bind_rows), removed remove id column for Archived Dataset

# b. DF with T and RT
twitter_merged <- bind_rows(namelist, .id = "provenance")
twitter_merged <- twitter_merged %>%
  mutate(UID = dplyr::id(twitter_merged, drop = FALSE)) %>% 
  mutate(query = gsub("\"", "", query)) # remove quotes from query so that query words can match

# c. Country/Place name edits
sprintf(head(unique(twitter_merged$country_code)), 10)
is.na(twitter_merged$country_code) <- twitter_merged$country_code == ""
sprintf(head(unique(twitter_merged$country_code), 10))

sprintf(head(unique(twitter_merged$place_name)), 10)
is.na(twitter_merged$place_name) <- twitter_merged$place_name == ""
sprintf(head(unique(twitter_merged$place_name), 10))

countrycode(twitter_merged$country_code[i], origin = "iso2c", destination = "country.name")
?countrycode
# change country code to country name
for (i in 1:length(twitter_merged$country_code)){
  if(twitter_merged$country_code[i] != "台灣" & nchar(twitter_merged$country_code[i]) <= 2 & !is.na(twitter_merged$country_code[i])) {
    twitter_merged$country_code[i] <- countrycode(twitter_merged$country_code[i], origin = "iso2c", destination = "country.name")
    }
}
# unique(twitter_merged$country_code)
unique(twitter_merged$country_code)
?codelist

# Rename country column as it is not a code anymore
# names(twitter_merged)
names(twitter_merged)[names(twitter_merged) == 'country_code'] <- 'country'

# d. DF with RT removed
# twitter_merged_noRT <- bind_rows(namelist, .id = "provenance") 
twitter_merged_noRT <- twitter_merged %>% 
  # mutate(UID = id(twitter_merged_noRT, drop = FALSE)) %>%
  # mutate(query = gsub("\"", "", query)) %>% 
  filter(!str_detect(text, "^RT")) # ^ used to select only RT at start of text. subs with "starts_with()"
                                   #note: issue with adding piping code lines on newly created df ...(to fix). Made two pipes sequences for now 

str(twitter_merged_noRT)

is.na(twitter_merged_noRT$country_code) <- twitter_merged_noRT$country_code == ""
is.na(twitter_merged_noRT$place_name) <- twitter_merged_noRT$place_name == ""

### d. Mutate 'Hits' column with keyword hits from tweet text ####

# Function to replace `character(0)` with NAs as NULL values are dropped when flattening list
# inspired by: https://colinfay.me/purrr-set-na/
charnull_set <- function(x){
  p <- as_mapper(~identical(., character(0)))
  x[p(x)] <- NA
  return(x)
}

# Text bits to search through # keywords = query words
# keywords
keywords <- paste0(c("soil health", "healthy soil", "#soilhealth", "#healthysoil", 
                     "soil quality", "soil fertility", "#soilquality", "#soilfertility",
                     "rangeland health","#rangelandhealth","healthy rangelands",
                     "#healthyrangelands"), collapse = "|")

## Store the matches as a new columns with words seprated by `;`
twitter_merged <- twitter_merged %>%
  mutate(hits = str_extract_all(text, pattern = regex(keywords, ignore_case=TRUE)) %>%  # Extract all the keywords
           map(~charnull_set(.x)) %>%   # Replace character(0) with NAs
           map_chr(~glue::collapse(.x, sep = ";")) %>%   # collapse the multiple hits/collapse instead of glue_collapse
           tolower) # all our keywords are lower case

twitter_merged_noRT <- twitter_merged_noRT %>%
  mutate(hits = str_extract_all(text, pattern = regex(keywords, ignore_case=TRUE)) %>%  # Extract all the keywords
           map(~charnull_set(.x)) %>%   # Replace character(0) with NAs
           map_chr(~glue::collapse(.x, sep = ";")) %>%   # collapse the multiple hits/collapse instead of glue_collapse
           tolower) # all our keywords are lower case

# e. Confirm UIDs

# which(duplicated(twitter_merged, fromLast = T))
# length(unique((twitter_merged$UID))) #128293
# length(twitter_merged$UID) # #129673 to verify...

## Analysis of structure for with RT and no RT:
# str(twitter_merged_noRT)
# skim(twitter_merged_noRT)
# Looking at missing values: 
# twitter_merged_noRT[which(is.na(twitter_merged_noRT$retweet_count)),]
# NA_fav_count_test <- twitter_merged_noRT[which(is.na(twitter_merged_noRT$favorite_count)),] # only twitter merged which has NA favorites
# unique(NA_fav_count_test$provenance) # --> all NA in favorite count from ARC, as df format differed. should we consider this ) favorites ?
# dim(twitter_merged_noRT[which(is.na(twitter_merged_noRT$favorite_count)),]) #provenance of missing values --> Arc
# dim(twitter_merged[which(is.na(twitter_merged$favorite_count)),]) #provenance of missing values --> Arc

## View dataset ready for analysis: 
  # View(sample_n(twitter_merged,10))
  # View(sample_n(twitter_merged_noRT,10))
  # View(sample_n(twitter_merged, 100))
  # View(sample_n(twitter_merged_noRT, 100))
  # View(twitter_merged)
  # View(twitter_merged_noRT)

names(twitter_merged)
str(twitter_merged_noRT)

# f. Write CSV!
saveRDS(twitter_merged, "/home/shares/soilcarbon/Twitter/twitter_merged")
saveRDS(twitter_merged_noRT, "/home/shares/soilcarbon/Twitter/twitter_merged_noRT")

# write.csv(twitter_merged, file = "./twitter_merged.csv", row.names = FALSE)
# write.csv(twitter_merged_noRT, file = "./twitter_merged_noRT.csv", row.names = FALSE)
