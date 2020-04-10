# Twitter
# API only has data going back 6-9 days
# devtools::install_github("mkearney/rtweet")




## LIBRARIES ----
suppressPackageStartupMessages(library(tidyverse))
library(rtweet)
library(countrycode)
library(lubridate)
library(jsonlite)
library(streamR)
library(httr)


## DO NOT run the write.csv lines when making edits to script ----
# (make a seperate path in your own home folder on aurora)  


## CONSTANTS ----

# path to the master files
path_shared <- '/home/shares/soilcarbon/Twitter/' # Location of the shared folder on aurora

# master files
master_data <- "Merged_v3/twitter_merged_v3.csv"
master_data_noRT <-"/Merged_v3/twitter_merged_noRT_v3.csv"

# Get the path to folder for cron job
args <- commandArgs(trailingOnly = TRUE)
script_dir <- as.character(args[1])

# Build the path to the script location
if (is.na(script_dir)) {
  path_local <- ''
} else {
  path_local <- script_dir
}

# Source the functions
source(paste0(path_local, "text_analysis_functions.R")) # use paste0 instead of file.path to handle the local run




## READ PREVIOUS (MASTER) DATA ----


# Master files 
twitter_merged.master <- read.csv(paste(path_shared, master_data, sep = ""), stringsAsFactors = FALSE) 
twitter_merged_noRT.master <- read.csv(paste(path_shared, master_data_noRT, sep = ""), stringsAsFactors = FALSE) 


# twitter_merged.master <- flag_india(twitter_merged.master) # one time fix (used 2019/09/06)
# twitter_merged_noRT.master <- flag_india(twitter_merged_noRT.master) # one time fix (used 2019/09/06)




## **QUERY** TWITTER API FOR LAST 6-9 DAYS OF TWEET DATA ----

# Read the Twitter API token
twitter_token <- readRDS(file.path(path_shared,'twitter_token.rds'))

# Import tag_list.csv (this contains the keywords to be used in search query of twitter data)
keyword_list <- read.csv(paste0(path_local, 'tag_list.csv'), stringsAsFactors = FALSE)

# Take tag_list and put quotes around each element for the twitterAPI search below
q <- unname(sapply(keyword_list$tag_list, function(x) toString(dQuote(x))))

# Searching tweets with query above (THIS CODE SEARCHES TWITTER FOR TERMS LISTED IN tag_list OVER THE LAST 6-9 DAYS)
twitterAPI_new <- search_tweets2(q, n = 100000, token=twitter_token, retryonratelimit = T)

# Write the raw API response as a csv (including quoted tweet)
filename_raw <- paste0(path_shared, "/API_csv/raw_api_data/rawdata_", Sys.Date(), '.csv')
write_as_csv(x = twitterAPI_new, file_name = filename_raw)



## CLEAN NEW DATA ----

# Make it a data frame
twitterAPI_new <- as.data.frame(twitterAPI_new, stringsAsFactors = FALSE)

# Collapsing hashtags column 

# Flatten the list and collapse
new_hashtags <- twitterAPI_new$hashtags %>% map_chr(~paste(unlist(.x), collapse="|")) 

# overwrite the column
twitterAPI_new$hashtags <- new_hashtags


# Selecting columns we want to match the archive dataset
twitterAPI_new <- twitterAPI_new %>% 
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
         query,
         is_retweet)

# Remove row names
rownames(twitterAPI_new) <- NULL

# Remove quotes around query terms
twitterAPI_new$query <- str_remove_all(twitterAPI_new$query,"[^A-Za-z\\s\\#]+")




## **EXPORT** CLEANED RAW DATA ----

# saving data as .csv file
# Creating file name 
file.name <- paste0(path_shared, '/API_csv/', Sys.Date(), '.csv')

# Converting list columns to character columns to allow for writing to csv
i <- sapply(twitterAPI_new, is.list)
twitterAPI_new[i] <- lapply(twitterAPI_new[i], as.character)

# Creating csv file
write.csv(twitterAPI_new, file.name)




## PREPARE DATA FOR MERGE ----

# Changing the type of specific columns
twitter_merged.master$created_at <- as_datetime(twitter_merged.master$created_at)
twitter_merged_noRT.master$created_at <- as_datetime(twitter_merged_noRT.master$created_at)

# Creating provenance columns w/ value as API
twitterAPI_new <- add_column(twitterAPI_new, provenance = "API", .before = 1)  

# Replacing NA with ""
is.na(twitterAPI_new$country_code) <- twitterAPI_new$country_code == ""
is.na(twitterAPI_new$place_name) <- twitterAPI_new$place_name == ""

# Create empty column called 'country'
twitterAPI_new['country'] <- NA

# Change country code to country name
for (i in 1:length(twitterAPI_new$country_code)){
  if(twitterAPI_new$country_code[i] != "台灣" & nchar(twitterAPI_new$country_code[i]) <= 2 & !is.na(twitterAPI_new$country_code[i])) {
    twitterAPI_new$country[i] <- countrycode(twitterAPI_new$country_code[i], origin = "iso2c", destination = "country.name")
  }
}

# Adding UID column (didn't use id function due to deprecation)
UID <- c((dim(twitter_merged.master)[1]+1):(dim(twitter_merged.master)[1]+dim(twitterAPI_new)[1]))
twitterAPI_new$UID <- UID

# Mutate 'Hits' column with keyword hits from tweet text
# Function to replace `character(0)` with NAs as NULL values are dropped when flattening list
charnull_set <- function(x){
  p <- as_mapper(~identical(., character(0)))
  x[p(x)] <- NA
  return(x)
}

# Text bits to search through # keywords = query words
keywords <- paste0(keyword_list, collapse = "|")

# Store the matches as a new columns with words seprated by `;`
twitterAPI_new <- twitterAPI_new %>%
  mutate(hits = str_extract_all(text, pattern = regex(keywords, ignore_case=TRUE)) %>%  # Extract all the keywords
           map(~charnull_set(.x)) %>%   # Replace character(0) with NAs
           map_chr(~glue::glue_collapse(.x, sep = ";")) %>%   # collapse the multiple hits/collapse instead of glue_collapse
           tolower) %>% # all our keywords are lower case
  distinct()

# Flag tweets with HINDI 
twitterAPI_new <- flag_india(twitterAPI_new)


## SEPARATE INTO RT AND NORT DATA FRAMES ----
twitterAPI_new_noRT <- twitterAPI_new %>% 
  filter(is_retweet == FALSE)




## REMOVE OLD TWEETS THAT ARE DUPLICATES OF NEWLY SCRAPED TWEETS ----


# -- Removing duplicates within each dataframe (merged and new api) --

# we only really care about created_at, screen_name, and text. 
# take tweet with largest retweet count. if multiple, then pick the first one we see.
twitter_merged.master_nodup <- rm_dups(twitter_merged.master)
twitterAPI_new_nodup <- rm_dups(twitterAPI_new)
twitter_merged_noRT.master_nodup <- rm_dups(twitter_merged_noRT.master)
twitterAPI_new_noRT_nodup <- rm_dups(twitterAPI_new_noRT)



  
# -- Removing duplicates between the two dataframes -- 

# looking through new tweets and comparing to old tweets in master data frame then
# removing tweets from old df as to keep the most up-to-date tweets
uniqueRows <- !(do.call(paste0, twitter_merged.master_nodup[,c("created_at", "user_id", "screen_name", "text", "source")]) %in% 
                  do.call(paste0, twitterAPI_new_nodup[,c("created_at", "user_id", "screen_name", "text", "source")]))
twitter_merged.master <- twitter_merged.master_nodup[uniqueRows,]

uniqueRows_noRT <- !(do.call(paste0, twitter_merged_noRT.master_nodup[,c("created_at", "user_id", "screen_name", "text", "source")]) %in% 
                       do.call(paste0, twitterAPI_new_noRT_nodup[,c("created_at", "user_id", "screen_name", "text", "source")]))
twitter_merged_noRT.master <- twitter_merged_noRT.master_nodup[uniqueRows_noRT,]


message("<--------- Exporting data ---------->\n")

## MERGING AND **EXPORTING** DATA ----

# Merging datasets together using rbind 
twitter_merged_new <- rbind(twitter_merged.master, twitterAPI_new_nodup)
twitter_merged_noRTnew <- rbind(twitter_merged_noRT.master, twitterAPI_new_noRT_nodup)

# Re-exporting new merged dataset to master csv

write.csv(twitter_merged_new, file.path(path_shared, master_data), row.names = FALSE) # CHANGE NAME OF FILE TO YOUR MASTER FILE NAME
write.csv(twitter_merged_noRTnew, file.path(path_shared, master_data_noRT),  row.names = FALSE) # CHANGE NAME OF FILE TO YOUR MASTER FILE NAME

message("<--------- Run completed succesfully ---------->\n")
