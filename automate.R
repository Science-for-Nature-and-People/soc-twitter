# Twitter
# API only has data going back 6-9 days
# devtools::install_github("mkearney/rtweet")
library(tidyverse)
library(rtweet)
library(countrycode)
library(lubridate)
library(magrittr)
library(jsonlite)
library(streamR)
library(httr)

## DO NOT run the write.csv lines when making edits to script (make a seperate path in your own home folder on aurora)  

path <- '/home/shares/soilcarbon/Twitter/' # LOCATION OF MASTER FILES

## Read previous data ----

# Master files 
twitter_merged.master <- read.csv(paste(path, '/Merged_v2/twitter_merged_v2.csv', sep = ""), stringsAsFactors = FALSE) 
twitter_merged_noRT.master <- read.csv(paste(path, '/Merged_v2/twitter_merged_noRT_v2.csv', sep = ""), stringsAsFactors = FALSE) 

## Query the Twitter API for the latest data ----

# Create token
twitter_token <- readRDS('twitter_token.rds')

# Import tag_list.csv (this contains the words to be used in search query of twitter data)
tag_file = read.csv('tag_list.csv')

# Create a list from tag_list.csv
tag_list = as.character(tag_file$tag_list)

# Take tag_list and put quotes around each element for the twitterAPI search below
q = unname(sapply(tag_list, function(x) toString(dQuote(x))))

# Searching tweets with query above
twitterAPI_new.prac <- search_tweets2(q, n = 100000, token=twitter_token, retryonratelimit = T)

# Make it a data frame
twitterAPI_new <- as.data.frame(twitterAPI_new.prac, stringsAsFactors = FALSE)

# Collapsing hashtags column 
new_hashtags <- c()
for (i in c(1:nrow(twitterAPI_new))){
  new_hashtags <- c(new_hashtags, paste(unlist(twitterAPI_new$hashtags[i]), collapse = "|"))
}

twitterAPI_new$hashtags <- new_hashtags

# Selecting columns we want
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

######### JB (We should save the dataframe to csv at this point appending the date of the day to the name)
# Creating file name 
file.name <- paste0(path, '/API_csv/', Sys.Date(), '.csv')

# Converting list columns to character columns to allow for writing to csv
i <- sapply(twitterAPI_new, is.list)
twitterAPI_new[i] <- lapply(twitterAPI_new[i], as.character)

# Creating csv file
write.csv(twitterAPI_new, file.name)
######### JB

## Prepare the data for the merge ----

# Changing the type of specific columns
twitter_merged.master$created_at <- as_datetime(twitter_merged.master$created_at)
twitter_merged_noRT.master$created_at <- as_datetime(twitter_merged_noRT.master$created_at)
twitterAPI_new$created_at <- as_datetime(twitterAPI_new$created_at)

# Creating provenance columns w/ value as API
twitterAPI_new <- add_column(twitterAPI_new, provenance = "API", .before = 1)  

# Removing quotes from query columns
twitterAPI_new  <- twitterAPI_new  %>%
                          mutate(query = gsub("\"", "", query))

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

######### JB 
# Removing any duplicates with the same values in the five columns chosen (I did this instead of doing duplicated function due to UID column)
uniqueRows <- !(do.call(paste0, twitterAPI_new[,c("created_at", "user_id", "screen_name", "text", "source")]) %in% 
                  do.call(paste0, twitter_merged.master[,c("created_at", "user_id", "screen_name", "text", "source")]))
twitterAPI_new <- twitterAPI_new[uniqueRows,]
                  
######### JB

# Creating new dataframe with no RTs (if error exists, restart R session and run code again)
twitterAPI_new_noRT <- twitterAPI_new %>% 
                          filter(is_retweet == FALSE)

# Mutate 'Hits' column with keyword hits from tweet text
# Function to replace `character(0)` with NAs as NULL values are dropped when flattening list
charnull_set <- function(x){
  p <- as_mapper(~identical(., character(0)))
  x[p(x)] <- NA
  return(x)
}

# Text bits to search through # keywords = query words

keywords <- paste0(tag_list, collapse = "|")

## Store the matches as a new columns with words seprated by `;`
twitterAPI_new <- twitterAPI_new %>%
  mutate(hits = str_extract_all(text, pattern = regex(keywords, ignore_case=TRUE)) %>%  # Extract all the keywords
           map(~charnull_set(.x)) %>%   # Replace character(0) with NAs
           map_chr(~glue::glue_collapse(.x, sep = ";")) %>%   # collapse the multiple hits/collapse instead of glue_collapse
           tolower) %>% # all our keywords are lower case
              distinct()

twitterAPI_new_noRT <- twitterAPI_new_noRT %>%
  mutate(hits = str_extract_all(text, pattern = regex(keywords, ignore_case=TRUE)) %>%  # Extract all the keywords
           map(~charnull_set(.x)) %>%   # Replace character(0) with NAs
           map_chr(~glue::glue_collapse(.x, sep = ";")) %>%   # collapse the multiple hits/collapse instead of glue_collapse
           tolower) %>% # all our keywords are lower case %>%
              distinct()
              
## Merging and exporting data ----

# Merging datasets together using rbind 
twitter_merged_new <- rbind(twitter_merged.master, twitterAPI_new)
twitter_merged_noRTnew <- rbind(twitter_merged_noRT.master, twitterAPI_new_noRT)

# Re-exporting new merged dataset to master csv
write.csv(twitter_merged_new, paste(path, '/Merged6_v2/', 'twitter_merged_v2.csv', sep = ""), row.names = FALSE) # CHANGE NAME OF FILE TO YOUR MASTER FILE NAME
write.csv(twitter_merged_noRTnew, paste(path, '/Merged_v2/', 'twitter_merged_noRT_v2.csv', sep = ""), row.names = FALSE) # CHANGE NAME OF FILE TO YOUR MASTER FILE NAME




