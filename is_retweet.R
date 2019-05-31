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

file.path <- '/home/nolasco/soc-twitter' # LOCATION OF MASTER FILES

# Function to add retweet column to json file and then to merge (ONLY RUN THIS FUNCTION IF MASTER FILE IS NOT UPDATED)
## path is where the location of the directory which contains API_csv and where you want to put the master files
is_retweet <- function(path){
  # Reading in json
  snapp_twitterdata_raw <- stream_in("/home/shares/soilcarbon/Twitter/twitter.json")
  
  # Remove all NA rows  
  snapp_twitterdata <- snapp_twitterdata_raw %>% 
    filter(!is.na(body))
  
  # Determining whether it was retweet if body column starts with RT AND object.body isn't NA
  retweet <- ifelse(str_detect(snapp_twitterdata$body, "^RT") & !is.na(snapp_twitterdata$object.body), TRUE, FALSE)
  
  # Adding retweet column to json
  snapp_twitterdata$is_retweet <- retweet
  
  # Creating hashtag_text column
  snapp_twitterdata <- snapp_twitterdata %>%
    unite("hashtag_text", grep("twitter_entities.hashtags.*text", names(snapp_twitterdata)), sep = "|", remove = F)
  snapp_twitterdata$hashtag_text <- gsub("\\|NA|NA|^\\|", "", snapp_twitterdata$hashtag_text)
  
  # Selecting columns
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
           location.country_code,
           is_retweet) %>%
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
                   "query",
                   "is_retweet")) 
  
  # Remove id:twitter.com in user id
  snapp_twitterdata_merge$user_id <- str_remove(snapp_twitterdata_merge$user_id, "id:twitter.com:")
  
  # Dates
  snapp_twitterdata_merge$created_at <- str_remove(snapp_twitterdata_merge$created_at, ".000")
  
  # Convert create_at date to date format
  snapp_twitterdata_merge$created_at <- as_datetime(snapp_twitterdata_merge$created_at)
  
  # Populate Query column in with query words from Twitter.R (API script)
  query_ARC <- str_match(
    snapp_twitterdata_merge$text,
    pattern = regex('soil health|healthy soil|#soilhealth|#SoilHealth|#healthysoil|soil quality|soil fertility|#soilquality|#soilfertility|rangeland health|#rangelandhealth|healthy rangelands|#healthyrangelands',
                    ignore_case=TRUE))
  
  snapp_twitterdata_merge$query <- paste0(query_ARC)           ##, sep = "|", collapse = NULL)
  
  #Fix NA in query - turn character "NA' into logical NA
  is.na(snapp_twitterdata_merge$query) <- snapp_twitterdata_merge$query == "NA"
  
  # Run the bash script to remove EOL (UNCOMMENT ONLY IF PATH ISN'T FIXED)
  #system("sh fix_tweet.sh") # do not edit with RStudio, used CLI tools such as `vim`
  
  # read the files in
  twitter_API <- do.call(dplyr::bind_rows,
                         lapply(list.files(path = paste(path, "/API_csv", sep = ""),
                                           pattern="^fixed_",
                                           full.names=TRUE), function(x) {read.csv(x, stringsAsFactors =FALSE)}))
  
  # DF build for merge
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
           query,
           is_retweet) %>% distinct()
  
  # Dates: 
  # Convert create_at date to date format
  twitter_API_merge$created_at <- as_datetime(twitter_API_merge$created_at)
  
  # ID conversion: 
  twitter_API_merge$user_id <- as.character(twitter_API_merge$user_id) # char --> num
  
  # Created a provenance column and a unique id column
  namelist <- list (API = twitter_API_merge, ARC = snapp_twitterdata_merge) # For merged (i.e. bind_rows), removed remove id column for Archived Dataset
  
  # DF with T and RT
  twitter_merged <- bind_rows(namelist, .id = "provenance")
  twitter_merged <- twitter_merged %>%
    mutate(UID = dplyr::id(twitter_merged, drop = FALSE)) %>% 
    mutate(query = gsub("\"", "", query))
  
  # Country/Place name edits
  is.na(twitter_merged$country_code) <- twitter_merged$country_code == ""
  is.na(twitter_merged$place_name) <- twitter_merged$place_name == ""
  
  # Create empty column called 'country'
  twitter_merged['country'] <- NA
  
  # change country code to country name
  for (i in 1:length(twitter_merged$country_code)){
    if(twitter_merged$country_code[i] != "台灣" & nchar(twitter_merged$country_code[i]) <= 2 & !is.na(twitter_merged$country_code[i])) {
      twitter_merged$country[i] <- countrycode(twitter_merged$country_code[i], origin = "iso2c", destination = "country.name")
    }
  }
  
  # DF with RT removed
  twitter_merged_noRT <- twitter_merged %>% 
    filter(is_retweet == FALSE) # ^ used to select only RT at start of text. subs with "starts_with()"
  
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
             tolower) %>% # all our keywords are lower case
        distinct()
  
  twitter_merged_noRT <- twitter_merged_noRT %>%
    mutate(hits = str_extract_all(text, pattern = regex(keywords, ignore_case=TRUE)) %>%  # Extract all the keywords
             map(~charnull_set(.x)) %>%   # Replace character(0) with NAs
             map_chr(~glue::collapse(.x, sep = ";")) %>%   # collapse the multiple hits/collapse instead of glue_collapse
             tolower) %>% # all our keywords are lower case 
        distinct()
  
  # Write CSV!
  write.csv(twitter_merged, paste(path, '/Merged_v2/twitter_merged_v2.csv', sep = ""), row.names =  FALSE)
  write.csv(twitter_merged_noRT, paste(path, '/Merged_v2/twitter_merged_noRT_v2.csv', sep = ""), row.names = FALSE)
}

is_retweet(file.path)