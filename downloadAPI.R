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

# Function to add retweet column to json file and then to merge
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
  
  # change country code to country name
  for (i in 1:length(twitter_merged$country_code)){
    if(twitter_merged$country_code[i] != "台灣" & nchar(twitter_merged$country_code[i]) <= 2 & !is.na(twitter_merged$country_code[i])) {
      twitter_merged$country_code[i] <- countrycode(twitter_merged$country_code[i], origin = "iso2c", destination = "country.name")
    }
  }
  
  # Rename country column as it is not a code anymore
  names(twitter_merged)[names(twitter_merged) == 'country_code'] <- 'country'
  
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
             tolower) # all our keywords are lower case
  
  twitter_merged_noRT <- twitter_merged_noRT %>%
    mutate(hits = str_extract_all(text, pattern = regex(keywords, ignore_case=TRUE)) %>%  # Extract all the keywords
             map(~charnull_set(.x)) %>%   # Replace character(0) with NAs
             map_chr(~glue::collapse(.x, sep = ";")) %>%   # collapse the multiple hits/collapse instead of glue_collapse
             tolower) # all our keywords are lower case
  
  
  # Write CSV!
  write.csv(twitter_merged, paste(path, '/twitter_merged_v2.csv', sep = ""))
  write.csv(twitter_merged_noRT, paste(path, '/twitter_mergednoRT_v2.csv', sep = ""))
}

# Function that downloads tweets from API
## csv.path is the path to the directory where all the API csv's are located
api_csv <- function(token.path, csv.path){
  
  # Create token
  twitter_token <- readRDS(token.path)
  
  # Search twitter
  q <- c('"soil health"', '"healthy soil"', '#soilhealth', '#healthysoil', 
         '"soil quality"', '"soil fertility"', '#soilquality', '#soilfertility',
         '"rangeland health"','#rangelandhealth','"healthy rangelands"',
         '#healthyrangelands')
  
  # Searching tweets with query above
  twitterAPI_new <- search_tweets2(q, n = 100000, token=twitter_token, retryonratelimit = T)
  twitterAPI_new <- as.data.frame(twitterAPI_new)
  
  # Delete useless rownames 
  rownames(twitterAPI_new) <- c()
  
  # Creating file name 
  file.name <- paste0(csv.path, Sys.Date(), '.csv')
  
  # Converting list columns to character columns to allow for writing to csv
  i <- sapply(twitterAPI_new, is.list)
  twitterAPI_new[i] <- lapply(twitterAPI_new[i], as.character)
  
  # Creating csv file
  write.csv(twitterAPI_new, file.name)
}

# Function to do data preprocessing on csv file and merge it to master files 
merge_master <- function(csv.file, master.file){
  
  # Master files 
  twitter_merged.master <- read.csv(paste(master.file, '/twitter_mergedPrac.csv', sep = ""), stringsAsFactors = FALSE)
  twitter_merged_noRT.master <- read.csv(paste(master.file, '/twitter_merged_noRTprac.csv', sep = ""), stringsAsFactors = FALSE)
  
  # Reading in csv file 
  twitterAPI_new <- read.csv(csv.file, stringsAsFactors = FALSE)
  
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
  
  # Changing the type of specific columns
  twitterAPI_new$created_at <- as_datetime(twitterAPI_new$created_at)
  twitterAPI_new$user_id <- as.character(twitterAPI_new$user_id)
  
  # Creating provenance columns w/ value as API
  namelist <- list(API = twitterAPI_new) 
  twitterAPI_new <- bind_rows(namelist, .id = "provenance")
  
  # Removing quotes from query columns
  twitterAPI_new  <- twitterAPI_new  %>%
    mutate(query = gsub("\"", "", query))
  
  # Replacing NA with ""
  is.na(twitterAPI_new$country_code) <- twitterAPI_new$country_code == ""
  is.na(twitterAPI_new$place_name) <- twitterAPI_new$place_name == ""
  
  # Change country code to country name
  for (i in 1:length(twitterAPI_new$country_code)){
    if(twitterAPI_new$country_code[i] != "台灣" & nchar(twitterAPI_new$country_code[i]) <= 2 & !is.na(twitterAPI_new$country_code[i])) {
      twitterAPI_new$country_code[i] <- countrycode(twitterAPI_new$country_code[i], origin = "iso2c", destination = "country.name")
    }
  }
  
  # Changing column name to country since we're not using country code anymore
  names(twitterAPI_new)[names(twitterAPI_new) == 'country_code'] <- 'country'
  
  # Adding UID column (didn't use id function due to deprecation)
  UID <- c((dim(twitter_merged.master)[1]+1):(dim(twitter_merged.master)[1]+dim(twitterAPI_new)[1]))
  twitterAPI_new$UID <- UID
  
  # Removing any duplicates with the same values in the five columns chosen (I did this instead of doing duplicated function due to UID column)
  uniqueRows <- !(do.call(paste0, twitterAPI_new[,c("created_at", "user_id", "screen_name", "text", "source")]) %in% 
                    do.call(paste0, twitter_merged.master[,c("created_at", "user_id", "screen_name", "text", "source")]))
  twitterAPI_new <- twitterAPI_new[uniqueRows,]
  
  # Creating new dataframe with no RTs (if error exists, restart R session and run code again)
  twitterAPI_new_noRT <- twitterAPI_new %>% 
    filter(!str_detect(text, "^RT")) # ^ used to select only RT at start of text
  
  # Mutate 'Hits' column with keyword hits from tweet text
  # Function to replace `character(0)` with NAs as NULL values are dropped when flattening list
  charnull_set <- function(x){
    p <- as_mapper(~identical(., character(0)))
    x[p(x)] <- NA
    return(x)
  }
  
  # Text bits to search through # keywords = query words
  keywords <- paste0(c("soil health", "healthy soil", "#soilhealth", "#healthysoil", 
                       "soil quality", "soil fertility", "#soilquality", "#soilfertility",
                       "rangeland health","#rangelandhealth","healthy rangelands",
                       "#healthyrangelands"), collapse = "|")
  
  ## Store the matches as a new columns with words seprated by `;`
  twitterAPI_new <- twitterAPI_new %>%
    mutate(hits = str_extract_all(text, pattern = regex(keywords, ignore_case=TRUE)) %>%  # Extract all the keywords
             map(~charnull_set(.x)) %>%   # Replace character(0) with NAs
             map_chr(~glue::collapse(.x, sep = ";")) %>%   # collapse the multiple hits/collapse instead of glue_collapse
             tolower) # all our keywords are lower case
  
  twitterAPI_new_noRT <- twitterAPI_new_noRT %>%
    mutate(hits = str_extract_all(text, pattern = regex(keywords, ignore_case=TRUE)) %>%  # Extract all the keywords
             map(~charnull_set(.x)) %>%   # Replace character(0) with NAs
             map_chr(~glue::collapse(.x, sep = ";")) %>%   # collapse the multiple hits/collapse instead of glue_collapse
             tolower) # all our keywords are lower case
  
  # Merging datasets together using rbind 
  twitter_merged_new <- rbind(twitter_merged.master, twitterAPI_new)
  twitter_merged_noRTnew <- rbind(twitter_merged_noRT.master, twitterAPI_new_noRT)
  
  # Re-exporting new merged dataset to master csv
  write.csv(twitter_merged_new, paste(master.file, '/twitter_mergedPrac.csv', sep = ""))
  write.csv(twitter_merged_noRTnew, paste(master.file, '/twitter_merged_noRTprac.csv', sep = ""))
}
