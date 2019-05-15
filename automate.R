# Twitter
# API only has data going back 6-9 days
# devtools::install_github("mkearney/rtweet")
library(tidyverse)
library(rtweet)
library(countrycode)
library(lubridate)


path <- '/home/shares/soilcarbon/Twitter'


## Read previous data ----

# Master files 
twitter_merged.master <- read.csv(file.path(path, '/Merged_data/twitter_merged.csv'), stringsAsFactors = FALSE)
twitter_merged_noRT.master <- read.csv(file.path(path, '/Merged_data/twitter_merged_noRT.csv'), stringsAsFactors = FALSE)


## Query the Twitter API for the latest data ----

# Create token
twitter_token <- readRDS('twitter_token.rds')

# Search twitter
q <- c('"soil health"', '"healthy soil"', '#soilhealth', '#healthysoil', 
       '"soil quality"', '"soil fertility"', '#soilquality', '#soilfertility',
       '"rangeland health"','#rangelandhealth','"healthy rangelands"',
       '#healthyrangelands')

# Searching tweets with query above
twitterAPI_new.prac <- search_tweets2(q, n = 100000, token=twitter_token, retryonratelimit = T)

# Make it a data frame
twitterAPI_new <- as.data.frame(twitterAPI_new.prac, stringsAsFactors = FALSE)  #JB hashtags are a vector, you should collapse with paste it 

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
                         query)
# Remove row names
rownames(twitterAPI_new) <- NULL

######### JB
# We should save the dataframe to csv at this point appending the date of the day to the name.
######### JB

######### JB
# We should add a check on potential duplicates with the main data set as we want to run the script weekly
######### JB


## Prepare the data for the merge ----

# Changing the type of specific columns
twitterAPI_new$created_at <- as_datetime(twitterAPI_new$created_at)
# twitterAPI_new$user_id <- as.character(twitterAPI_new$user_id)

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

# Changing column name to country since we're not using country code anymore
names(twitterAPI_new)[names(twitterAPI_new) == 'country_code'] <- 'countryCode'

# Adding UID column (didn't use id function due to deprecation)
UID <- c((dim(twitter_merged.master)[1]+1):(dim(twitter_merged.master)[1]+dim(twitterAPI_new)[1]))
twitterAPI_new$UID <- UID

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
           map_chr(~glue::glue_collapse(.x, sep = ";")) %>%   # collapse the multiple hits/collapse instead of glue_collapse
           tolower) # all our keywords are lower case

twitterAPI_new_noRT <- twitterAPI_new_noRT %>%
  mutate(hits = str_extract_all(text, pattern = regex(keywords, ignore_case=TRUE)) %>%  # Extract all the keywords
           map(~charnull_set(.x)) %>%   # Replace character(0) with NAs
           map_chr(~glue::glue_collapse(.x, sep = ";")) %>%   # collapse the multiple hits/collapse instead of glue_collapse
           tolower) # all our keywords are lower case


## Merging and exporting data ----

# Merging datasets together using rbind 
twitter_merged_new <- rbind(twitter_merged.master, twitterAPI_new)
twitter_merged_noRTnew <- rbind(twitter_merged_noRT.master, twitterAPI_new_noRT)

# Re-exporting new merged dataset to master csv
write.csv(twitter_merged_new, 'twitter_mergedPrac.csv', row.names = FALSE)
write.csv(twitter_merged_noRTnew, 'twitter_merged_noRTprac.csv', row.names = FALSE)

