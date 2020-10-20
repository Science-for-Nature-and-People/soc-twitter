# Script to clean data used for the manuscript


## Library
suppressPackageStartupMessages(library(tidyverse))
# library(rtweet)
library(glue)



# twitter_merged_noRT.master_fixed <- read.csv("/home/shares/soilcarbon/Twitter/Merged_vMan/twitter_merged_noRT_vMan.csv", stringsAsFactors = FALSE)
twitter_merged_noRT.master <- read.csv("/home/shares/soilcarbon/Twitter/Merged_vMan/backup/twitter_merged_noRT_v4.csv", stringsAsFactors = FALSE)
twitter_merged <- read.csv("/home/shares/soilcarbon/Twitter/Merged_vMan/backup/twitter_merged_v4.csv", stringsAsFactors = FALSE)


keyword_list <- read.csv('tag_list.csv', stringsAsFactors = FALSE)

# Mutate 'Hits' column with keyword hits from tweet text
# Function to replace `character(0)` with NAs as NULL values are dropped when flattening list
charnull_set <- function(x){
  p <- as_mapper(~identical(., character(0)))
  x[p(x)] <- NA
  return(x)
}

## NO RT ----
## Seems there were some problems with the hits
# Text bits to search through # keywords = query words
keywords_p1 <- keyword_list$tag_category
keywords_p2 <- keyword_list$tag_list # %>% filter(!str_detect(tag_list,"\\#")) %>% pull(tag_list) # removing the hastags
keywords <- paste(unique(c(keywords_p1, keywords_p2)), collapse="|")

# Store the matches as a new columns with words separated by `;`
twitter_noRT_fixed <- twitter_merged_noRT.master %>%
  mutate(hits = str_extract_all(text, pattern = regex(keywords, ignore_case=TRUE)) %>%  # Extract all the keywords
           map(~charnull_set(.x)) %>%     # Replace character(0) with NAs
           # map(~str_replace_all(.x, regex("\\W+"), " ")) %>%   
           map_chr(~glue::glue_collapse(unique(tolower(trimws(.x))), sep = ";")))  # collapse the multiple hits/collapse instead of glue_collapse

# Removing old quotes around the query terms
twitter_noRT_fixed$query <- gsub('“', '', twitter_noRT_fixed$query) %>% gsub('”', '', .) 

# saved the file
write.csv(twitter_noRT_fixed, 
          "/home/shares/soilcarbon/Twitter/Merged_vMan/twitter_merged_noRT_vMan.csv", 
          row.names =  FALSE)

### Master ----
## Seems there were some problems with the hits


# Store the matches as a new columns with words separated by `;`
twitter_fixed <- twitter_merged %>%
  mutate(hits = str_extract_all(text, pattern = regex(keywords, ignore_case=TRUE)) %>%  # Extract all the keywords
           map(~charnull_set(.x)) %>%     # Replace character(0) with NAs
           # map(~str_replace_all(.x, regex("\\W+"), " ")) %>%   
           map_chr(~glue::glue_collapse(unique(tolower(trimws(.x))), sep = ";")))  # collapse the multiple hits/collapse instead of glue_collapse

# Removing old quotes around the query terms
twitter_fixed$query <- gsub('“', '', twitter_fixed$query) %>% gsub('”', '', .) 

# saved the file
write.csv(twitter_fixed, 
          "/home/shares/soilcarbon/Twitter/Merged_vMan/twitter_merged_vMan.csv", 
          row.names =  FALSE)