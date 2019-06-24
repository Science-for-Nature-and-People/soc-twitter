## generate user content ###
library(tidyverse)
library(stringr)
library(rtweet)

twitter_merged <- read_csv("/home/shares/soilcarbon/Twitter/Merged_v2/twitter_merged_v2.csv")


## generate list of users
users <- twitter_merged %>% 
  select(screen_name) %>% 
  unique() %>% 
  group_by(screen_name) 

# Add IDs to users for future referencing
users$ID <- seq(1,nrow(users),1)

#get user info

#create empty vector to populate 
n <- nrow(users)
user_info <- list()

#retrieve user timeline data while navigating the rate limit
for (i in 1:nrow(users)) {
  user_info[[i]] <- get_timeline(users$screen_name[i], n = 100)
  
  ## assuming full rate limit at start, wait for fresh reset every 160 users - this is slightly conservative as we could theoritically go to 180, but the API sometime returns >100 tweets so we want to be careful
  if (i %% 170L == 0L) {
    rl <- rate_limit("get_timeline")
    Sys.sleep(as.numeric(rl$reset, "secs"))
  }
  ## print update message
  cat(i, " ")
}



## merge into single data frame (do_call_rbind will preserve users data)
user_info_df <- do_call_rbind(user_info)

#select desired content
user_content <- user_info_df %>% 
  select(screen_name, text, description)

# add IDs
user_id <- left_join(users, user_content, by = "screen_name") %>% 
  na.omit()

#function for creating single cell with all past 100 tweets + description for each user (to be used with lapply)
combine_content <- function(x) {
 
    #create empty data frame
   full_content <- data.frame()
    
   #filter for specific user
      user <- user_id %>% 
        filter(ID == x)
      
      # create new data frame with 
      full_content[1,'screen_name'] <- user$screen_name[1]
      full_content[1,'text'] <- paste(user$text[1:nrow(user)], collapse = " ")
      full_content[1, 'description'] <- user$description[1]
      
      full_content <- unite(full_content, col = 'content', text, description)
      
      return(full_content)
}

#perform for all users
full_content <- lapply(1:nrow(user_id), combine_content)
#unlist into dataframe
all_content <- bind_rows(full_content)










