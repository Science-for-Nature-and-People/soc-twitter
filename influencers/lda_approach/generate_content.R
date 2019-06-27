#############################
### generate user content ###
#############################

#This code 
#~ 1. identifies all unique users from our full dataset(w/ RT)
#~ 2. compiles their past 100 tweets + their user descriptions
#~ 3. combines all that content into a single cell which will be treated as a single document when input into the sLDA model


library(tidyverse)
library(stringr)
library(rtweet)

#read in auto-updated data
twitter_merged <- read_csv("/home/shares/soilcarbon/Twitter/Merged_v2/twitter_merged_v2.csv")


#################### Step 1: id users ######################

## generate list of unique users
users <- twitter_merged %>% 
  select(screen_name) %>% 
  unique()

# Add IDs to users for future referencing
users$ID <- seq(1,nrow(users),1)


#################### Step 2: get content ######################

#create empty vector to populate 
n <- nrow(users)
user_info <- list()

#retrieve user timeline data while navigating the rate limit
for (i in 1:nrow(users)) {
  user_info[[i]] <- get_timeline(users$screen_name[i], n = 100)
  
  ## assuming full rate limit at start, wait for fresh reset every 160 users - this is slightly conservative as we could theoritically go to 180, but the API sometime returns >100 tweets so we want to be careful
  if (i %% 170L == 0L) {
    rl <- rtweet::rate_limit("get_timeline")
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



#################### Step 3: combine into single cell/'document' ######################

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

write.csv(all_content, "user_content.csv")








