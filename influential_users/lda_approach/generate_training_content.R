#################################
### generate training content ###
#################################

# this is essentially the same code as generate_content.R but only gets the past 100 tweets for the users that we mannually grouped.
# this was so i could start testing the model during the 7+ days it takes to querry the API for all the users in our dataset

#This code 
#~ 1. identifies all unique users from our full dataset(w/ RT)
#~ 2. compiles their past 100 tweets + their user descriptions
#~ 3. combines all that content into a single cell which will be treated as a single document when input into the sLDA model

# Note: the final variable (training_content) is needed in the training_data.R script

library(tidyverse)
library(rtweet)
library(tidytext)

twitter_merged <- read_csv("/home/shares/soilcarbon/Twitter/Merged_v2/twitter_merged_v2.csv")

test_groups <- read_csv("manual_grouping_data/known_groups.csv") %>% 
     select(-X1)
  


#################### Step 1: id users ######################

## generate list of unique users
users <- twitter_merged %>% 
  select(screen_name) %>% 
  unique() 

# Add IDs to users for future referencing
users$ID <- seq(1,nrow(users),1)


training_users <- left_join(test_groups, users, by = 'screen_name')


#################### Step 2: get content ######################

#create empty vector to populate 
user_info <- list()

#retrieve user timeline data while navigating the rate limit
for (i in 1:nrow(training_users)) {
  user_info[[i]] <- get_timeline(training_users$screen_name[i], n = 100)
  
  ## assuming full rate limit at start, wait for fresh reset every 160 users - this is slightly conservative as we could theoritically go to 180, but the API sometime returns >100 tweets so we want to be careful
  if (i %% 160L == 0L) {
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
training_content <- lapply(1:nrow(user_id), combine_content)
#unlist into dataframe
training_content <- bind_rows(training_content)

write.csv(training_content, "training_content.csv")













