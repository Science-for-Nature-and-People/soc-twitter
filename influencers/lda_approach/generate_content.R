#############################
### generate user content ###
#############################

#This code 
#~ 1. identifies all unique users from our full dataset(w/ RT)
#~ 2. compiles their past 100 tweets + their user descriptions
#~ 3. combines all that content into a single cell which will be treated as a single document when input into the sLDA model


### at the bottom of this code ~line 100, is the (still in progress) attempt at incorperating fwrite() into this code to minimize the time lost if an error occurs.
###~~ this code currently does write a csv that can be read back in, but I havn't backchecked to make sure that it's accurate and doing exactly what is expected
###~~   - of the 241  users that should have been queried, the csv that is returned has 154 rows, some of which are NA, 
###~~   - it makes sense that there would be less as some users are private or have deleted their accounts, but its hard to confirm if thats all we should have actually gotten
###~~     - next step might be to create a mock dataset to run through the loop instead of doing the get_timeline.

library(tidyverse)
library(stringr)
library(rtweet)
library(data.table)

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




####################################################################
############### proposed loop that incorperates fwrite #############
####################################################################





## generate list of unique users
users <- twitter_merged %>% 
  select(screen_name) %>% 
  unique()

# Add IDs to users for future referencing
users$ID <- seq(1,nrow(users),1)




#create empty vector to populate 

user_info <- list()


for(i in 1:241){
  
  user_info[[i]] <- get_timeline(users$screen_name[i], n = 100)
  
  ## assuming full rate limit at start, wait for fresh reset every 160 users - this is slightly conservative as we could theoritically go to 180, but the API sometime returns >100 tweets so we want to be careful
  ## before resetting
  if (i %% 170L == 0L) {
    
    ## merge into single data frame (do_call_rbind will preserve users data)
    user_info_df <- do_call_rbind(user_info)
    
    #select desired content
    user_content <- user_info_df %>% 
      select(screen_name, text, description)
    
    # add IDs
    user_id <- left_join(users, user_content, by = "screen_name") %>% 
      na.omit()
    
    #perform for all users
    num_users <- length(unique(user_id$ID))
    full_content <- lapply(1:num_users, combine_content)
    
    #unlist into dataframe
    all_content <- bind_rows(full_content)
    
    # replace all pipes (|) with "" so that we can use that as a delimiter
    all_content_no_pipes <- all_content %>% 
      mutate(content = str_replace_all(content, "|", ""))
    
    fwrite(all_content_no_pipes, 'user_content.csv', sep = "|")
    
    rl <- rtweet::rate_limit("get_timeline")
    Sys.sleep(as.numeric(rl$reset, "secs"))
  }
  
  ## print update message
  cat(i, " ")
  
}


foo <- read.csv('user_content.csv', sep = "|")
 

