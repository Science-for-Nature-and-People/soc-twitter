library(rtweet)
library(tidyverse)


## CONSTANTS ----

# path to the master files
path_shared <- '/home/shares/soilcarbon/Twitter/' # Location of the shared folder on aurora


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

# Getting the token
twitter_token <- readRDS(file.path(path_shared,'twitter_token.rds'))




### use `content_creator` variable from the top_users.Rmd scripts
top_users <- read_csv("user_list.csv")
users <- top_users$users

# starts from the end to complement local run
users <- rev(users)


# start loop
for (i in 1:length(users)){
  # JB commenting out because added append = T to write_csv so we do not grow an huge object
  # if (i == 1){
  friends <- get_friends(users[i], token = twitter_token, retryonratelimit = TRUE) # rate limit can be reached for one user with a lot of followers 
  # } else {
  #   tmp <- get_friends(users[i])
  #   friends <- rbind(friends, tmp)
  # }
  # pause if divisible by 15
  # if (i %% 15 == 0){
  write_csv(friends, "user_friends.csv", append = TRUE) # appending data of each user at the time
  print(paste("last user ", i , " ------ ",users[i] ))
  Sys.sleep(61)
  # }
}