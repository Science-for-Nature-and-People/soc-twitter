#### generate data for network analysis for followers of top 1000 users
library(rtweet)
library(tidyverse)
source("text_analysis_functions.R")

noRT <- read.csv("/home/shares/soilcarbon/Twitter/Merged_v3/twitter_merged_noRT_v3.csv", stringsAsFactors = FALSE)


## get top 1000 users based on total number of retweets per user
top_RT <- noRT %>% 
  filter(is_india == 0 & screen_name != "Pontifex") %>% 
group_by(screen_name) %>% 
  dplyr::summarise(
    total_RT = sum(retweet_count)
  ) %>% 
  arrange(desc(total_RT)) %>% 
  head(1000)

users <- top_RT$screen_name
# wrtie to csv for use on local R
# users <- as.data.frame(users)
# write_csv(users, "user_list.csv")

#### code for getting friends
for (i in 1:length(users)){
  if (i == 1){
    friends <- get_friends(users[i])
  } else {
    tmp <- get_friends(users[i])
    friends <- rbind(friends, tmp)
  }


  # pause if divisible by 15
  if (i %% 15 == 0){
    write_csv(friends, "user_friends.csv")
    print(paste("last i =", i, "------", round((1000-i)/15*4,2), "hours remaing"))
    Sys.sleep(15*61)
  }
}


