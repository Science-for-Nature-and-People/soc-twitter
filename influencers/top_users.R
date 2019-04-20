library(rtweet)
library(tidyverse)


create_token(
  app = "monper_twitter_app",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret
)



twitter_merged_noRT <- read.csv("twitter_merged_noRT.csv", stringsAsFactors = FALSE) %>% 
  distinct()

partners <- read_csv("twitter-institutions.csv")



#remove the pope
noRT <- twitter_merged_noRT %>% 
  arrange(-retweet_count) %>% 
  filter(screen_name != "Pontifex")



#find top 100 users
#~ do this by counting number of retweets for each user- this might be a way of seeing ranking each users influence
top_100_users <- noRT %>% 
  group_by(screen_name) %>% 
  summarise(
    total_rt = sum(retweet_count)
  ) %>% 
  arrange(-total_rt) %>% 
  head(100)



#check overlap between this list and partners
user_overlap <- top_100_users$screen_name[top_100_users$screen_name %in% partners$Handle]
length(user_overlap)
# only 4 of TNC's partners are among the top 100 users
user_overlap
#"USDA_NRCS"    "nature_org"   "USDA"         "nobleresinst"



user_list <- noRT %>% 
  filter(screen_name %in% top_100_users$screen_name)


usr_df <- lookup_users(user_list$user_id)




