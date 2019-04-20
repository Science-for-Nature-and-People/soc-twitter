library(rtweet)
library(tidyverse)


create_token(
  app = "monper_twitter_app",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret
)



twitter_merged_noRT <- read.csv("twitter_merged_noRT.csv", stringsAsFactors = FALSE) %>% 
  distinct()
partners <- read_csv("twitter-institutions.csv")


#remove the pope
noRT <- twitter_merged_noRT %>% 
  arrange(-retweet_count) %>% 
  filter(screen_name != "Pontifex")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#find top 100 users

#~ counting number of retweets for each user- 
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


#create list of top users
user_list <- noRT %>% 
  filter(screen_name %in% top_100_users$screen_name)

#get user data
usr_df <- lookup_users(user_list$user_id)

usr_df %>%
  select(screen_name, name, location, description, followers_count, friends_count, favourites_count, statuses_count, listed_count, account_created_at)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#same anlysis but using favorites_count as a metric
top_100_favs <- noRT %>% 
  group_by(screen_name) %>%
  summarise(
    total_fav = sum(favorite_count)) %>% #add remove NA
  arrange(-total_fav) %>% 
  head(100)


user_fav_overlap <- top_100_favs$screen_name[top_100_favs$screen_name %in% partners$Handle]
length(user_fav_overlap)
# 0 TNC's partners are among the top 100 users by this metric


fav_list <- noRT %>% 
  filter(screen_name %in% top_100_favs$screen_name)

fav_df <- lookup_users(fav_list$user_id)

fav_df %>%
  select(screen_name, name, location, description, followers_count, friends_count, favourites_count, statuses_count, listed_count, account_created_at)


