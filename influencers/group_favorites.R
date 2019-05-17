##################################
#     'group' favorites          #
##################################


### This script builds a function [ find_group_prop() ] that can quickly identify the proportion of which user groups retweeted any given tweet, this is to aid in my attempt at answering:
#~~ Are there differences in the content/message of these tweets that make them more appealing to these different groups?


library(tidyverse)
library(stringr)
library(rtweet)
library(wordcloud)
library(tidytext)

source("text_analysis_functions.R") # for word cloud creator

twitter_merged <- read.csv("twitter_merged.csv", stringsAsFactors = FALSE) %>%
  distinct()
twitter_merged_noRT <- read.csv("twitter_merged_noRT.csv", stringsAsFactors = FALSE) %>%
  distinct() %>%
  arrange(-retweet_count)


##### creating a column to flag tweets from or about India ####
RT <- twitter_merged %>% 
  mutate(
    is_india = case_when(
      str_detect(tolower(text), "[\u0900-\u097F]+|india|crore|health card|rupee|narendramodi|managed to feed 1.25 billion people|akshaykumar") ~ 1,
      str_detect(tolower(screen_name), "[\u0900-\u097F]+|india|crore|health card|rupee|narendramodi") ~ 1,
      str_detect(tolower(hashtags), "[\u0900-\u097F]+|india|crore|health card|rupee|narendramodi") ~ 1))

#replace na w/ 0 to indicate non-india related tweets
RT$is_india[is.na(RT$is_india)] <- 0


RT <- RT %>% 
  filter(!str_detect(tolower(text), "pontifex"), #remove the popes RTs
         screen_name != "Pontifex") 

### repeate for noRT
noRT <- twitter_merged_noRT %>% 
  mutate(
    is_india = case_when(
      str_detect(tolower(text), "[\u0900-\u097F]+|india|crore|health card|rupee|narendramodi|managed to feed 1.25 billion people|akshaykumar") ~ 1,
      str_detect(tolower(screen_name), "[\u0900-\u097F]+|india|crore|health card|rupee|narendramodi") ~ 1,
      str_detect(tolower(hashtags), "[\u0900-\u097F]+|india|crore|health card|rupee|narendramodi") ~ 1))

#replace na w/ 0
noRT$is_india[is.na(noRT$is_india)] <- 0


###### Word list (built within user_groups.R) #######
##~~this will likely get refined
science_WL <- c('ologist', 'science', 'scientist', 'university', 'data', 'studies', 'research', 'agronomy', 'institute')
farmer_WL <- c("farm", "rancher", "ranching", "cattle", "sheep")
govt_WL <- c("endorse", "nrcs", "usda", "gov", "public") 
business_WL <- c("company" , "business" ,"customer", "entrepreneur")
media_WL <- c("edit", "journalist")
environmental_WL <- c("conserv", "sustain", "water", "organic", "climate", "environment")




#### calculation proportion of likes by each group for tweets ####


#' This function takes a given tweet, Identifies the users who retweeted it, 
#' queries the API for details on those users and uses their 'descriptions' to place each user into a 'group' 
#' (government, environmentalist, media, farmer, scientist, business)  
#' then returns a dataframe that shows the proportion of users from each group retweeted that tweet 
#' --- this is meant to be a proxy for what tweets appeal most to which user groups
#' 
#' ~~This function was created to be used along with lapply so that numerous tweets could be analyzed at once~~
#'
#' 
#' this function relies on :
#' ~~~having both RT and noRT dataframes in your environment, 
#' ~~~the noRT has the `is_india` flag included
#' 
#'
#' @param x selects the row that will be used for analysis - it was set up this way so that it could be easily used in conjunction with `lapply()` i.e x <- 1:100 will return the top 100 tweets arranged by retweet_count
#' @param y input either 0 or 1 to either exclude or include india respectively 
#' @param category enter search term to filter noRT by, so that we can perform this easily for diferent categories. default set to no filter
#'
#'
#'
#' @return data frame with columns: 
#' 'group' (see 'user_group.R' for how these were created)
#' 'prop_like' - which is the propotion that each respective group RTed that given tweet
#' tweet_og - the original tweet that was used for analysis
#' handle_og - the original handle that was used for analysis
#' n - gives number of users that retweeted the `tweet_og` 
#' 
#'
#' @examples
#' x <- 1:20
#' prop_list <- lapply(x, find_group_prop) #if this throws an HTTP error, simply re-run this line untill it works, sometime the API doesn't play nice
#' prop_df <- bind_rows(prop_list, .id = "ID")
#' 
#'
find_group_prop <- function(x, category = "", y = 1) { # default set to not include india related tweets - there is a bug in the regex recognition, that is causing this to fail for when looping through the top 20+ tweets (when india is included)
  
  #filter for is_india & category - as defined by the function arguments
  noRT_filtered <- noRT %>% 
    filter(is_india == y |
             is_india == 0) %>% 
    filter(
      str_detect(tolower(text), category)) %>% 
    arrange(-retweet_count)
  
  #for the given tweet within the specified row (which is variable 'x' in the function), this will find each instance of a retweet - thereby giving us the users who retweeted it
  RT_users <- RT %>% 
    filter(
      str_detect(text,
                 fixed(
                   str_c(                                         
                   word(noRT_filtered[x,]$text, 1:6), collapse = ' '))))  #str_c() combines each word that has been individually selected by word() into a single string. This creates a six word string using the first six words of each tweet. this should be enough to uniquely ID instances of a RTs using str_detect
  
  
  #get user info via twitter API
  user_info <- lookup_users(unique(RT_users$user_id))
  
  #### categorize users into groups
  #detects any words from relevent word list (hence paste(x,collapse="|"))
  user_group <- user_info %>%
    select(screen_name, description) %>%
    mutate(
      is_scientist = case_when(str_detect(tolower(description), paste(science_WL, collapse = "|" )) ~ 1),
      is_farmer = case_when(str_detect(tolower(description), paste(farmer_WL, collapse = "|" )) ~ 1),
      is_govt = case_when(str_detect(tolower(description), paste(govt_WL, collapse = "|" )) ~ 1),
      is_business = case_when(str_detect(tolower(description), paste(business_WL, collapse = "|" )) ~ 1),
      is_media = case_when(str_detect(tolower(description), paste(media_WL, collapse = "|" )) ~ 1),
      is_envnmtal = case_when(str_detect(tolower(description), paste(environmental_WL, collapse = "|" )) ~ 1))
  
  #set NA's to 0 to allow summing
  user_group[is.na(user_group)] <- 0
  
  #create df showing proportion of each group that RTed the original tweet
  prop_likes <- user_group %>%
    mutate(                                  
      scientist = sum(is_scientist)/nrow(.),
      farmer = sum(is_farmer)/nrow(.),
      govt = sum(is_govt)/nrow(.),
      business = sum(is_business)/nrow(.),
      media = sum(is_media)/nrow(.),
      envnmtal = sum(is_envnmtal)/nrow(.)) %>%
    select(9:14) %>% #select only the newly mutated rows showing proportions
    head(1) %>% #the sums were put into each row -- only need one
    gather(group, prop_like) %>% # gather into tidy format
    mutate(
      tweet_og = noRT_filtered[x,]$text, #original tweet
      handle_og = noRT_filtered[x,]$screen_name, #Screen name of original tweet
      n = nrow(.)) #gives number of users that retweeted the `tweet_og` 
      
  
  return(prop_likes)
}



###############################################################
#~~~~~~~~~~~~~~~~~~~~~~  Begin Analysis  ~~~~~~~~~~~~~~~~~~~~~#
###############################################################

#### ~~~~~~~~~~~  noIndia  ~~~~~~~~~####

####~~~~~~ Top 100 unfiltered  ~~~~~####

#### answering the question, which tweets are most liked by which user group 
non_india_list <- lapply(1:100, find_group_prop, y = 0)
non_india_df <- bind_rows(non_india_list, .id = "ID")

### Box plots comparing the distribution of which user groups retweeted each of the top 100 non-india related tweets
boxplot(prop_like ~ group, data = non_india_df,
        xlab = "User Group",
        ylab = "Proportion of RTs", 
        main = "Proportion of RTs by user group top 100 non-India related tweets")

## filter for which group most liked each tweet from the top 50 most retweeted (not filtered for any category)
group_favorite <- non_india_df %>% 
  group_by(ID) %>% 
  filter(prop_like == max(prop_like) &
           prop_like > 0) %>% # added this b/c there is at least 1 tweet that has no 'likes' by anyone and therefor has all groups as 'max'
  ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
environmentalist <- group_favorite %>% 
  filter(group == "envnmtal") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og) # add new column called 'text' to work with the create_wordcloud function

create_wordcloud(environmentalist)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
government <- group_favorite %>% 
  filter(group == "govt") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(government)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
farmer <- group_favorite %>% 
  filter(group == "farmer") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(farmer)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
scientist <- group_favorite %>% 
  filter(group == "scientist") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(scientist)

#####~~~~~~~~~  top 50 tweets,  'soil' ~~~~~~~~~~~~####
non_india_soil <- lapply(1:50, find_group_prop,y = 0, category = "soil")
soil_df <- bind_rows(non_india_soil, .id = "ID")

boxplot(prop_like ~ group, data = soil_df,
        xlab = "User Group",
        ylab = "Proportion of RTs", 
        main = "Proportion of RTs by user group top 50 non-India related tweets about 'soil' ")

## filter for which group most liked each tweet from the top 50 most retweeted (not filtered for any category)
group_fav_soil <- soil_df %>% 
  group_by(ID) %>% 
  filter(prop_like == max(prop_like) &
           prop_like > 0) %>% # added this b/c there is at least 1 tweet that has no likes by anyone and therefor has all groups as 'max'
  ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
envt_soil <- group_fav_soil %>% 
  filter(group == "envnmtal") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og) # add new column called 'text' to work with the create_wordcloud function

create_wordcloud(envt_soil)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
govt_soil <- group_fav_soil %>% 
  filter(group == "govt") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(govt_soil)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
farmer_soil <- group_fav_soil %>% 
  filter(group == "farmer") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(farmer_soil)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
scientist_soil <- group_fav_soil %>% 
  filter(group == "scientist") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(scientist_soil)





#####~~~~~~~~~  top 10 tweets,  'rangeland' ~~~~~~~~~~~~~####
#after filtering for no_india and "rangeland" only 9 tweets have any RTs (the 10th at least has some favorites so i kept it)
non_india_range <- lapply(1:10, find_group_prop, y = 0, category = "rangeland")
range_df <- bind_rows(non_india_range, .id = "ID")

boxplot(prop_like ~ group, data = range_df,
        xlab = "User Group",
        ylab = "Proportion of RTs", 
        main = "Proportion of RTs by user group top 10 non-India related tweets about 'rangeland' ")

## filter for which group most liked each tweet from the top 50 most retweeted (not filtered for any category)
group_fav_range <- range_df %>% 
  group_by(ID) %>% 
  filter(prop_like == max(prop_like) &
           prop_like > 0) %>% # added this b/c there is at least 1 tweet that has no likes by anyone and therefor has all groups as 'max'
  ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
envt_range<- group_fav_range %>% 
  filter(group == "envnmtal") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og) # add new column called 'text' to work with the create_wordcloud function
#
create_wordcloud(envt_range)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
govt_range <- group_fav_range %>% 
  filter(group == "govt") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(govt_range)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
farmer_range <- group_fav_range %>% 
  filter(group == "farmer") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(farmer_range)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
scientist_range <- group_fav_range %>% 
  filter(group == "scientist") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(scientist_range)


#### ~~~~~~~~~~~  All tweets  ~~~~~~~~~####

####~~~~~~ Top 100 unfiltered  ~~~~~####

#### answering the question, which tweets are most liked by which user group 
full_list <- lapply(1:100, find_group_prop)
full_df <- bind_rows(full_list, .id = "ID")

### Box plots comparing the distribution of which user groups retweeted each of the top 100 non-india related tweets
boxplot(prop_like ~ group, data = full_df,
        xlab = "User Group",
        ylab = "Proportion of RTs", 
        main = "Proportion of RTs by user group for top 100 tweets")

## filter for which group most liked each tweet from the top 50 most retweeted (not filtered for any category)
group_favorite_full <- full_df %>% 
  group_by(ID) %>% 
  filter(prop_like == max(prop_like) &
           prop_like > 0) %>% # added this b/c there is at least 1 tweet that has no likes by anyone and therefor has all groups as 'max'
  ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
envt_full <- group_favorite_full %>% 
  filter(group == "envnmtal") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og) # add new column called 'text' to work with the create_wordcloud function

create_wordcloud(envt_full)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
govt_full <- group_favorite_full %>% 
  filter(group == "govt") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(govt_full)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
farmer_full <- group_favorite_full %>% 
  filter(group == "farmer") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(farmer_full)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
scientist_full <- group_favorite_full %>% 
  filter(group == "scientist") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(scientist_full)










####~~~~~~ top 50 , 'soil' ~~~~~####

full_soil <- lapply(1:10, find_group_prop, category = "soil")
full_soil_df <- bind_rows(full_soil, .id = "ID")

boxplot(prop_like ~ group, data = full_soil_df,
        xlab = "User Group",
        ylab = "Proportion of RTs", 
        main = "Proportion of RTs by user group top 50 tweets about 'soil' ")

## filter for which group most liked each tweet from the top 50 most retweeted (not filtered for any category)
group_fav_soil <- soil_df %>% 
  group_by(ID) %>% 
  filter(prop_like == max(prop_like) &
           prop_like > 0) %>% # added this b/c there is at least 1 tweet that has no likes by anyone and therefor has all groups as 'max'
  ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
envt_soil <- group_fav_soil %>% 
  filter(group == "envnmtal") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og) # add new column called 'text' to work with the create_wordcloud function

create_wordcloud(envt_soil)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
govt_soil <- group_fav_soil %>% 
  filter(group == "govt") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(govt_soil)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
farmer_soil <- group_fav_soil %>% 
  filter(group == "farmer") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(farmer_soil)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
scientist_soil <- group_fav_soil %>% 
  filter(group == "scientist") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(scientist_soil)



####~~~~~~ top 50 , 'rangeland' ~~~~~####
foo <- lapply(1:10, find_group_prop, category = "rangeland")
bar <- bind_rows(foo, .id = "ID")

full_range <- lapply(1:10, find_group_prop, category = "rangeland")
full_range_df <- bind_rows(full_range, .id = "ID")

boxplot(prop_like ~ group, data = full_range_df,
        xlab = "User Group",
        ylab = "Proportion of RTs", 
        main = "Proportion of RTs by user group top 10 non-India related tweets about 'rangeland' ")

## filter for which group most liked each tweet from the top 50 most retweeted (not filtered for any category)
group_fav_range <- full_range_df %>% 
  group_by(ID) %>% 
  filter(prop_like == max(prop_like) &
           prop_like > 0) %>% # added this b/c there is at least 1 tweet that has no likes by anyone and therefor has all groups as 'max'
  ungroup()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
envt_range<- group_fav_range %>% 
  filter(group == "envnmtal") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og) # add new column called 'text' to work with the create_wordcloud function
#
create_wordcloud(envt_range)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
govt_range <- group_fav_range %>% 
  filter(group == "govt") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(govt_range)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
farmer_range <- group_fav_range %>% 
  filter(group == "farmer") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(farmer_range)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
scientist_range <- group_fav_range %>% 
  filter(group == "scientist") %>% 
  select(-ID) %>% 
  distinct() %>% 
  mutate(text = tweet_og)

create_wordcloud(scientist_range)



























