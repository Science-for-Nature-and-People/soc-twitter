##########################################################
# Table 1                                                #
# Creates a list of top tweets based on:                 #
# 1. Number of total retweets of all their tweets        #
#      (called top_users)                                #
# 2. Ratio of tweets to retweets, based on some limit    #
#      (called top_user_ratio)                           #
##########################################################

#### LOAD PACKAGES ####
library(tidyverse)
library(magick)
library(knitr)
library(kableExtra)
library(tm)
source("../../text_analysis_functions.R")

#### LOAD DATA ####
noRT <- read.csv("/home/shares/soilcarbon/Twitter/Merged_v2/twitter_merged_noRT_v2.csv", stringsAsFactors = FALSE)

#### MANIPULATE DATA ####
# Set rm_pope & rm_india to FALSE to keep both the pope and india tweets respectively
noRT_clean <- clean_data(noRT, rm_pope = T, rm_india = T)

# Number of users to include in tables:
num_users <- 20

# Create a list of top 500 users in terms of num of total retweets of all their tweets
# Create a list with username, text and retweet_count
user.df <- noRT %>%
  select(screen_name, text, retweet_count, user_id)

user.df$text <- substring(user.df$text, 1, 30)

# Remove duplicate usernames/text combos
user.df <- user.df[!duplicated(user.df[1:2]), ]

# Sum up retweet_count based on username
user_counts <- aggregate(user.df$retweet_count,
  by = list(Users = user.df$screen_name),
  FUN = sum
)
names(user_counts) <- c("screen_name", "tot_retweet_count")


# Calculate the total # of tweets each of these top users have created
tweet_count <- user.df %>%
  group_by(screen_name) %>%
  tally()

all_counts <- dplyr::left_join(user_counts, tweet_count, by = "screen_name")

## Get ratio of number of tweets from a user to number of retweets
all_counts$ratio <- round(all_counts$tot_retweet_count / all_counts$n, 2)

#### TOP USER ####
# Approach 1. Create lists of top users based on total number of retweets
# Select top users
top_user <- all_counts %>%
  select(-n, -ratio) %>%
  arrange(-tot_retweet_count) %>%
  head(num_users)

# Add in their most retweeted tweet and RT count for it
top_tweets <- noRT %>%
  filter(screen_name %in% top_user$screen_name) %>%
  group_by(screen_name) %>%
  filter(retweet_count == max(retweet_count)) %>%
  select(screen_name, retweet_count, text)

top_user_info <- left_join(top_user, top_tweets)
names(top_user_info) <- c("User name", "Total number of RTs", "RT count for most RTed tweet", "Most RTed tweet")

# Create table
kable(top_user_info) %>%
  kable_styling(bootstrap_options = "striped") %>%
  column_spec(2, width = "1in") %>%
  column_spec(3, width = "1in") %>%
  arsenal::write2html("/home/swood/soc-twitter/Tables/table1.html", quiet = TRUE)


#### RETWEET RATIO ####
# Repeat above, using RT:Tweet ratio as metric
# Define parameters:
num_tweet_limit <- 10
ratio_limit <- 2

top_user_ratio <- all_counts %>%
  filter(n >= num_tweet_limit & ratio >= ratio_limit) %>%
  select(screen_name, tot_retweet_count, ratio) %>%
  arrange(-tot_retweet_count) %>%
  head(num_users) ## defined on line 86

top_tweets_ratio <- noRT %>%
  filter(screen_name %in% top_user_ratio$screen_name) %>%
  group_by(screen_name) %>%
  filter(retweet_count == max(retweet_count)) %>%
  select(screen_name, retweet_count, text)

top_ratio_info <- left_join(top_user_ratio, top_tweets_ratio)
names(top_ratio_info) <- c("User name", "Total number of RTs", "RTs:Tweets", "RT count for most RTed tweet", "Most RTed tweet")

### create table
kable(top_ratio_info) %>%
  kable_styling(bootstrap_options = "striped") %>%
  column_spec(2, width = "1in") %>%
  column_spec(3, width = "1in") %>%
  arsenal::write2html("/home/swood/soc-twitter/Tables/table1b.html", quiet = TRUE)

rm(list = ls())


#### DEPRECATED ####
## clean text and format (words w/ counts)
tidy_counts <- prepare_text(top_10)

## get sentiment for each lexicon
nrc <- tidytext::get_sentiments("nrc")
bing <- tidytext::get_sentiments("bing")
afin <- tidytext::get_sentiments("afinn")
lough <- tidytext::get_sentiments("loughran")

top_10_nrc <- inner_join(tidy_counts, nrc)
top_10_bing <- inner_join(tidy_counts, bing)
top_10_afin <- inner_join(tidy_counts, afin)
top_10_lough <- inner_join(tidy_counts, lough)

# ~~ bing seems to be the
top_100 <- noRT %>%
  dplyr::select(screen_name, retweet_count, text) %>%
  arrange(-retweet_count) %>%
  head(100)

tidy_100 <- prepare_text(top_100)

top_100_bing <- inner_join(tidy_100, bing)

bing_100_sum <- top_100_bing %>%
  group_by(sentiment, word) %>%
  dplyr::summarise(n = sum(n)) %>%
  filter(n > 1)

ggplot(bing_100_sum, aes(sentiment, n)) +
  geom_col()
