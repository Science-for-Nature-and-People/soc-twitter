#########################################
# Testing sentiment analysis with tweets#
# This scripts analyzes twitter data    #
#########################################

library(tidytext)
library(wordcloud)
library(tidyverse)
library(dplyr)

#### loading data
# not: uncomment if not yet loaded 
# twitter.data.full<-stream_in("/home/shares/soilcarbon/Twitter/twitter.json")
# 
# class(twitter.data.full)
# names(twitter.data.full)
# str(twitter.data.full)
# test1<-sample_n(twitter.data.full, 10)

### Parsing through tweets ####

# Selecting relevant columns: 
main_tweet_columns<-data.frame(cbind(twitter.data.full$actor.displayName, twitter.data.full$actor.summary,
                               twitter.data.full$body,
                               twitter.data.full$object.summary, twitter.data.full$postedTime))

##Renaming columns:
colnames(main_tweet_columns)<-c("name", "actorSummary", "tweet_body", "tweet_body_noRT", "time")

## Sample dataset: 
##took sample and call the all_tweets_column - started with a small sample of 100 tweets and then enlarge. now chose 90000

# main_tweet_columns_sample<-sample_n(main_tweet_columns, 10) #comment out when running twitter.data.full

##Call whole dataset 
main_tweet_columns_sample <- main_tweet_columns

##Separate tweets and make edited column where we can manipulate:
tweets <- main_tweet_columns_sample %>% 
  select(tweet_body) %>% # take only raw tweets
  mutate(tweet_edited=as.character(tweet_body)) %>% # change from character to factor  
  mutate(tweet_edited=tolower(tweet_body)) %>% #make lower case
  # mutate(tweets_edited=str_replace_all(tweets_edited, ' ' , '_')) %>% #put underscores instead of spaces (removed - not necessary)
  filter(!is.na(tweet_edited)) #remove NA columns

# note: there are 73074 out of 96553 tweets that are valid. 23479 NA rows. 

##Unnest to separate by words
unnest_tweets <- tweets %>% 
  unnest_tokens(word, tweet_edited) #unnest to get words

##Count table with sorted words by number of times seen  
tweet_counts <- unnest_tweets %>% 
  anti_join(stop_words) %>% 
  count(word, sort=TRUE) %>% 
  filter(!word %in% c("https","rt","t.co"))

##Wordcloud  
library(wordcloud)
tweet_counts %>% 
  with(wordcloud(word, n, max.words=200, color=brewer.pal(7,"Dark2")))

###Sentiment analysis
# in general, get_sentiment has afinn scores/ranks fro -5 to +5 for positive or negative sentiment

# get_sentiments("afinn") %>% 
# head(20)

#For our Tweets:
tweets_sentiment <- unnest_tweets %>% 
  left_join(get_sentiments("nrc"), by = "word") %>% 
  filter(sentiment !="NA")

## Sorting words with associated adjective:
count_sentiment <- tweets_sentiment %>% 
  count(word, sentiment, sort=TRUE)
count_sentiment

## group sentiment adjectives  
total_sentiment <- count_sentiment %>% 
  group_by(sentiment) %>% 
  summarise(totals=sum(n)) %>% 
  arrange(-totals)
total_sentiment

#graph
ggplot(total_sentiment)+
  geom_col(aes(x=sentiment, y=totals))

# note: nrc dictionary not best for assessing sentiment about soil health. I.e. soil matched with "disgust."