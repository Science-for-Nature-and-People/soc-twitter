# Code for pre-processing data

  - separating twitter data frame into no retweets and retweets with [is_retweet.R](https://github.com/jeremyknox-ucsb/soc-twitter/blob/master/pre_processing/is_retweet.R) 
  - remove duplicate from RT and noRT master data frame with [remove_duplicates.R](remove_duplicates.R)
  - for processing original data frame purchase from twitter use [raw_data_processing.R](raw_data_processing.R) with [fix_tweet.sh](fix_tweet.sh) to correct parsing errors found in the csv files derived from the API (cell overlap)
  - found outlier tweets from source = Twittascope (robo tweets), plot distributions of tweets based on dates, plot dates that are missing from noRT data frame [tweets_dates_viz.R](tweets_dates_viz.R)
