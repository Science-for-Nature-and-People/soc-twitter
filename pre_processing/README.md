# Code for pre-processing data

  - separating twitter data frame into no retweets and retweets with [is_retweet.R](https://github.com/jeremyknox-ucsb/soc-twitter/blob/master/pre_processing/is_retweet.R) NOTE: this was used for intial json archive data file and most likley does not need to be run again
  - for processing original data frame purchase from twitter use [raw_data_processing.R](raw_data_processing.R) with [fix_tweet.sh](fix_tweet.sh) to correct parsing errors found in the csv files derived from the API (cell overlap)
  - remove duplicate from RT and noRT master data frame with [remove_duplicates.R](remove_duplicates.R) 
  - found outlier tweets from source = Twittascope (robo tweets, n > 20,000), plot distributions of tweets based on dates, plot dates that are missing from noRT data frame [tweets_dates_viz.R](tweets_dates_viz.R)
- Replace old retweets that started with "RT @xxxx:" and ended with "...", because they were truncated by Twitter, with the the original tweet that was retweeted with [fix_old_retweets.R](fix_old_retweets.R) NOTE: this was run and kicked off version 3 of master data frames
