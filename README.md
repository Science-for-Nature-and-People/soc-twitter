# SNAPP Soil Organic Carbon -- Tweet Parsing and Analysis 

This repository contains several scripts developed to process Twitter data to investigate how soil organic content and health are related.

Two different data sources are used:

- Data collected directly from the Twitter API
- Data from the Twitter archives

The following scripts can be used for:

- Main: snapp_twitter-script2.R
  - Used to:
  - read raw twitter datasets from different sources (Json or csv format). 
  - Clean and standardize to enable a merge
  - Simple analysis of what the data looks like. 

- Associated script: 
fixed_tweet.sh
  - Used to correct for parsing error found in the csv files derived from the API (cell overlap)
Sentiment_test.R
  - Used to explore text mining options with Archived/json data. Reproducible for the larger merged dataset.

