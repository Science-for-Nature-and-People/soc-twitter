# SNAPP Soil Organic Carbon -- Tweet Parsing and Analysis 

This repository contains several scripts developed to process Twitter data to investigate how soil organic content and health are related.

Two different data sources are used:

- Data collected directly from the Twitter API
- Data from the Twitter archives

The following scripts can be used for:

- Main: [raw_data_processing.R](raw_data_processing.R):
  - read raw twitter datasets from different sources (Json or csv format). 
  - Clean and standardize to enable a merge
  - Simple analysis of what the data looks like. 

- Associated script: 
[fixed_tweet.sh](fixed_tweet.sh)
  - Correct parsing errors found in the csv files derived from the API (cell overlap)

**!!! This script needs to be edited from the command line and _NOT_ from R, as it is dealing with hidden characters**
 
- Some inititial data exploration: 
  - [Data_viz_script.R](Data_viz_script.R): Data visualization and exploration
  - [Sentiment_test.R](sentiment_test.R): Used to explore text mining options with Archived/json data. Reproducible for the larger merged dataset.

More specific exploration and visualizations can be found in the following folders (see their respective README's for more detailed information about specific analyses):
- [tweet_content]() various way of visualizing the content of tweets by different categories
- [influencers]() attempts to identify what type of content appeals to different user groups
-each of these ^ rely on the functions within [text_analysis_functions.R](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/text_analysis_functions.R)
