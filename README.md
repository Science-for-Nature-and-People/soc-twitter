# SNAPP Soil Organic Carbon -- Tweet Parsing and Analysis 
***
This repository contains several scripts developed to process Twitter data to investigate how soil organic content and health are related.

Two different data sources are used:

- Data collected directly from the Twitter API 
- Data from the Twitter archives

For the archive data:

- Main: [raw_data_processing.R](raw_data_processing.R):
  - read raw twitter datasets from different sources (Json or csv format) 
  - clean and standardize to enable a merge
  - simple analysis of what the data looks like 
  - to correct parsing errors found in the csv files derived from the API (cell overlap) use [fixed_tweet.sh](fixed_tweet.sh)
    **!!! This script needs to be edited from the command line and _NOT_ from R, as it is dealing with hidden characters !!!**

For the collected data via Twitter API:

- Main: [automate.R](automate.R) 
  - runs every week collecting the last 6-9 days of twitter data based on query words from [tag_list.csv](tag_list.csv)
  - cleans and standarize to enable merge
  
*** 

- Inititial data exploration: 
  - [Data_viz_script.R](Data_viz_script.R): Data visualization and exploration
  - [Sentiment_test.R](sentiment_test.R): Used to explore text mining options with Archived/json data. Reproducible for the larger merged dataset.

- More specific exploration and visualizations can be found in the following folders (see their respective README's for more detailed information about specific analyses):
  - various way of visualizing the content of tweets by different categories[tweet_content](https://github.com/Science-for-Nature-and-People/soc-twitter/tree/master/tweet_content) 
  - attempts to identify what type of content appeals to different user groups [influencers](https://github.com/Science-for-Nature-and-People/soc-twitter/tree/master/influencers)
  - each of these ^ rely on the functions within [text_analysis_functions.R](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/text_analysis_functions.R)
  
***       
      
[translation](https://github.com/Science-for-Nature-and-People/soc-twitter/tree/master/translation) folder contains scripts for translating hindi using google translate via webinterface

*** 

[pre_processing](https://github.com/jeremyknox-ucsb/soc-twitter/tree/master/pre_processing) folder contains scripts for specific tasks (usually run once). 
