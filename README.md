# SNAPP Soil Organic Carbon -- Tweet Parsing and Analysis 

This repository contains several scripts developed to process Twitter data to investigate how soil organic content and health are related.


### Processing Twitter Data

Two different data sources are used:

- Data from the Twitter archives
- Data collected directly from the Twitter API 


   History of Data: 

     - Archive Data (purchased) 2017-04-01 to 2017-10-10
     - Manual API Data 2017-10-11 to 2019-06-14 NOTE there is missing data during these dates
     - Automatic API Data 2019-06-15 to Current
     
     There have been multiple "versions" of data, current code should be run on the latest version (ie. change code and directory names to v3):  
     - V1 "Merged_data":  
     - V2 "Merged_v2":  
     - V3 "Merged_v3": this fixed issue where archive data did not have *is_retweet* flagged and replaced old retweets where usernames were present within the original tweet *text*, see: [fix_old_retweets.R](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/pre_processing/fix_old_retweets.R)  
     - V4 "Merged_v4": this removed duplicates within and between datasets, with cleaner processing on the original purchased dataset. See [data_version_check.R](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/60809f78eef5ea52aba165526b55c1f1f018c0aa/data_version_check.Rmd) for a comparison between data versions 3 and 4
        

For the archive data:

- Main: [raw_data_processing.R](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/pre_processing/raw_data_processing.R):
  - read raw twitter datasets from different sources (Json or csv format) 
  - clean and standardize to enable a merge
  - simple analysis of what the data looks like 
  - to correct parsing errors found in the csv files derived from the API (cell overlap) use [fixed_tweet.sh](fixed_tweet.sh)
    **!!! This script needs to be edited from the command line and _NOT_ from R, as it is dealing with hidden characters !!!**

For the data collected via Twitter API:

- Main: [automate.R](automate.R) 
  - saves two files:  
  (1) raw data from Twitter API in .csv format saved in directory /home/shares/soilcarbon/Twitter/API_csv/   
  (2) writes over previous /home/shares/soilcarbon/Twitter/Merged_v3/ (master files) cleaning and standardizing to enable merge, as well as removing duplicates
  - runs twice a week collecting the last 6-9 days of twitter data based on query words from [tag_list.csv](tag_list.csv)
  
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
