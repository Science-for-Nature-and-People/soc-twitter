## Identifying communities & top users

This folder contains scripts for identifying important users within our dataset and performing network analysis around those top users.


* [gen_data.R](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/tweet_content/content_creators/gen_data.R) gets the list of friends for the 1000 users with the most collecive retweets

* [friends_network.Rmd](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/tweet_content/content_creators/friends_network.Rmd) uses the data from gen_data.R to perform network identify most central users based on their friends list

* [RT_networkds.Rmd](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/tweet_content/content_creators/RT_networks.Rmd)  creates network graphs based on retweets between:
  - all manually coded user types 
  - soil health and regen ag key words


* [top_users.rmd](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/tweet_content/content_creators/top_users.Rmd) 
  - uses two different metrics for identifying "top users". 
  - creates table of top tweets for the paper
  - begins sentiment analysis on top tweets
