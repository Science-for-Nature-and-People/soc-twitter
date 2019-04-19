# Twitter
# API only has data going back 6-9 days
# devtools::install_github("mkearney/rtweet")
library(tidyverse)
library(rtweet)
library(countrycode)
library(lubridate)

# Function that downloads tweets from API 
api_csv <- function(){
  
  # Create token
  twitter_token <- readRDS('twitter_token.rds')
  
  # Search twitter
  q <- c('"soil health"', '"healthy soil"', '#soilhealth', '#healthysoil', 
         '"soil quality"', '"soil fertility"', '#soilquality', '#soilfertility',
         '"rangeland health"','#rangelandhealth','"healthy rangelands"',
         '#healthyrangelands')
  
  # Searching tweets with query above
  twitterAPI_new <- search_tweets2(q, n = 100000, token=twitter_token, retryonratelimit = T)
  twitterAPI_new <- as.data.frame(twitterAPI_new)
  
  # Delete useless rownames
  rownames(twitterAPI_new) <- c()
  
  # Creating file name 
  file.name <- paste0('/home/nolasco/soc-twitter/API_csv/', Sys.Date(), '.csv')
  
  # Creating csv file
  write.csv(twitterAPI_new, file.name)
}
