# Data exploration of tweet dates

####----Load Libraries and Data Set----####
library(tidyverse)
library(lubridate)
path <- "/home/shares/soilcarbon/Twitter/Merged_v2"
noRT <- read.csv(file.path(path, "twitter_merged_noRT_v2.csv"), stringsAsFactors = FALSE)




####----Data Cleaning----####

viz_noRT <- noRT %>% 
  mutate(created_at = ymd_hms(created_at)) %>% # turn 'created_at' into date object
  mutate(created_at = floor_date(created_at, "day")) %>% # remove hours, minutes, seconds
  filter(source != "Twittascope") # remove outlier (source = "Twittascope")

# remove outlier day "2019-07-06", identified by running: CODE FOR FINDING MAX AND REMOVING
#date = names(which.max(table(viz_noRT$created_at)))
#noRT <- noRT %>% 
#  filter(ymd(created_at) != ymd(date))




####----Distribution Plots----####

# plot distribution of dates by DAYS OF THE WEEK
qplot(wday(viz_noRT$created_at, label = T), data = viz_noRT, geom = "bar")

# plot time series count by DAY
ggplot(viz_noRT, aes(created_at)) +
  geom_bar(stat = "count") 

# plot time series count by WEEK
ggplot(viz_noRT, aes(floor_date(viz_noRT$created_at, "week"))) +
  geom_bar(stat = "count")

# plot time series count by MONTH
ggplot(viz_noRT, aes(floor_date(viz_noRT$created_at, "month"))) +
  geom_bar(stat = "count")
  
# plot time series count by DAY separated by year
ggplot(viz_noRT, aes(created_at)) +
  geom_bar() +
  facet_wrap(~year(created_at), scales = "free")

# plot distribution of dates by WEEK separated by year
ggplot(viz_noRT, aes(week(created_at))) +
  geom_bar() +
  facet_wrap(~year(created_at), scales = "free")

# plot distribution of dates by MONTH separated by year
ggplot(viz_noRT, aes(month(created_at))) +
  geom_bar() +
  facet_wrap(~year(created_at), scales = "free")

  
