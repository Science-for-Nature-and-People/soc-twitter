# Data exploration of tweet dates

####----Load Libraries and Data Set----####
library(tidyverse)
library(lubridate)
library(scales)
path <- "/home/shares/soilcarbon/Twitter/Merged_v2"
noRT <- read.csv(file.path(path, "twitter_merged_noRT_v2.csv"), stringsAsFactors = FALSE)




####----Data Cleaning----####

viz_noRT <- noRT %>% 
  mutate(created_at = ymd_hms(created_at)) %>% # turn 'created_at' into date object
  mutate(created_at = floor_date(created_at, "day")) %>% # remove hours, minutes, seconds
  filter(source != "Twittascope") # remove outlier (spam tweets on 2019-07-06) (source = "Twittascope")

# remove outlier day "2019-07-06", identified by running: CODE FOR FINDING MAX AND REMOVING
#date = names(which.max(table(viz_noRT$created_at)))
#noRT <- noRT %>% 
#  filter(ymd(created_at) != ymd(date))




####----Plot Date Distributions----####

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

  

####----Plot Missing Dates----####

min_dt <- as.Date(as.POSIXct.Date(min(as.numeric(as.Date.POSIXct(ymd_hms(noRT$created_at)))))) # find first date in df
max_dt <- as.Date(as.POSIXct.Date(max(as.numeric(as.Date.POSIXct(ymd_hms(noRT$created_at)))))) # find last date in df 
all_days <- seq.Date(min_dt, max_dt, by = "days") # sequence of all days between min_dt and max_dt
missing_days <- all_days[!all_days %in% as.Date(unique(noRT$created_at))] # days where tweets are missing

df <- data_frame(missing_days)
ggplot(df, aes(missing_days)) +
  geom_histogram(bins = length(all_days)) +
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%b %y"))

# Below shows that there are no missing days from the original data frame purchased from Twitter
testing <- read_csv("/home/shares/soilcarbon/Twitter/Merged_data/twitter_merged_noRT.csv")
test_df <- testing %>% 
  filter(provenance != "API")
min_dt <- as.Date(as.POSIXct.Date(min(as.numeric(as.Date.POSIXct(ymd_hms(test_df$created_at)))))) # find first date in df
max_dt <- as.Date(as.POSIXct.Date(max(as.numeric(as.Date.POSIXct(ymd_hms(test_df$created_at)))))) # find last date in df 
all_days <- seq.Date(min_dt, max_dt, by = "days") # sequence of all days between min_dt and max_dt
missing_days <- all_days[!all_days %in% as.Date(unique(test_df$created_at))] # days where tweets are missing


