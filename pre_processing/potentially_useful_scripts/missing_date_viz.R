# Visualization of 

library(tidyverse)
library(lubridate)
library(ggplot2)

flag_india <- function(data) {
  
  results <- data %>% 
    mutate(
      is_india = case_when(
        str_detect(tolower(text), "[\u0900-\u097F]+|india|crore|health card|rupee|narendramodi|managed to feed 1.25 billion people|akshaykumar") ~ 1,
        str_detect(tolower(screen_name), "[\u0900-\u097F]+|india|crore|health card|rupee|narendramodi") ~ 1,
        str_detect(tolower(hashtags), "[\u0900-\u097F]+|india|crore|health card|rupee|narendramodi") ~ 1))
  
  #replace na w/ 0 to indicate non-india related tweets
  results$is_india[is.na(results$is_india)] <- 0
  
  return(results)
}

path <- '/home/shares/soilcarbon/Twitter' # LOCATION OF MASTER FILES

twitter_merged <- read.csv(file.path(path, 'Merged_v2/twitter_merged_v2.csv'), stringsAsFactors = FALSE) 
#twitter_merged_noRT <- read.csv(file.path(path, 'Merged_v2/twitter_merged_noRT_v2.csv'), stringsAsFactors = FALSE) 

# DF of days when there are tweets 
RT_unique <- twitter_merged %>% 
  mutate(created_at = ymd_hms(created_at)) %>% 
  mutate(created_at = round_date(created_at, unit = "day")) %>% 
  distinct(created_at, .keep_all = TRUE) %>% 
  select(created_at) %>% 
  mutate(created_at = ymd(created_at))
# plot of missing days in master data frame
max_date <- max(RT_unique$created_at)
RT_unique_viz <- ggplot(RT_unique, aes(x = created_at)) +
  geom_bar(stat = 'count') +
  scale_x_date(date_breaks = "1 month", date_labels = "%y %b") +
  geom_segment(aes(x=ymd("2017-04-01"),xend=ymd("2017-04-01"),y=0,yend=1.05)) +
  annotate("text", x = ymd("2017-04-01"), y = 1.1, label = "Purchased ->") +
  geom_segment(aes(x=ymd("2017-10-11"),xend=ymd("2017-10-11"),y=0,yend=1.05)) +
  annotate("text", x = ymd("2017-10-11"), y = 1.1, label = "Manual API ->") +
  geom_segment(aes(x=ymd("2019-06-15"),xend=ymd("2019-06-15"),y=0,yend=1.05)) +
  annotate("text", x = ymd("2019-06-15"), y = 1.1, label = "Automatic API ->") +
  geom_segment(aes(x=max_date,xend=max_date,y=0,yend=1.05)) +
  annotate("text", x = max_date, y = 1.1, label = "End of Data") +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank())





# DF of count of tweets per day 
RT_count <- twitter_merged %>% 
  mutate(created_at = ymd_hms(created_at)) %>% 
  mutate(created_at = round_date(created_at, unit = "day")) %>% 
  select(created_at) %>% 
  mutate(created_at = ymd(created_at))
# plot of distibution (histogram) of number of tweets per day 
int <- interval(min(RT_count$created_at), max(RT_count$created_at))
RT_count_viz <- ggplot(RT_count, aes(x = created_at)) +
  geom_histogram(bins = time_length(int, "day")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%y %b") +
  theme_classic()

# DF of count of tweets per day 
RT_count_rm <- twitter_merged %>% 
  mutate(created_at = ymd_hms(created_at)) %>% 
  mutate(created_at = round_date(created_at, unit = "day")) %>%   
  mutate(created_at = ymd(created_at)) %>% 
  filter(created_at != ymd("2018-01-10") &
           created_at != ymd("2017-08-22") &
           created_at != ymd("2019-07-06") &
           created_at != ymd("2019-07-07")) %>% 
  select(created_at)
# plot of distibution (histogram) of number of tweets per day, outliers removed (n > 2,500)
RT_count_viz_rm <- ggplot(RT_count_rm, aes(x = created_at)) +
  geom_histogram(bins = time_length(int, "day")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%y %b") +
  theme_classic()


# extra code
twitter_merged_rm <- twitter_merged %>% 
  flag_india() %>% 
  filter(is_india == 0)

df_big <- as.data.frame(table(ymd(round_date(ymd_hms(twitter_merged_rm$created_at), unit = "day"))))
df <- df_big[df_big$Freq >= 1000,]

#df1 <- twitter_merged[ymd(df$Var1) %in% ymd(round_date(ymd_hms(twitter_merged$created_at), unit = "day")), ]

df1 <- twitter_merged_rm %>% 
    filter(ymd(floor_date(ymd_hms(created_at), unit = "day")) == as.character(df$Var1[1]))

df_i <- twitter_merged %>% 
  filter(ymd(floor_date(ymd_hms(created_at), unit = "day")) == as.character(df$Var1[i]))







pope_francis_RTs = twitter_merged %>% 
  filter(ymd(floor_date(ymd_hms(created_at), unit = "day")) == "2018-01-10")

narendra_modi_RTs = twitter_merged %>% 
  filter(ymd(floor_date(ymd_hms(created_at), unit = "day")) == "2017-08-22")

twittascope_RTs = twitter_merged %>% 
  filter(ymd(floor_date(ymd_hms(created_at), unit = "day")) == "2019-07-06"|
           ymd(floor_date(ymd_hms(created_at), unit = "day")) == "2019-07-07")
