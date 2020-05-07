##### this script creates the boxplot figure showing retweet decay over time



library(tidyverse)
library(plyr)
source("text_analysis_functions.R")



noRT <- read.csv("/home/shares/soilcarbon/Twitter/Merged_v4/twitter_merged_noRT_v4.csv", stringsAsFactors = FALSE)
RT <- read.csv("/home/shares/soilcarbon/Twitter/Merged_v4/twitter_merged_v4.csv",stringsAsFactors = FALSE)

noRT_clean <- clean_data(noRT, rm_pope = T, rm_india = T)
RT_clean <- clean_data(RT, rm_pope = T, rm_india = T)


## set limit for number of retweets
limit <- 10

rt_limit <- noRT_clean %>% 
  filter(retweet_count > limit) %>% 
  dplyr::select(created_at, user_id, screen_name, text) %>% 
  distinct()


### funcion for IDing RTs
library(plyr)
find_rt <- function(rank, noRT_dataset, RT_dataset) {
  result_rt <- RT_dataset %>% 
    filter(substring(RT_dataset$text, 1, 30) == substring(noRT_dataset$text[rank], 1, 30))
  # aggregate tweets by date
  result.df <- ddply(result_rt, .(date(result_rt$created_at), result_rt$query), nrow)
  names(result.df) <- c("Date", "Query", "Number")
  result.df <- result.df %>% arrange (result.df$Date)
  # calculate day_since based on the original tweet (noRT) date
  result.df$time_since <- result.df$Date - date(noRT_dataset$created_at[rank])
  result.df$content <- substring(result_rt$text[1], 1, 30)
  names(result.df) <- c("Date", "Query", "Number", "Time_since", "Content")
  return(result.df)
}



decay <- list()

## identify rts
# this will take a while baed on what rt_limit is set
for (i in 1:nrow(rt_limit)) {
  tmp <- find_rt(i, rt_limit, RT_clean)
  tmp <- tmp %>% 
    mutate(
      prop = round(Number/sum(Number),4)
    )
  decay[[i]] <- tmp
  
  if(i %% 500 == 0) {
    print(i)
  }
}

test <- bind_rows(decay)

names(test) <- tolower(names(test))

summ <- test %>% 
  group_by(time_since) %>% 
  filter(time_since <= 31 & time_since >= 0) %>% 
  dplyr::summarise(
    mean = round(mean(prop),4),
    sd = round(sd(prop),2)
  )

ggplot(summ, aes(as.factor(time_since), mean)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(.9)) +
  labs(x = "days since first tweet",
       y = "proportion of total retweets",
       title = "Average retweet decay over 1 month")

### get only one month and remove spaces and hashtags
box <- test %>% 
  filter(time_since <= 31 & time_since >= 0) %>% 
  mutate(query = str_replace_all(query, "#| |\"", ""))

###combining soilhealth and healthysoil
box$query[box$query == "healthysoil"] <- "soilhealth"
box$query[box$query == "healthyrangelands"] <- "rangelandhealth"


# define which search terms you want to include
terms <- c("soilhealth", "regenerativeagriculture")

box_sub <- filter(box, query %in% terms)

ggplot(box_sub, aes(as.factor(time_since), prop)) +
  geom_boxplot(outlier.shape = NA) +
  scale_x_discrete(breaks=pretty(box$time_since,n=10)) +
  facet_wrap(~query) +
  labs(x = "days since first tweet",
       y = "proportion of total retweets",
       title = "Average retweet decay over 1 month for tweets with >10 RTs") +
  theme_bw()



ggplot(box, aes(as.factor(time_since), prop)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "days since first tweet",
       y = "proportion of total retweets",
       title = "Average retweet decay over 1 month")


