###################################
# Visualize the decay of retweets #
###################################

# LOAD PACKAGES
library(tidyverse)
library(RColorBrewer)
library(tidytext)
library(stringr)
library(ggraph)
library(igraph)
library(tm)
library(NLP)
library(quanteda)
library(lubridate)
library(dplyr)
library(ggplot2)
source("soc-twitter/text_analysis_functions.R")

#### LOAD DATA ####
noRT <- read.csv("/home/shares/soilcarbon/Twitter/Merged_v3/twitter_merged_noRT_v3.csv", stringsAsFactors = FALSE)
RT <- read.csv("/home/shares/soilcarbon/Twitter/Merged_v3/twitter_merged_v3.csv",stringsAsFactors = FALSE)

#### MANIPULATE DATA ####
noRT_no_india <- clean_data(noRT, rm_india = T)
RT_no_india <- clean_data(RT, rm_india = T)

# Filter out rewtweets less than 5
rt_limit <- noRT_clean %>% 
  filter(retweet_count > 5)

decay <- list()

## identify rts
# this will take a while what rt_limit is set
for (i in 1:nrow(rt_limit)) {
  tmp <- find_rt(i, rt_limit, RT_clean)
  tmp <- tmp %>% 
    mutate(
      prop = round(number/sum(number),4)
    )
  decay[[i]] <- tmp
  
}

test <- bind_rows(decay)

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

# define which search terms you want to include
terms <- c("soilhealth", "rangelandhealth", "regenerativeagriculture")

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


