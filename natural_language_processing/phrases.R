#######################################################
# Quantify top phrases used in tweets by search terms #
#######################################################


#### Load packages ####
library(tidyverse)
library(tidytext)
library(stringr)
library(ggraph)
library(igraph)
library(tm)
library(NLP)
library(quanteda)
library(SnowballC)
source("soc-twitter/text_analysis_functions.R")


#### Load data ####
noRT_no_india <- read.csv("soc-twitter/data_for_analysis/cleaned_data/noRT_clean.csv", stringsAsFactors = FALSE) 
noRT <- read.csv("soc-twitter/data_for_analysis/cleaned_data/noRT_clean_india.csv", stringsAsFactors = FALSE)
RT <- read.csv("soc-twitter/data_for_analysis/cleaned_data/RT_clean.csv", stringsAsFactors = FALSE)


#### Manipulate data ####
# Select tweets based on 10% most retweeted 
top_10_noRT <- noRT %>% 
  arrange(-retweet_count) %>% 
  head(.1 * nrow(noRT))
top_10_noRT_no_IN <- noRT_no_india %>% 
  arrange(-retweet_count) %>% 
  head(.1 * nrow(noRT))

# Break out common phrases by search term
## soil health
soil_health_tweets <- noRT %>% 
  filter(
    str_detect(tolower(text), paste(c("soil health","#soilhealth","healthy soil","#healthysoil"), collapse = '|')))
soil_health_tweets$hits <- "soil health"
## soil health no India
soil_health_noIN_tweets <- noRT_no_india %>% 
  filter(
    str_detect(tolower(text), paste(c("soil health","#soilhealth","healthy soil","#healthysoil"), collapse = '|')))
soil_health_noIN_tweets$hits <- "soil health"
## regenerative agriculture
regen_agri_tweets <- noRT %>% 
  filter(
    str_detect(tolower(text), paste(c("regenerative agriculture","#regenerativeagriculture"), collapse = '|')))
regen_agri_tweets$hits <- "regenerative agriculture"


#### DETECT PHRASES ####
# Full noRT dataset
tstat_col_caps <- phrases(noRT, 50)
head(tstat_col_caps, 20)
# Top 10% noRT
tstat_col_caps_100 <- phrases(top_10_noRT, 50)
head(tstat_col_caps_100, 20)
# Soil health
soil_health_col_caps <- phrases(soil_health_tweets, 50)
head(soil_health_col_caps, 20)
# Soil health no India
soil_health_noIN_col_caps <- phrases(soil_health_noIN_tweets, 50)
head(soil_health_noIN_col_caps, 20)
# Top 10% soil health
sh_top10 <- top_10_noRT %>% 
  filter(
    str_detect(tolower(text), paste(c("soil health","#soilhealth","healthy soil","#healthysoil"), collapse = '|')))
sh_top10$hits <- "soil health"
sh_top10 <- phrases(sh_top10, 50)
head(sh_top10, 20)
# Top 10% soil health no India
sh_noIN_top10 <- top_10_noRT_no_IN %>% 
  filter(
    str_detect(tolower(text), paste(c("soil health","#soilhealth","healthy soil","#healthysoil"), collapse = '|')))
sh_noIN_top10$hits <- "soil health"
sh_noIN_top10 <- phrases(sh_noIN_top10, 50)
head(sh_noIN_top10, 20)
# regenerative agriculture
regen_agri_col_caps <- phrases(regen_agri_tweets, 50)
head(regen_agri_col_caps, 20)
# Top 10% regenerative
rg_top10 <- top_10_noRT %>% 
  filter(
    str_detect(tolower(text), paste(c("regenerative agriculture","#regenerativeagriculture"), collapse = '|')))
rg_top10$hits <- "regenerative agriculture"
rg_top10 <- phrases(rg_top10, 50)
head(rg_top10, 20)


#### PREPARE AND EXPORT TABLES ####
export <- cbind(
  head(soil_health_col_caps, 20)[1:2],
  head(soil_health_noIN_col_caps, 20)[1:2],
  head(regen_agri_col_caps, 20)[1:2]
)
names(export) <- c("Soil Health","Count","Soil Health, No India","Count","Regenerative Agriculture","Count")
write_csv(export, "soc-twitter/tables/Table2.csv")


#### REMOVE ALL OBJECTS ####
rm(list = ls())
