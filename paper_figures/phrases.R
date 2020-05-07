## this creates two csv's for the top 100 phrases based on sail health and regen ag search terms

# load packages
library(tidyverse)
library(tidytext)
library(stringr)
library(ggraph)
library(igraph)
library(tm)
library(NLP)
library(quanteda)
library(SnowballC)
source("text_analysis_functions.R")

# load data
noRT_clean <- read.csv("/home/shares/soilcarbon/Twitter/cleaned_data/noRT_clean.csv", stringsAsFactors = FALSE)


#### soil health
input <- noRT_clean

soil_health_tweets <- input %>% 
  filter(
    str_detect(tolower(text), paste(c("soil health","#soilhealth","healthy soil","#healthysoil"), collapse = '|')))
soil_health_tweets$hits <- "soil health"

soil_health_col_caps <- phrases(soil_health_tweets, 200)

counts <- head(soil_health_col_caps, 100)

##for table in manuscript
write_csv(counts[,1:2], "soil_health_phrases.csv")



#### regenerative agriculture
regen_agri_tweets <- input %>% 
  filter(
    str_detect(tolower(text), paste(c("regenerative agriculture","#regenerativeagriculture"), collapse = '|')))
regen_agri_tweets$hits <- "regenerative agriculture"


regen_agri_col_caps <- phrases(regen_agri_tweets, 50)

counts_regen <- head(regen_agri_col_caps, 100)

write_csv(counts_regen[,1:2], "regen_phrases.csv")





