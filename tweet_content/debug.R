library(tidyverse)
library(tidytext)
library(stringr)
library(ggraph)
library(igraph)


noRT <- read.csv("twitter_merged_noRT.csv", stringsAsFactors = FALSE)


source("text_analysis_functions.R")



foo <- noRT %>% 
  filter(
    str_detect(tolower(text), str_c(soil_health, collapse = "|"))
  ) %>% 
  slice(300:600)


bar <- create_bigram(foo, group = T)  