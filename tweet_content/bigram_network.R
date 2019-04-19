library(tidyverse)
library(tidytext)
library(stringr)
library(ggraph)
library(igraph)


twitter_merged <- read.csv("twitter_merged.csv", stringsAsFactors = FALSE)
twitter_merged_noRT <- read.csv("twitter_merged_noRT.csv", stringsAsFactors = FALSE)

##remove duplications
twitter_merged_noRT <- distinct(twitter_merged_noRT)
twitter_merged <- distinct(twitter_merged)



noRT <- twitter_merged_noRT %>% 
  arrange(-retweet_count) %>% 
  filter(screen_name != "Pontifex") #remove the pope..


#function for creating bigram
#' Title
#'
#' @param x 
#' @param filter_by 
#'
#' @return
#' @export
#'
#' @examples
create_bigram <- function(x,filter_by) {
  
  filtered <- x %>% 
    filter(
      str_detect(tolower(text), filter_by))
  
bigrams <- filtered %>% 
  select(text) %>% 
  mutate(text = tolower(text)) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  count(bigram, sort = TRUE)

bigrams_separated <- bigrams %>% 
separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word1 %in% c("https","rt","t.co","amp")) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word2 %in% c("https","rt","t.co","amp"))

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
}



#function for creating a word network 
gram_network <- function(data,count) {
  bigrams_separated <- data %>% 
    separate(bigram, c("word1", "word2"), sep = " ")
  
  bigram_graph <- bigrams_separated %>%
    filter(n > count) %>%
    graph_from_data_frame()
  
  set.seed(2019)
  
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}


####full dataset####
noRT_bigram <- create_bigram(noRT, "")
gram_network(noRT_bigram,200)

###soil
soil_bigram <- create_bigram(noRT, "soil health")
gram_network(soil_bigram, 100)


###rangeland
range_bigram <- create_bigram(noRT, "rangeland health")
gram_network(range_bigram, 1)

###
forest_bigram <- create_bigram(noRT, "forest health")
gram_network(forest_bigram, 0)
















