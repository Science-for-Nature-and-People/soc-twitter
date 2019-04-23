#### identifying top tweets ####
# question: filter by category first or select 'top' first?
# how to filter 'top':
#~~ top 100
#~~ great than 100 RT
#~~ most commented 
#~~~~~ how to ID commented?
#~~ most 'favorited'
#~~some sort of weighted metric ie RT_count*favorite_count


###initial observations:
#~ searching for "rangeland/forest health" or "rangeland/forest quality" yields very few results among the most popular tweets
#~~using only "soil" "rangeland" and "forest" is more insightfull


library(tidyverse)
library(tidytext)
library(stringr)
library(ggraph)
library(igraph)
library(wordcloud)


twitter_merged <- read.csv("twitter_merged.csv", stringsAsFactors = FALSE) %>% 
  distinct()
twitter_merged_noRT <- read.csv("twitter_merged_noRT.csv", stringsAsFactors = FALSE) %>% 
  distinct()


#function for creating bigram
#~do we want to remove non-english characters?
create_bigram <- function(x,filter_by) {
  
  #english <- x[which(!grepl("[^\x01-\x7F]+", x$text)),] #removes rows non-english characters - would be great if we could figure out a way to simply remove/replace the specific words and not the whole row.....
  
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

#function for creating a bigram word network 
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

#function for creating word cloud
create_wordcloud <- function(x, filter_by) {
  
  #english <- x[which(!grepl("[^\x01-\x7F]+", x$text)),] #removes rows non-english characters - would be great if we could figure out a way to simply remove/replace the specific words and not the whole row.....
  
  text_words <- english %>% 
    filter(
      str_detect(tolower(text), filter_by)) %>% 
    select(text) %>% 
    mutate(text = tolower(text)) %>% 
    unnest_tokens(word, text)
  
  filtered <- text_words %>% 
    anti_join(stop_words) %>% 
    count(word, sort=TRUE) %>% 
    filter(!word %in% c("https","rt","t.co","amp","[^\x01-\x7F]+")) %>% #remove words associated with images/links and special characters, (i.e. amp = &)
    filter(!word %in% c("soil","health", "healthy", "soilhealth"))  #These terms consistently come out as top words perhaps as an atrifact of the initial querry, so i remove them here

  
  filtered %>% 
    with(wordcloud(word, n, 
                   min.freq = 1,
                   max.words=200, 
                   random.order=FALSE, 
                   color=brewer.pal(7,"Dark2")))
}


####view top 100 RT (no filter)####
top_100_RT <- twitter_merged_noRT %>% 
  arrange(-retweet_count) %>% 
  head(100)

top_100_bigrm <- create_bigram(top_100_RT, "")
gram_network(top_100_bigrm, 100)

create_wordcloud(top_100_RT, "")

####view >50 RT (no filter)####
###summary, this (no suprise) yields very similar results depending on filter range


#explore range/distribution of RT to help define ^'X'
RT_counts <- twitter_merged_noRT %>% 
  filter(screen_name != "Pontifex") %>% 
  group_by(retweet_count) %>% 
  summarise(
    count = n()
  )
plot(RT_counts)
#vast magority have <~100 RT
## when filtering for >100 n=30
##try 50 - yields n=61

grtr_50_RT <- twitter_merged_noRT %>% 
  filter(screen_name != "Pontifex") %>% 
  filter(retweet_count > 50)


grtr_50_bigrm <- create_bigram(grtr_50_RT, "")
gram_network(grtr_50_bigrm, 1)

create_wordcloud(grtr_100_RT, "")


#### filtering most top_100_RT by "soil"####
soil_bigram <- create_bigram(top_100_RT, "soil")
gram_network(soil_bigram, 1)
create_wordcloud(top_100_RT, "soil")

#### filtering top_100_RT by "rangeland"####

#rangeland_bigram <- create_bigram(top_100_RT, "rangeland")
#~there are no mentions of rangeland in top_100_RT, therefore we have to first filter for "rangeland" and then select the top x number of tweets

top_range <- twitter_merged_noRT %>% 
  filter(
    str_detect(tolower(text), "rangeland") & 
      retweet_count > 0)

rangeland_bigram <- create_bigram(top_range, "")
gram_network(rangeland_bigram, 0)
create_wordcloud(top_range, "")

#### filtering top_100_RT by "forest"####

forest_bigram <- create_bigram(top_100_RT, "forest")
gram_network(forest_bigram, 0)
create_wordcloud(top_100_RT, "forest")












#### filtering top_100_RT by "* health" ####
###soil
soil_health_bigram <- create_bigram(top_100_RT, "soil health")
gram_network(soil_health_bigram, 2)
create_wordcloud(top_100_RT, "soil health")

###rangeland
range_health_bigram <- create_bigram(top_range, "rangeland health") # used `top_range` df which filters for hits on "rangeland" with retweet_count > 0
gram_network(range_health_bigram, 0)
create_wordcloud(top_range, "rangeland health")


###forest
top_forest <- twitter_merged_noRT %>% 
  filter(
    str_detect(tolower(text), "forest") & 
      retweet_count > 0)

forest_health_bigram <- create_bigram(top_forest, "forest health")
#length = 0


#### View 100 most 'favorited' ####
#these also very similar to top_RT
most_fvrt <- twitter_merged_noRT %>% 
  arrange(-favorite_count) %>% 
  head(100)

most_fvrt_bigram <- create_bigram(most_fvrt, "")
gram_network(most_fvrt_bigram, 2)
create_wordcloud(most_fvrt, "")











