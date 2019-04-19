#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# functions for basic text analysis    #
# and visualization of tweets          #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(tidyverse)
library(tidytext)
library(stringr)
library(igraph)
library(ggraph)

#' This takes any data frame that contains a column  called `.$text` 
#'  containing text strings and returns a new dataframe that lists 
#'  the words within .$text and their respective counts
#'  
#'  the function is set to remove tweet specific stop words 'c("https","rt","t.co","amp")'
#'  as well as dominant terms used in this analysis c("soil","health", "healthy", "soilhealth")
#'  this can be changed in the future by adding a new parameter "stopwords" of custom terms
#'  
#'  requires: tidytext::
#'
#' @param x - a data frame (must containg a variable `text`)
#'
#' @return data frame of two rows (word, n) where n is the count of each 
#'         respective word minus stop words
#' 
#'
#' @examples
#' tweet <- data.frame(text = "the bird said Tweet tweet")
#' prepare_text(tweet)
prepare_text <- function(x) {
  text_words <- x %>% 
    select(text) %>% 
    mutate(text = tolower(text)) %>% #make all text lower case
    unnest_tokens(word, text) #takes each rows' string and separates each word into a new row
  
  #new pipeline as R doesnt like going from `unnest_tokens` to anti_join
  text_words %>% 
    anti_join(stop_words) %>% #remove common stop words using the tidytext built in stop words
    count(word, sort=TRUE) %>% 
    filter(!word %in% c("https","rt","t.co","amp")) %>% #remove words associated with images/links and special characters, (i.e. amp = &)
    filter(!word %in% c("soil","health", "healthy", "soilhealth")) #These terms consistently come out as top words perhaps as an atrifact of the initial querry, so i remove them here
}



#' an admittedly very specific function that uses the code from `prepare_text`
#' and creates a word cloud using the resultant word counts
#' 
#' requires- tidytext:: and stringr::
#'
#' @param x - any data from with acolumn `text` containg word strings
#' @param filter_by search term used that relies on str_detect to only select 
#'        tweets that contain your term of interest, default set to no filter
#'
#' @return - a wordcloud based on the dataframe and search term
#' 
#'
#' @examples
create_wordcloud <- function(x, filter_by = "") {
  
  #english <- x[which(!grepl("[^\x01-\x7F]+", x$text)),] #removes rows non-english characters - would be great if we could figure out a way to simply remove/replace the specific words and not the whole row.....
  
  text_words <- x %>% 
    filter(
      str_detect(tolower(text), filter_by)) %>% #selects only rows that cointain your term of interest
    select(text) %>% 
    mutate(text = tolower(text)) %>% 
    unnest_tokens(word, text) #takes each rows' string and separates each word into a new row
  
  filtered <- text_words %>% 
    anti_join(stop_words) %>% 
    count(word, sort=TRUE) %>% 
    filter(!word %in% c("https","rt","t.co","amp")) %>% #remove words associated with images/links and special characters, (i.e. amp = &)
    filter(!word %in% c("soil","health", "healthy", "soilhealth"))  #These terms consistently come out as top words perhaps as an atrifact of the initial querry, so i remove them here
  
  
  filtered %>% 
    with(wordcloud(word, n, 
                   min.freq = 1,
                   max.words=200, 
                   random.order=FALSE, 
                   color=brewer.pal(7,"Dark2")))
}




#' creates a list of bigrams and their counts 
#' the function is set to remove tweet specific stop words 'c("https","rt","t.co","amp")'
#'
#'requires- tidytext:: and stringr::
#'
#'
#' @param x - a dataframe with a column called: 'text'
#' @param filter_by search term to filter by. default set to no filter
#'
#' @return list of bigrams and their counts 
#' 
#'
#' @examples
create_bigram <- function(x, filter_by = "") {
  
  
  filtered <- x %>% 
    filter(
      str_detect(tolower(text), filter_by)) #select only rows that contain search term of interest
  
  bigrams <- filtered %>% 
    select(text) %>% 
    mutate(text = tolower(text)) %>% 
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% #creates sigle column of all possible bigrams from text strings
    count(bigram, sort = TRUE) 
  
  bigrams_separated <- bigrams %>% 
    separate(bigram, c("word1", "word2"), sep = " ") #separate the bigrams so that stop words can be filtered out
  
  bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word1 %in% c("https","rt","t.co","amp")) %>% 
    filter(!word2 %in% stop_words$word) %>% 
    filter(!word2 %in% c("https","rt","t.co","amp"))
  
  bigrams_united <- bigrams_filtered %>%
    unite(bigram, word1, word2, sep = " ") #rejoin words back into bigrams
}







#' returns a network graph that shows the directionality of how terms are arranged using arrows
#' and colors the arrows based on the relative occurance of those terms together (darker = higher count)
#' 
#' see: https://www.tidytextmining.com/ngrams.html for for information
#'
#' @param data - a dataframe of bigrams (works best when used in conjuction with `create_bigram`) 
#'               column title for bigrams must be bigram
#' @param limit - sets the filter/cutoff for which bigram terms to use based on the count of their occurance 
#' - use larger numbers for larger datasets
#'
#' @return a network graph of bigrams showing directionality
#' 
#'
#' @examples
gram_network <- function(data, limit) {
  bigram_graph <- data %>% 
    separate(bigram, c("word1", "word2"), sep = " ") %>% #separates bigrams into two columns
    filter(n > limit) %>%
    igraph::graph_from_data_frame() #generates a an igraph graph (resembiling a table) showing direction of terms
  
  set.seed(2019) # ensures consistency in output
  
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))  #adds arrows connecting each "node" (word in this case)
  
  ggraph::ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = T,
                   arrow = a, end_cap = circle(.07, 'inches')) + #defines how the edges are visualized
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void() #aesthetic
}














