#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# functions for basic text analysis    #
# and visualization of tweets          #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(tidyverse)
library(tidytext)
library(stringr)
library(igraph)
library(ggraph)








#' word_umbrella combines various terms into a single word 'umbrella' 
#'
#' @param data any data frame with a column 'text' that is a text string
#'
#' @return replaces a variety of terms with a single 'umbrella' term
#' 
#'
#' @examples
word_umbrella <- function(data) {
  
  
  #list of terms that fall under a single umbrella
  soil_health <- c("healthy soil", "soilhealth", "healthysoil", "soil health")
  soil_qual <- c("soil quality", "soilquality")
  soil_fert <- c("soil fertility", "soilfertility")
  regen_ag <- c("regenerative agriculture",	"regenerativeagriculture", "regenerative ag", "regenerative agricultural")
  conserv_ag <- c("conservation agriculture",	"conservationagriculture",	"conservationag")
  cover_crop <- c("cover crop",	"cover cropping",	"cover crops",	"covercrop",	"covercropping",	"covercrops")
  conserv_till <- c("conservation tillage",	"no till",	"reduced till",	"reduced tillage",	"no tillage",	'notill',	"reducedtill",	"reducedtillage",	"notillage",	"conservationtillage",	"conservationtill",	"conservation till")
  rangeland <- c("rangeland health",	"healthy rangelands",	"rangelandhealth",	"healthyrangelands")
  health_card <- c("soil health card",	"soil health cards",	"soilhealthcard",	"soilhealthcards")
  n_modi <- c("narendra modi",	"narendramodi",	"narendra",	"modi") # have to use str_replace vs str_replace_all for this one
  
  #replace text
  umbrella <- data %>% 
    mutate(
      text = str_replace_all(tolower(text), str_c(soil_qual, collapse = "|"), "soil_quality") , 
      text = str_replace_all(tolower(text), str_c(soil_fert, collapse = "|"), "soil_fertility") , 
      text = str_replace_all(tolower(text), str_c(regen_ag, collapse = "|"), "regenerative_agriculture") , 
      text = str_replace_all(tolower(text), str_c(conserv_ag, collapse = "|"), "conservation_agriculture") , 
      text = str_replace_all(tolower(text), str_c(cover_crop, collapse = "|"), "cover_crop") , 
      text = str_replace_all(tolower(text), str_c(conserv_till, collapse = "|"), "conservation_tillage") , 
      text = str_replace_all(tolower(text), str_c(rangeland, collapse = "|"), "rangeland_health") , 
      text = str_replace_all(tolower(text), str_c(health_card, collapse = "|"), "soil_health_card") , 
      text = str_replace(tolower(text), str_c(n_modi, collapse = "|"), "Narendra_Modi") , 
      text = str_replace_all(tolower(text), "soil health institute", "soil_health_institute") , 
      text = str_replace_all(tolower(text), "soil health partnership", "soil_health_partnership") , 
      text = str_replace_all(tolower(text), "soil organic matter", "soil_organic_matter") , 
      text = str_replace_all(tolower(text), "soil carbon", "soil_carbon") , 
      text = str_replace_all(tolower(text), "aggregate stability", "agregate_stability") , 
      text = str_replace_all(tolower(text), "microbial biomass", "microbial_biomass") , 
      text = str_replace_all(tolower(text), "crop rotation", "crop_rotation") , 
      text = str_replace_all(tolower(text), "crop insurance", "crop_insurance") , 
      text = str_replace_all(tolower(text), "climate change", "climate_change") , 
      text = str_replace_all(tolower(text), "food security", "food_security"),
      text = str_replace_all(tolower(text), str_c(soil_health, collapse = "|"), "soil_health") , 
    )
  
  return(umbrella)
  
}


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

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
#' @param data - a data frame (must containg a variable `text`)
#'
#' @return data frame of two rows (word, n) where n is the count of each 
#'         respective word minus stop words
#' 
#'
#' @examples
#' tweet <- data.frame(text = "the bird said Tweet tweet")
#' prepare_text(tweet)
prepare_text <- function(data) {
  
  grouped_terms <- word_umbrella(data)
  
  text_words <- grouped_terms %>% 
    select(text) %>% 
    mutate(text = tolower(text)) %>% #make all text lower case
    unnest_tokens(word, text) %>%  #takes each rows' string and separates each word into a new row
    mutate(word = sub("'s$", "", word),  # remove possessives
           word = sub("cards", "card", word),
           word = sub("crops", "crop", word),
           word = sub("improves", "improve", word))
  
  #new pipeline as R doesnt like going from `unnest_tokens` to anti_join
  text_words %>% 
    anti_join(stop_words)  %>% #remove common stop words using the tidytext built in stop words
    count(word, sort=TRUE) %>% 
    filter(!word %in% c("https","rt","t.co","amp")) %>% #remove words associated with images/links and special characters, (i.e. amp = &)
    filter(!word %in% c("soil","health", "healthy", "soilhealth")) #These terms consistently come out as top words perhaps as an atrifact of the initial querry, so i remove them here
}


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

#' Same as prepare_text() but doesnt filter for the top words
#' 
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
#' @param data - a data frame (must containg a variable `text`)
#'
#' @return data frame of two rows (word, n) where n is the count of each 
#'         respective word minus stop words
#' 
#'
#' @examples
#' tweet <- data.frame(text = "the bird said Tweet tweet")
#' prepare_text(tweet)
prepare_text_full <- function(data) {
  
  grouped_terms <- word_umbrella(data)
  
  text_words <- grouped_terms %>% 
    select(text) %>% 
    mutate(text = tolower(text)) %>% #make all text lower case
    unnest_tokens(word, text) %>% #takes each rows' string and separates each word into a new row
    mutate(word = sub("'s$", "", word),
           word = sub("cards", "card", word),
           word = sub("crops", "crop", word),
           word = sub("improves", "improve", word))
  
  #new pipeline as R doesnt like going from `unnest_tokens` to anti_join
  text_words %>% 
    anti_join(stop_words) %>% #remove common stop words using the tidytext built in stop words
    count(word, sort=TRUE) %>% 
    filter(!word %in% c("https","rt","t.co","amp")) #remove words associated with images/links and special characters, (i.e. amp = &)
}


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

#' an admittedly very specific function that uses the code from `prepare_text`
#' and creates a word cloud using the resultant word counts
#' 
#' requires- tidytext:: and stringr::
#'
#' @param data - any data from with acolumn `text` containg word strings
#' @param filter_by search term used that relies on str_detect to only select 
#'        tweets that contain your term of interest, default set to no filter
#'
#' @return - a wordcloud based on the dataframe and search term
#' 
#'
#' @examples
create_wordcloud <- function(data, filter_by = "") {
  
  grouped_terms <- word_umbrella(data)
  
  text_words <- grouped_terms %>% 
    filter(
      str_detect(tolower(text), filter_by)) %>% #selects only rows that cointain your term of interest
    select(text) %>% 
    mutate(text = tolower(text)) %>% 
    unnest_tokens(word, text) %>%  #takes each rows' string and separates each word into a new row
    mutate(word = sub("'s$", "", word),
           word = sub("cards", "card", word),
          word = sub("crops", "crop", word),
          word = sub("improves", "improve", word))
  
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

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####


#' creates a list of bigrams and their counts 
#' the function is set to remove tweet specific stop words 'c("https","rt","t.co","amp")'
#'
#'requires- tidytext:: and stringr::
#'
#'
#' @param data - a dataframe with a column called: 'text'
#' @param filter_by search term to filter by. default set to no filter
#'
#' @return list of bigrams and their counts 
#' 
#'
#' @examples
create_bigram <- function(data, filter_by = "", group=FALSE) {
  
  
  filtered <- data %>% 
    filter(
      str_detect(tolower(text), filter_by)) #select only rows that contain search term of interest
  
  if (group) {grouped_terms <- word_umbrella(filtered)}
  else {
    grouped_terms <- filtered}
  
  bigrams <- grouped_terms %>% 
    select(text) %>% 
    mutate(text = tolower(text)) %>% 
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% #creates sigle column of all possible bigrams from text strings
    count(bigram, sort = TRUE) 
  
  bigrams_separated <- bigrams %>% 
    separate(bigram, c("word1", "word2"), sep = " ") #separate the bigrams so that stop words can be filtered out
  
  bigrams_filtered <- bigrams_separated %>%
    mutate(word1 = sub("'s$", "", word1),
           word1 = sub("cards", "card", word1),
           word1 = sub("crops", "crop", word1),
           word1 = sub("improves", "improve", word1)) %>% 
    filter(!word1 %in% stop_words$word) %>%
    filter(!word1 %in% c("https","rt","t.co","amp")) %>% 
     mutate(word2 = sub("'s$", "", word2),
            word2 = sub("cards", "card", word2),
            word2 = sub("crops", "crop", word2),
            word2 = sub("improves", "improve", word2)) %>% 
    filter(!word2 %in% stop_words$word) %>% 
    filter(!word2 %in% c("https","rt","t.co","amp"))
  
  bigrams_united <- bigrams_filtered %>%
    unite(bigram, word1, word2, sep = " ") #rejoin words back into bigrams
  
}



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####



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














