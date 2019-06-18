#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# functions for basic text analysis    #
# and visualization of tweets          #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(tidyverse)
library(tidytext)
library(stringr)
library(igraph)
library(ggraph)
library(SnowballC)







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
  regen_ag <- c("regenerative agriculture",	"regenerativeagriculture", "regenerative ag", "regenerative agricultural", "regenerativeag")
  conserv_ag <- c("conservation agriculture",	"conservationagriculture",	"conservationag")
  cover_crop <- c("cover crop",	"cover cropping",	"cover crops",	"covercrop",	"covercropping",	"covercrops")
  conserv_till <- c("conservation tillage",	"no till",	"reduced till",	"reduced tillage",	"no tillage",	'notill',	"reducedtill",	"reducedtillage",	"notillage",	"conservationtillage",	"conservationtill",	"conservation till")
  rangeland <- c("rangeland health",	"healthy rangelands",	"rangelandhealth",	"healthyrangelands")
  health_card <- c("soil health card",	"soil health cards",	"soilhealthcard",	"soilhealthcards")
  healthy_people <- c('human health', 'healthy people')
  organic_ag <- c('organic agriculture', 'organic ag','sustainable ag','sustainable agriculture')
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
      text = str_replace_all(tolower(text), str_c(healthy_people, collapse = "|"), "healthy_people") , 
      text = str_replace_all(tolower(text), str_c(organic_ag, collapse = "|"), "organic_ag") , 
      text = str_replace(tolower(text), str_c(n_modi, collapse = "|"), "Narendra_Modi") , 
      text = str_replace_all(tolower(text), "soil health institute", "soil_health_institute") , 
      text = str_replace_all(tolower(text), "forest health", "forest_health") , 
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
prepare_text <- function(data, group = FALSE, stem = FALSE) {
  
  if (group) {terms <- word_umbrella(data)}
  else {
    terms <- data}
  
  reg_words <- "([^A-Za-z_\u0900-\u097F\\d#@']|'(?![A-Za-z_\u0900-\u097F\\d#@]))" # regex expresions that we want to retain when creating tokens. includes all roman and hindi characters, and retains @ and #
  text_words <- terms %>% 
    select(text) %>% 
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>% # remvove anything associated with hyperlinks
    mutate(text = tolower(text)) %>% 
    unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
    filter(!word %in% stop_words$word)
  
  
  if (stem) {
    text_words <- text_words %>% 
      mutate(word = wordStem(word, language="english"))
  }
  
  
  text_words %>% 
    count(word, sort=TRUE) #These terms consistently come out as top words perhaps as an atrifact of the initial querry, so i remove them here
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
create_wordcloud <- function(data, filter_by = "", group = FALSE, stem = FALSE) {
  
  filtered <- data %>% 
    filter(
      str_detect(tolower(text), filter_by)) #selects only rows that cointain your term of interest
  
  if (group) {grouped_terms <- word_umbrella(filtered)}
  else {
    grouped_terms <- filtered}
  
  text_words <- grouped_terms %>% 
    select(text) %>% 
    mutate(text = tolower(text)) %>% 
    unnest_tokens(word, text)   #takes each rows' string and separates each word into a new row

  if (stem) {
    text_words <- text_words %>% 
      mutate(word = wordStem(word, language="english"))
  }
  
  filtered <- text_words %>% 
    anti_join(stop_words) %>% 
    count(word, sort=TRUE) %>% 
    filter(!word %in% c("https","rt","t.co","amp")) %>% #remove words associated with images/links and special characters, (i.e. amp = &)
    filter(!word %in% c("soil","health", "soil_health"))  #These terms consistently come out as top words perhaps as an atrifact of the initial querry, so i remove them here
  
  
  filtered %>% 
    with(wordcloud(word, n, 
                   min.freq = 1,
                   max.words=200, 
                   random.order=FALSE, 
                   color=brewer.pal(7,"Dark2")))
}

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####


#' creates a list of bigrams and their counts 
#' 
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
create_bigram <- function(data, filter_by = "", group = FALSE, stem = FALSE) {
  
  #select only rows that contain search term of interest
  filtered <- data %>% 
    filter(
      str_detect(tolower(text), filter_by)) 
  
  #if group = TRUE the group terms into their umbrella term
  if (group) {terms <- word_umbrella(filtered)}
  else {terms <- filtered}
  
  
  reg_words <- "([^A-Za-z_\u0900-\u097F\\d@']|'(?![A-Za-z_\u0900-\u097F\\d@]))" # regex expresions that we want to retain when creating tokens. includes all roman and hindi characters, and retains @ 
  
  bigrams_separated <- terms %>%
    select(text, user_id) %>% 
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
    unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
    mutate(next_word = lead(word)) %>%
    filter(!word %in% stop_words$word, # remove stop words
           !next_word %in% stop_words$word) 
  
  if (stem) {
    bigrams_separated<- bigrams_separated %>% 
      mutate(word = wordStem(word, language="english"),
             next_word = wordStem(next_word, language="english"))
  } 
  
  bigrams_united <- bigrams_separated %>% 
    unite(bigram, word, next_word, sep = ' ') %>%
    select(bigram)  %>% 
    count(bigram, sort = TRUE) 
  
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
  set.seed(2019) # ensures consistency in output
  
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))  #adds arrows connecting each "node" (word in this case)
  
  bigrams <- data %>% 
    separate(bigram, c("word1", "word2"), sep = " ") %>% #separates bigrams into two columns
    filter(n > limit)
  
  counts <- bigrams %>% 
    gather(item, word, word1, word2) %>% 
    group_by(word) %>% 
    summarise(n = sum(n))
  #generates a an igraph graph (resembiling a table) showing direction of terms
  
  bigrams %>% 
    igraph::graph_from_data_frame(vertices = counts) %>% 
    ggraph::ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = T,
                   arrow = a, end_cap = circle(.07, 'inches')) + #defines how the edges are visualized
    geom_node_point(color = "lightblue", aes(size = n)) +
    scale_size(range = c(2,8)) +
    geom_node_text(aes(label = name), repel = T) +
    theme_void() 
}






