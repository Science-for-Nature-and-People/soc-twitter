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
library(plyr)



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
##                    word_unbrella                            ##
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

#' word_umbrella combines various terms into a single word 'umbrella' 
#' this function has so far only ben embedded into other functions like `prepare_text()` and `create_bigram()`
#' 
#' requires tidyverse:: & stringr::
#'
#' @param data any data frame with a column 'text' that is a text string
#'
#' @return replaces a variety of terms with a single 'umbrella' term
#' 
#'
#' @examples
#' 
#' data <- data.frame(text = 'regenerativeag that includes no till and cover crops lead to #healthysoil)
#' 
#' word_umbrella(data)
#' # 1 regenerative_agriculture that includes conservation_tillage and cover_crops lead to #soil_health
#' 
word_umbrella <- function(data) {
  
  
  #list of terms that fall under a single umbrella
  soil_health <- c("healthy soil", "soilhealth", "healthysoil", "soil health")
  soil_qual <- c("soil quality", "soilquality")
  soil_fert <- c("soil fertility", "soilfertility")
  carbon_seq <- c('sequester carbon','carbon sequestration','sequestering carbon','sequesters carbon','sequestered carbon')
  regen_ag <- c("regenerative agriculture",	"regenerativeagriculture", "regenerative ag", "regenerative agricultural", "regenerativeag")
  conserv_ag <- c("conservation agriculture",	"conservationagriculture",	"conservationag")
  cover_crop <- c("cover crop",	"cover cropping",	"cover crops",	"covercrop",	"covercropping",	"covercrops")
  conserv_till <- c("conservation tillage",	"no till",	"reduced till",	"reduced tillage",	"no tillage",	'notill',	"reducedtill",	"reducedtillage",	"notillage",	"conservationtillage",	"conservationtill",	"conservation till")
  rangeland <- c("rangeland health",	"healthy rangelands",	"rangelandhealth",	"healthyrangelands")
  health_card <- c("soil health card",	"soil health cards",	"soilhealthcard",	"soilhealthcards")
  healthy_people <- c('human health', 'healthy people')
  organic_ag <- c('organic agriculture', 'organic ag','sustainable ag','sustainable agriculture','organic farming','organic farm')
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
      text = str_replace_all(tolower(text), str_c(carbon_seq, collapse = "|"), "carbon_sequestration") , 
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
      text = str_replace_all(tolower(text), "climatechange", "climate_change") ,
      text = str_replace_all(tolower(text), "food security", "food_security"),
      text = str_replace_all(tolower(text), str_c(soil_health, collapse = "|"), "soil_health") , 
    )
  
  return(umbrella)
  
}

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
##                   prepare_text                              ##
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

#' This takes any data frame that contains a column  called `text` 
#'  containing text strings and returns a new dataframe that lists 
#'  the words within .$text and their respective counts
#'  
#'  the function is set to remove tweet specific stop words 'c("https","rt","t.co","amp")'
#'  
#'  
#'  requires: tidytext::
#'
#' @param data - a data frame (must containg a variable `text`)
#' @param group - T/F to employ the word_umbrella function
#' @param stem - T/F to stem words
#'
#' @return data frame of two rows (word, n) where n is the count of each 
#'         respective word minus stop words
#' 
#'
#' @examples
#' tweet <- data.frame(text = "the bird said Tweet tweeted no till")
#' prepare_text(tweet)
#' 
#'   word      n
#'   <chr>   <int>
#' 1 bird        1
#' 2 till        1
#' 3 tweet       1
#' 4 tweeted     1
#'  
#'  
#'  prepare_text(tweet, group = TRUE, stem = TRUE)
#'     word                    n
#'    <chr>                 <int>
#'  1 tweet                   2
#'  2 bird                    1
#'  3 conservation_tillag     1
#' 
#' 
#' 
prepare_text <- function(data, group = FALSE, stem = FALSE) {
  
  # if group is set to TRUE, then run the word_umbrella function
  if (group) {terms <- word_umbrella(data)}
  else {
    terms <- data}
  
  # define regex that we want to retain when creating tokens. includes all roman and hindi characters, and retains @ and #
  reg_words <- "([^A-Za-z_\u0900-\u097F\\d#@']|'(?![A-Za-z_\u0900-\u097F\\d#@]))" 
  
  # create tokens
  text_words <- terms %>% 
    select(text) %>% 
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>% # remvove anything associated with hyperlinks
    mutate(text = tolower(text)) %>% #make all lower case
    unnest_tokens(word, text, token = "regex", pattern = reg_words) %>% # unnest words based on the regex defined above
    filter(!word %in% stop_words$word) # remove stop words
  
  # if stem is TRUE, then stem words
  if (stem) {
    text_words <- text_words %>% 
      mutate(word = wordStem(word, language="english"))
  }
  
  #get word counts and arrange in decending order
  text_words %>% 
    count(word, sort=TRUE) 
}


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
##                    create_wordcloud                         ##
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

#' an admittedly very specific function that uses the code from `prepare_text`
#' and creates a word cloud using the resultant word counts
#' 
#' requires- tidytext:: and stringr::
#'
#' @param data - any data from with acolumn `text` containg word strings
#' @param filter_by search term used that relies on str_detect to only select 
#'        tweets that contain your term of interest, default set to no filter
#' @param group - T/F to employ the word_umbrella function
#' @param stem - T/F to stem words
#'
#' @return - a wordcloud based on the dataframe and search term
#' 
#'
#' @examples
create_wordcloud <- function(data, filter_by = "", group = FALSE, stem = FALSE) {
  
  #filter data
  filtered <- data %>% 
    filter(
      str_detect(tolower(text), filter_by)) #selects only rows that cointain your term of interest
  
  # if group is TRUE then apply the word_umbrella function
  if (group) {grouped_terms <- word_umbrella(filtered)}
  else {
    grouped_terms <- filtered}
  
  #unnest tokens
  text_words <- grouped_terms %>% 
    select(text) %>% 
    mutate(text = tolower(text)) %>% 
    unnest_tokens(word, text)   #takes each rows' string and separates each word into a new row

  #stem words is stem = TRUE
  if (stem) {
    text_words <- text_words %>% 
      mutate(word = wordStem(word, language="english"))
  }
  
  #filter stop words and words whose weights are biased b/c of the querry
  filtered <- text_words %>% 
    anti_join(stop_words) %>% 
    count(word, sort=TRUE) %>% 
    filter(!word %in% c("https","rt","t.co","amp")) %>% #remove words associated with images/links and special characters, (i.e. amp = &)
    filter(!word %in% c("soil","health", "soil_health"))  #These terms consistently come out as top words perhaps as an atrifact of the initial querry, so i remove them here
  
  #create generic word cloud
  filtered %>% 
    with(wordcloud(word, n, 
                   min.freq = 1,
                   max.words=100, 
                   random.order=FALSE, 
                   color=brewer.pal(7,"Dark2")))
}

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
##                    create_bigram                            ##
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####


#' creates a list of bigrams and their counts 
#' 
#'
#'requires- tidyverse:: tidytext:: and stringr::
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
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>% # remove anything associated with a hyperlink
    unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
    mutate(next_word = lead(word)) %>% #creates a new column that has the next word for each respective row (this creates the bigrams)
    filter(!word %in% stop_words$word, # remove stop words
           !next_word %in% stop_words$word) 
  
  #stem
  if (stem) {
    bigrams_separated<- bigrams_separated %>% 
      mutate(word = wordStem(word, language="english"),
             next_word = wordStem(next_word, language="english"))
  } 
  
  #combine the two columns into a single bigram term, then count
  bigrams_united <- bigrams_separated %>% 
    unite(bigram, word, next_word, sep = ' ') %>%
    select(bigram)  %>% 
    count(bigram, sort = TRUE) 
  
}



####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
##                     gram_network                            ##
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####



#' returns a network graph that shows the directionality of how terms are arranged using arrows
#' and colors the arrows based on the relative occurance of those terms together (darker = higher count)
#' 
#' requires: tidyverse, igraph, ggraph
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
#' 
#' foo <- create_bigram(noRT)
#' gram_network(foo, 120)
#' 
gram_network <- function(data, limit) {
  set.seed(2019) # ensures consistency in output
  
  # defining the arrow asthetics
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))  
  
  #filter bigrams based on their counts
  bigrams <- data %>% 
    separate(bigram, c("word1", "word2"), sep = " ") %>% #separates bigrams into two columns
    filter(n > limit)
  
  #count each word so we can size the nodes based on their count
  counts <- bigrams %>% 
    gather(item, word, word1, word2) %>% 
    group_by(word) %>% 
    summarise(n = sum(n))
  
  #generates a an igraph graph (resembiling a table) showing direction of terms (see roxygen notes for link to where i got this code)
  bigrams %>% 
    igraph::graph_from_data_frame(vertices = counts) %>% # use word count to scale nodes
    ggraph::ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = T, 
                   arrow = a, end_cap = circle(.07, 'inches')) + #defines how the edges are visualized
    geom_node_point(color = "lightblue", aes(size = n)) + # aes for nodes, (says the color and to scale by word count)
    scale_size(range = c(1,10)) + # range of sizes for nodes
    geom_node_text(aes(label = name), repel = T) + # show word ot each node and repel (i.e avoid overlap/clutter)
    theme_void() 
}



###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
##                  flag india                             ##
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

#' flag tweets from india
#' 
#' This list was created iteratively based manually checking to see if the function accurately flagged tweets related to India -- this may need to be updated..
#'
#' @param data tweet data
#'
#' @return new column 'is_india' that has 1's and 0's indicating that a tweet is or is not from/related to India
#' 
#'
#' @examples
flag_india <- function(data) {
  
  results <- data %>% 
    mutate(
      is_india = case_when(
        str_detect(tolower(text), "[\u0900-\u097F]+|india|crore|health card|rupee|narendramodi|managed to feed 1.25 billion people|akshaykumar|sadhgurujv|rallyforrivers") ~ 1,
        str_detect(tolower(screen_name), "[\u0900-\u097F]+|india|crore|health card|rupee|narendramodi|sadhgurujv|rallyforrivers") ~ 1,
        str_detect(tolower(hashtags), "[\u0900-\u097F]+|india|crore|health card|rupee|narendramodi|sadhguru|rallyforrivers") ~ 1))
  
  #replace na w/ 0 to indicate non-india related tweets
  results$is_india[is.na(results$is_india)] <- 0
  
  return(results)
}

################################################
##            clean data                      ##
################################################

#function clean data to remove numbers, usernames, websites, non-ASCII characters and outlier
#' Clean data
#' i.e remove extranious numbers,characters, and users (like the pope)
#' 
#' NOTE: this also runs flag_india() and filters out all india related tweets
#'
#' @param input tiwtter data frame - either RT or noRT
#'
#' @return same data frame with the 'text' column cleaned
#' @export
#'
#' @examples
clean_data <- function(input){
  input_clean <- removeNumbers(input$text)
  input_clean <- gsub("@\\w+","",input_clean)
  input_clean <- gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", input_clean)
  input_clean <- gsub("#\\s+","", input_clean)
  input_clean <- gsub("amp", "", input_clean)
  input_clean <- gsub("[^\x01-\x7F]", "", input_clean)
  
  input$text <- input_clean
  input <- input %>% 
    filter(source != "Twittascope") 
  
  # to remove the pope
  input <- input %>%
    arrange(-retweet_count) %>%
    filter(screen_name != "Pontifex")
  
  # to remove all india related tweets
  input_india <- flag_india(input)
  input_no_india <- input_india %>% 
    filter(is_india == 0) 
  
  return(input_no_india)
}



########################################
####        Find RT                 ####
########################################


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
  names(result.df) <- c("date", "query", "number", "time_since", "content")
  result.df$id <- rank
  return(result.df)
}


