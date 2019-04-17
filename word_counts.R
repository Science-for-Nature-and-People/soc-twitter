library(tidyverse)
library(tidytext)
library(wordcloud)
library(stringr)

#this script is separated into 4 groups and creates wordclouds and column graphs of top words for each grouping and category.
#these groups are: 
#~full dataset
#~soil" "rangeland" "forest"
#~"soil health" "rangeland health" "forest health"
#~"soil quality" "rangeland quality" "forest quality"



## initial observations:
#when searching for *health the only common term among the top used word for each category was: biodiversity
#searching for *quality yielded no results from 'forest' only one result from 'rangeland'


twitter_merged_noRT <- read.csv("twitter_merged_noRT.csv", stringsAsFactors = FALSE)

##remove duplications
twitter_merged_noRT <- distinct(twitter_merged_noRT)




##Function for preparing df for a wordcloud | column graph of word counts
#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
prepare_text <- function(x) {
  text_words <- x %>% 
    select(text) %>% 
    mutate(text = tolower(text)) %>% 
    unnest_tokens(word, text)
  
  
  text_words %>% 
    anti_join(stop_words) %>% 
    count(word, sort=TRUE) %>% 
    filter(!word %in% c("https","rt","t.co","amp")) %>% #remove words associated with images/links and special characters, (i.e. amp = &)
    filter(!word %in% c("soil","health", "healthy", "soilhealth")) #These terms consistently come out as top words perhaps as an atrifact of the initial querry, so i remove them here
}





######### General Overview using all data ########

word_count <- prepare_text(twitter_merged_noRT)

#refactor order for the column graph
word_count$word <- factor(word_count$word, levels = word_count$word[order(word_count$n)])

ggplot(head(word_count,20), aes(word, n)) +
  geom_col() +
  coord_flip()
ggsave("word_prop_pngs/noRT_full.png")

word_count %>% 
  with(wordcloud(word, n, 
                 min.freq = 100,
                 max.words=200, 
                 random.order=FALSE, 
                 color=brewer.pal(7,"Dark2")))
## Farmers, produce, thrive, soul, fertility, life, fruit, joyful, agriculture, card
## explore user: @narendramodi (also a top hit)



####### filtering by: "soil" "rangeland" "forest" ########

####soil

#filter
soil_tweets <- twitter_merged_noRT %>% 
  filter(
    str_detect(tolower(text), "soil"))

#creat word count
soil_word_count <- prepare_text(soil_tweets)

soil_word_count %>% 
  with(wordcloud(word, n, 
                 min.freq = 100,
                 max.words=200, 
                 random.order=FALSE, 
                 color=brewer.pal(7,"Dark2")))


#### rangeland health 

rangeland_tweets <- twitter_merged_noRT %>% 
  filter(
    str_detect(tolower(text), "rangeland"))

#creat word count
rangeland_word_count <- prepare_text(rangeland_tweets) %>% 
  filter(word != "rangeland")


rangeland_word_count %>% 
  with(wordcloud(word, n, 
                 min.freq = 2,
                 max.words=200, 
                 random.order=FALSE, 
                 color=brewer.pal(7,"Dark2")))


#### forest health 

forest_tweets <- twitter_merged_noRT %>% 
  filter(
    str_detect(tolower(text), "forest"))

#creat word count
forest_word_count <- prepare_text(forest_tweets) %>% 
  filter(word != "forest")

forest_word_count %>% 
  with(wordcloud(word, n, 
                 min.freq = 10,
                 max.words=200, 
                 random.order=FALSE, 
                 color=brewer.pal(7,"Dark2")))

##### visualizing propotions 

#this is a very rough thought as a method for comparing proportion
forest_prop <- forest_word_count %>% 
  mutate(forest_prop = round(n/sum(n), 4)) %>% 
  select(-n)

soil_prop <- soil_word_count %>% 
  mutate(soil_prop = round(n/sum(n), 4)) %>% 
  select(-n)

range_prop <- rangeland_word_count %>% 
  mutate(range_prop = round(n/sum(n), 4)) %>% 
  select(-n)

total_prop <- merge(merge(forest_prop, soil_prop), range_prop)



#create word list:
top_soil <- head(soil_prop, 10)
top_range <- head(range_prop, 10) 
top_forest <- head(forest_prop, 10)

word_list <- c(paste(top_soil$word), paste(top_forest$word), paste(top_range$word))

#filter and restructure for visualization
selected_prop <- total_prop %>% 
  filter(word %in% word_list) %>% 
  gather("querry", "proportion", -word)
##this does not have length 30 as would be expected, some words have 0 overlap withing the different categories.

ggplot(selected_prop, aes(word, proportion)) +
  geom_col(aes(fill = word)) +
  facet_wrap(~querry) +
  coord_flip() +
  labs(title = "'rangeland' v 'soil' v 'forest'") +
  theme_bw()
ggsave("word_prop_pngs/word_proportions_all.png")



##repeating w/ forest excluded
total_prop_NOfor <- merge(soil_prop, range_prop)
word_list_NOfor <- c(paste(top_soil$word), paste(top_range$word))
selected_prop_NOfor <- total_prop_NOfor %>% 
  filter(word %in% word_list_NOfor) %>% 
  gather("querry", "proportion", -word)
##this does not have length 20 as would be expected, some words have 0 overlap withing the different categories.


ggplot(selected_prop_NOfor, aes(word, proportion)) +
  geom_col(aes(fill = word)) +
  facet_wrap(~querry) +
  coord_flip() +
  labs(title = "'rangeland' v 'soil'") +
  theme_bw()
ggsave("word_prop_pngs/range_soil.png")


###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


####### filtering by: "soil health" "rangeland health" "forest health" ########

####soil

#filter
soil_health_tweets <- twitter_merged_noRT %>% 
  filter(
    str_detect(tolower(text), "soil health"))

#creat word count
soil_health_wc <- prepare_text(soil_health_tweets)

soil_health_wc %>% 
  with(wordcloud(word, n, 
                 min.freq = 100,
                 max.words=200, 
                 random.order=FALSE, 
                 color=brewer.pal(7,"Dark2")))


#### rangeland health 

rangeland_health_tweets <- twitter_merged_noRT %>% 
  filter(
    str_detect(tolower(text), "rangeland health"))

#creat word count
rangeland_health_wc <- prepare_text(rangeland_health_tweets) %>% 
  filter(word != "rangeland")


rangeland_word_count %>% 
  with(wordcloud(word, n, 
                 min.freq = 2,
                 max.words=200, 
                 random.order=FALSE, 
                 color=brewer.pal(7,"Dark2")))


#### forest health 

forest_health_tweets <- twitter_merged_noRT %>% 
  filter(
    str_detect(tolower(text), "forest health"))

#creat word count
forest_health_wc <- prepare_text(forest_health_tweets) %>% 
  filter(word != "forest")

forest_health_wc %>% 
  with(wordcloud(word, n, 
                 min.freq = 10,
                 max.words=200, 
                 random.order=FALSE, 
                 color=brewer.pal(7,"Dark2")))

##### visualizing propotions 

#this is a very rough thought as a method for comparing proportion
forest_health_prop <- forest_health_wc %>% 
  mutate(forest_prop = round(n/sum(n), 4)) %>% 
  select(-n)

soil_health_prop <- soil_health_wc %>% 
  mutate(soil_prop = round(n/sum(n), 4)) %>% 
  select(-n)

range_health_prop <- rangeland_health_wc %>% 
  mutate(range_prop = round(n/sum(n), 4)) %>% 
  select(-n)

total_health_prop <- merge(merge(forest_health_prop, soil_health_prop), range_health_prop)



#create word list:
top_soil_health <- head(soil_health_prop, 10)
top_range_health <- head(range_health_prop, 10) 
top_forest_health <- head(forest_health_prop, 10)

word_list <- c(paste(top_soil_health$word), paste(top_forest_health$word), paste(top_range_health$word))

#filter and restructure for visualization
selected_health_prop <- total_health_prop %>% 
  filter(word %in% word_list) %>% 
  gather("querry", "proportion", -word)

ggplot(selected_health_prop, aes(word, proportion)) +
  geom_col(aes(fill = word)) +
  facet_wrap(~querry) +
  coord_flip() +
  theme_bw()
ggsave("word_prop_pngs/word_props_health.png")

###biodiversity is only common term
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###




####### filtering by: "soil quality" "rangeland quality" "forest quality"####
soil_quality_tweets <- twitter_merged_noRT %>% 
  filter(
    str_detect(tolower(text), "soil quality")) 

#creat word count
soil_quality_wc <- prepare_text(soil_quality_tweets) %>% 
  filter(word != "quality")

soil_quality_wc %>% 
  with(wordcloud(word, n, 
                 min.freq = 100,
                 max.words=200, 
                 random.order=FALSE, 
                 color=brewer.pal(7,"Dark2")))


#### rangeland quality 

rangeland_quality_tweets <- twitter_merged_noRT %>% 
  filter(
    str_detect(tolower(text), "rangeland quality"))
#only one hit


#creat word count
rangeland_quality_wc <- prepare_text(rangeland_quality_tweets) %>% 
  filter(word != "rangeland")


rangeland_word_count %>% 
  with(wordcloud(word, n, 
                 min.freq = 2,
                 max.words=200, 
                 random.order=FALSE, 
                 color=brewer.pal(7,"Dark2")))


#### forest quality 

forest_quality_tweets <- twitter_merged_noRT %>% 
  filter(
    str_detect(tolower(text), "forest quality"))
#no hits


##### visualizing propotions 

#this is a very rough thought as a method for comparing proportion

soil_quality_prop <- soil_quality_wc %>% 
  mutate(soil_prop = round(n/sum(n), 4)) %>% 
  select(-n)


#filter and restructure for visualization
selected_quality_prop <- soil_quality_prop %>% 
  gather("querry", "proportion", -word)

ggplot(head(selected_quality_prop,10), aes(word, proportion)) +
  geom_col(aes(fill = word)) +
  coord_flip() +
  theme_bw()
ggsave("word_prop_pngs/word_props_Soilquality.png")
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###