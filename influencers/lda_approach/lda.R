######################
## supervised LDA   ##
######################
library(lda)
demo(slda)

data(poliblog.documents)
data(poliblog.vocab)
data(poliblog.ratings)









#####~~~~~~~~~~~~~~~~~~~~~####
#  Tidytext mining approach


library(topicmodels)
library(tidytext)
library(rtweet)



# find all the users who RTed a given tweet
RT_users <- twitter_merged %>% 
  filter(
    str_detect(text,
               fixed(
                 str_c(                                         
                   word(noRT[22,]$text, 1:6), collapse = ' '))))  #str_c() combines each word that has been individually selected by word() into a single string. This creates a six word string using the first six words of each tweet. this should be enough to uniquely ID instances of a RTs using str_detect

# get all of their timelines and descriptions
timelines <- get_timelines(RT_users$screen_name, n = 100)


## one apporach is to combine the text and descriptions together, thus purposfully overweighting each users description
user_text_full <- timelines %>% 
  select(screen_name,text, description) %>% 
  mutate(
    full_text = paste(description, text, sep = " ") 
  )





text_words <- user_text_full %>% 
  select(screen_name, full_text) %>% 
  mutate(full_text = tolower(full_text)) %>% #make all lower case
  mutate(full_text = str_replace_all(full_text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, full_text) %>% # unnest words based on the regex defined above
  filter(!word %in% stop_words$word) 

word_count <- text_words %>% 
  group_by(word) %>% 
  mutate(wc = n()) %>% 
  ungroup() %>% 
  filter(wc > 50)

dtm <- word_count %>% 
  count(screen_name, word) %>% 
  cast_dtm(screen_name, word, n)
  
lda <- LDA(dtm, k = 5)

 
viz <- lda %>%
   tidy() %>%
   group_by(topic) %>%
   top_n(20, beta) %>%
   ungroup() %>%
   mutate(term = reorder(term, beta)) %>%
   ggplot(aes(term, beta, fill = factor(topic))) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~ topic, scales = "free_y") +
   coord_flip()
viz

 lda %>%
  tidy(matrix = "gamma") %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot()






