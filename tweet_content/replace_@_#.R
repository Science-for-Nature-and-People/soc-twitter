#### attempting to return @ and # to terms


text_words <- noRT[1278,] %>% 
  dplyr::select(text) %>% 
  mutate(text = tolower(text)) %>% #make all text lower case
  unnest_tokens(word, text, strip_punct =FALSE) 
text_words


if (str_detect(text_words$word[35], "@|#")) {
  print(lead(text$word))
}

if (str_detect(text_words$word, "@|#")) {
  x <- text_words$word[+1]
}

text_words$word[str_detect(text_words$word, "@|#")]


foo <- case_when(str_detect(text_words$word, "@|#") ~ text_words$word[str_detect(text_words$word, "@|#")+2])
foo


paste(text_words$word[str_detect(text_words$word, "@|#")], text_words$word[lag(str_detect(text_words$word, "@|#"))], sep="")




####working!!!!####
text_words <- noRT[1278,] %>% 
  dplyr::select(text) %>% 
  mutate(text = tolower(text)) %>% #make all text lower case
  unnest_tokens(word, text, strip_punct =FALSE) 


## select @ and # along with the following word that it should be associatd with
foo <- paste(text_words$word[str_detect(text_words$word, "@|#")], text_words$word[lag(str_detect(text_words$word, "@|#"))], sep="")

#make df to add to word list
bar <- as.data.frame(foo)
names(bar) <- "word"

combined <- bind_rows(text_words, bar)

#remove punctuation and stop words
cleaned <- combined %>% 
  anti_join(stop_words) %>% 
  count(word, sort=TRUE) %>% 
  filter(!word %in% c("https","rt","t.co","amp",'!','#','$','%','(',')','*',',','.',':',';','<','=','>','@','^','_','|','~','.','{','}',']','?','/','\\')) #removes all punctuation


nested <- combined %>% 
  nest(word)

test <- nested %>% 
  unnest_tokens(bigram, token = "ngrams", n = 2)




#### incorperate into bigram code
filtered <- x %>% 
  filter(
    str_detect(tolower(text), filter_by)) #select only rows that contain search term of interest

bigrams <- noRT[1:100,] %>% 
  select(text) %>% 
  mutate(text = tolower(text)) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2, ) %>% #creates sigle column of all possible bigrams from text strings
  count(bigram, sort = TRUE) 

bigrams_separated <- bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ") #separate the bigrams so that stop words can be filtered out

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word1 %in% c("https","rt","t.co","amp")) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word2 %in% c("https","rt","t.co","amp"))

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")















