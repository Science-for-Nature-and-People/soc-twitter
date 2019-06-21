tweet <- data.frame(text = "the bird said Tweet tweeted no till")
prepare_text(tweet, group = TRUE, stem = TRUE)



#### wrangling data into format for sLDA


test <- na.omit(all_content)



test_groups <- data.frame(screen_name = c('ecogreenlawn', 'nspugh', 'UMNmanure'), 
                          is_science = c(NA, 1, NA), 
                          is_media = c(1,1,NA), 
                          is_politics = c(NA,NA,1))


# join content to manuallly coded groups
training_groups <- left_join(test_groups, test, by = 'screen_name')


#gather to accomodate repeats and remove NA's
group_gathered <- training_groups %>% 
  gather('group', 'type', -content, -screen_name) %>% 
  filter(!is.na(type)) %>% 
  select(-type) 

#reclassify groups into numbers
group_id <- group_gathered %>% 
  mutate(group = case_when(!str_detect(group, 'is_media') ~ -1,
                              str_detect(group, 'is_media') ~ 1)) 



### subset training data ###

## remove cases where a user is both 'media' and 'not media'
#~ first seperate into is and is not
is_media <- filter(group_id, group == 1)
not_media <- filter(group_id, group == -1)


# anti_join to remove cases of overlap
not_media <- anti_join(not_media, is_media, by = 'screen_name')


# now make sure lengths of each are the same
if(nrow(is_media) > nrow(not_media)){
  
  not_media_equal <- sample_n(not_media, nrow(is_media), replace = TRUE)
  training_data <- bind_rows(is_media, not_media_equal)
  
} else if(nrow(is_media) < nrow(not_media)){
  
  is_media_equal <- sample_n(not_media, nrow(is_media), replace = TRUE)
  training_data <- bind_rows(is_media_equal, not_media)
  
} else {
  
  training_data <- bind_rows(is_media, not_media)
  
  }
## add ID to each observation to filter by later
training_data$doc_ID <- seq(1, nrow(training_data), 1)

## create tokens for the training data
training_tokens <- training_data %>% 
  mutate(content = str_replace_all(content, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>% # remvove anything associated with hyperlinks
  mutate(content = tolower(content)) %>% 
  unnest_tokens(word, content) %>% 
  filter(!word %in% stop_words$word)

       

## create vocab list
#get unique words and ID them for reference
vocab <- data.frame(word = unique(training_tokens$word), 
                    word_ID = seq(1,length(unique(training_tokens$word)),1))


id <- left_join(training_tokens, vocab, by = 'word')


#get counts of each word for each user
id_count <- id %>% 
  group_by(doc_ID, word_ID) %>% 
  summarise(
    count = n()
  ) %>% 
  ungroup()



id_count$count <- as.integer(id_count$count)


######### create LDA parameters ###########


## create document list for use in LDA

doc_list <- vector('list', nrow(training_data))
for(i in 1:nrow(training_data)) {

  doc <- id_count %>% 
    filter(doc_ID == 1) %>% 
    select(-doc_ID) %>% 
    transpose()
  
  doc <- as.matrix(doc)
  
  doc <- unname(doc) 
       
  doc_list[[i]] <- doc
  
}

## create vocab vector for use in LDA 
vocab_vector <- as.character(vocab$word)

# set ratings
ratings <- as.numeric(training_data$group)

storage.mode(doc_list) <- "integer"


###### run ###########

num.topics <- 2

## Initialize the params
params <- sample(c(-1, 1), num.topics, replace=TRUE)

result <- slda.em(documents=doc_list,
                  K=num.topics,
                  vocab=vocab_vector,
                  num.e.iterations=10,
                  num.m.iterations=4,
                  alpha=1.0, eta=0.1,
                  ratings,
                  params,
                  variance=0.25,
                  lambda=1.0,
                  logistic=FALSE,
                  method="sLDA")

## Make a pretty picture.
require("ggplot2")
Topics <- apply(top.topic.words(result$topics, 5, by.score=TRUE),
                2, paste, collapse=" ")
coefs <- data.frame(coef(summary(result$model)))
theme_set(theme_bw())
coefs <- cbind(coefs, Topics=factor(Topics, Topics[order(coefs$Estimate)]))
coefs <- coefs[order(coefs$Estimate),]
qplot(Topics, Estimate, colour=Estimate, size=abs(t.value), data=coefs) +
  geom_errorbar(width=0.5, aes(ymin=Estimate-Std..Error,
                               ymax=Estimate+Std..Error)) + coord_flip()




