#############################################
#### wrangling data into format for sLDA ####
#############################################

# This code creates training data for the sLDA and has 3 parts:
#~ 1. combines our API scraped user content with our manually IDed groupings
#~ 2. format the data to plug into the sLDA model, there are 3 requirements:
#~    a. document list that, for each document, has tokenized words and their counts
#~    b. vocab list tat is referenced within the list of documents ^
#~    c. ratings for each documents
#~ 3. The last step shows an example of running the model and predicting



library(tidyverse)
library(tidytext)
library(lda)
source("format_LDA_functions.R")  #functions for creating/formating all the requisit data to be input into the sLDA model



################# Step 1: combine data ################

## get users + their content that was created in the generate_content.R script
user_content <- read_csv('training_content.csv') %>% 
  select(-X1) %>% 
  na.omit()



test_groups <- read_csv("manual_grouping_data/known_groups.csv") %>% 
   select(-X1)


# join content to manuallly coded groups to get content for know groups so we can begin training
training_groups <- left_join(test_groups, user_content, by = 'screen_name')


#gather to accomodate users in multiple groups and to remove NA's
group_gathered <- training_groups %>% 
  gather('group', 'type', -content, -screen_name) %>% 
  filter(!is.na(type)) %>% 
  select(-type) 



################# Step 2: format data ################


## identify groups (in this example is_media vs isNOT_media) and bootstrap groups to be of equal length
## this function also creates 
media_training_data <- LDA_training_data(group_gathered, 'is_media')

## create tokens for ALL user content
media_training_tokens <- create_training_tokens(media_training_data)
  
## create vocab list of all unique words
vocab_vector <- create_vocab_list(media_training_tokens)


## create document 
doc_list <- create_document_list(media_training_tokens)


# set ratings
ratings <- as.numeric(media_training_data$group)






################# Step 3: run model ################

# set K number of topics
# 
num.topics <- 2

## Initialize the parameters (not really sure what this does -- copied from: demo(sLDA))
params <- sample(c(-1, 1), num.topics, replace=TRUE)

## run model
## havn't tried tweaking the iterations, alpha, lamda, etc.... yet
result <- slda.em(documents=doc_list,
                  K=num.topics,
                  vocab=vocab_vector,
                  num.e.iterations=10,
                  num.m.iterations=4,
                  alpha=1.0, eta=0.1,
                  ratings/100,
                  params,
                  variance=0.25,
                  lambda=1.0,
                  logistic=FALSE,
                  method="sLDA")


## Make a pretty picture (taken straight from demo(sLDA))
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





### predict ####

# format all documents so that we can run the model on it to predict
group_gathered$doc_ID <- seq(1, nrow(group_gathered), 1)
all_tokens <- create_training_tokens(group_gathered)
all_docs <- create_document_list(all_tokens)

predictions <- slda.predict(all_docs,
                                 result$topics, 
                                  result$model,
                                  alpha = 1.0,
                                  eta=0.1)


#plot
qplot(predictions,
             fill=factor(ratings),
             xlab = "predicted rating",
             ylab = "density",
             alpha=I(0.5),
             geom="density") +
               geom_vline(aes(xintercept=0)) +
               theme(legend.position = "none")











