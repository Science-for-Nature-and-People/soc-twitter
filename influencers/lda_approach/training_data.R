#############################################
#### wrangling data into format for sLDA ####
#############################################

# This code creates training data for the sLDA and has 3 parts:
#~ 1. combines our API scraped user content with our manually IDed groupings
#~ 2. format the data to plug into the sLDA model, there are 3 requirements:
#~    a. document list that, for each document, has tokenized words and their counts
#~    b. vocab list tat is referenced within the list of documents ^
#~    c. ratings for each documents
#~ 3. The last step shows 2 examples of running the model and predicting (one for is_media, one for is_political)
#~     - at a first glance, it appears that this sLDA model will not work to predict users 
#~     - may want to try tweaking some of the model parameters before disregarding this approach though
#~     - at the very least, this code is a solid foundating for formating data to suit sLDA


## Note: see demo(sLDA) for demo of how the LDA package creator uses the sLDA functions



library(tidyverse)
library(tidytext)
library(lda)
source("format_LDA_functions.R")  #functions for creating/formating all the requisit data to be input into the sLDA model



################# Step 1: combine data ################

## get users + their content that was created in the generate_training_content.R script
user_content <- read_csv('training_content.csv') %>% 
  select(-X1)


## get data table of manually grouped users
test_groups <- read_csv("manual_grouping_data/known_groups.csv") %>% 
   select(-X1)


# join content to manuallly coded groups to get content for know groups so we can begin training
training_groups <- left_join(test_groups, user_content, by = 'screen_name')  %>% 
  filter(!is.na(content))


#gather to accomodate users in multiple groups and to remove NA's
group_gathered <- training_groups %>% 
  gather('group', 'is_group', -content, -screen_name) %>% 
  filter(!is.na(is_group)) %>% 
  select(-is_group) 

##withhold 20% of known 'is_media' to test against later
group_witheld <- group_gathered %>% 
  filter(group == 'is_media') %>% 
  sample_n(9)

# remove these ^ from rest of group
test_media <- anti_join(group_gathered, group_witheld)

################# Step 2: format data ################


## identify groups (in this example is_media vs isNOT_media) 
## this function also:
## -ranks users as either is_group or not_group
## -ID's each 'document' (or collection of past 100 tweets + description for each user) which will then be used to create the document list below
media_training_data <- LDA_training_data(test_media, 'is_media')

## create tokens for user content
media_training_tokens <- create_training_tokens(media_training_data)
  
## create vocab list of all unique words 
## the order of these will be used as a reference key in each document matrix created in the next step
vocab_vector <- create_vocab_list(media_training_tokens)


## create document list
## the structure of this is a list of matrixes, where each matrix has two rows:
##  - row1 = numbers that correspond to words from the 'vocab_vector' 
##  - row2 = count of each of those words
doc_list <- create_document_list(media_training_tokens)


# set ratings
# this is another vector that in categorizes each document as is/is_not group
# the order is in reference to the document list ^ 
ratings <- as.numeric(media_training_data$group)






################# Step 3: run model ################

# set K number of topics
# play with these to see how it may affect the outcome
num.topics <- 2

## Initialize the parameters (not really sure what this does -- copied from: demo(sLDA))
params <- sample(c(-1, 1), num.topics, replace=TRUE)

## run model
## havn't tried tweaking the iterations, alpha, lamda, etc.... yet
result <- slda.em(documents=doc_list,
                  K= num.topics,
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


## test against withheld known media users
# format all documents so that we can run the model on it to predict
group_witheld$doc_ID <- seq(1, nrow(group_witheld), 1)
witheld_tokens <- create_training_tokens(group_witheld)
witheld_docs <- create_document_list(witheld_tokens)

predictions <- slda.predict(witheld_docs,
                                 result$topics, 
                                  result$model,
                                  alpha = 1.0,
                                  eta=0.1)

# not really sure how to interprete these results...



### test against known farmers to see if there is a difference in prediction values between groups
group_farmer <- group_gathered %>% 
  filter(group == 'is_farmer')

group_farmer$doc_ID <- seq(1, nrow(group_farmer), 1)
farmer_tokens <- create_training_tokens(group_farmer)
farmer_docs <- create_document_list(farmer_tokens)

predictions_farmer <- slda.predict(farmer_docs,
                            result$topics, 
                            result$model,
                            alpha = 1.0,
                            eta=0.1)
# Very little difference in range of values....

######## conclusion: FAILED ##########






####### Try again with is_political  #########

group_witheld <- group_gathered %>% 
  filter(group == 'is_political') %>% 
  sample_n(4)

# remove these ^ from rest of group
test_political <- anti_join(group_gathered, group_witheld)



political_training_data <- LDA_training_data(test_political, 'is_political')

## create tokens for ALL user content
political_training_tokens <- create_training_tokens(political_training_data)

## create vocab list of all unique words
vocab_vector <- create_vocab_list(political_training_tokens)


## create document 
doc_list <- create_document_list(political_training_tokens)


# set ratings
ratings <- as.numeric(political_training_data$group)






################# Step 3: run model ################

# set K number of topics
# 
num.topics <- 2

## Initialize the parameters (not really sure what this does -- copied from: demo(sLDA))
params <- sample(c(-1, 1), num.topics, replace=TRUE)

## run model
## havn't tried tweaking the iterations, alpha, lamda, etc.... yet
result_political <- slda.em(documents=doc_list,
                  K=num.topics,
                  vocab=vocab_vector,
                  num.e.iterations=100,
                  num.m.iterations=40,
                  alpha=1.0, eta=.1,
                  ratings/100,
                  params,
                  variance=0.25,
                  lambda=1.0,
                  logistic=TRUE,
                  method="sLDA")





group_witheld$doc_ID <- seq(1, nrow(group_witheld), 1)
all_tokens <- create_training_tokens(group_witheld)
politic_docs <- create_document_list(all_tokens)

predictions_political <- slda.predict(politic_docs,
                            result_political$topics, 
                            result_political$model,
                            alpha = 1.0,
                            eta=0.1)

#now test against farmers


group_farmer <- group_gathered %>% 
  filter(group == 'is_farmer')

group_farmer$doc_ID <- seq(1, nrow(group_farmer), 1)
farmer_tokens <- create_training_tokens(group_farmer)
farmer_docs <- create_document_list(farmer_tokens)

predictions_farmer <- slda.predict(farmer_docs,
                                   result$topics, 
                                   result$model,
                                   alpha = 1.0,
                                   eta=0.1)










