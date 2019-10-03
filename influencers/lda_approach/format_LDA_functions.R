### the functions in this script are designed to be part of a single workflow that preps data for use in the sLDA model


#################################~~~~~~~~~~~~~~~~~~~#######################################
################################# LDA_Training_data #######################################
#################################~~~~~~~~~~~~~~~~~~~#######################################



#' This function is designed to to do 3 things:
#' 1. categorize users based on the group of interest (100 = is_group, -100 = not_group)
#' 2. bootstrap content so that the is_group and isNOT_group categories are equal length
#' 
#'
#' @param data tidy format data that has users classified into groups based on their screen name and content ( see training_data.R for workflow on how to create this format)
#' @param group_name group of interest (i.e: 'is_media', 'is_science', 'is_farmer' etc)
#'
#' @return bootstrapped dataframe with each 'document' (i.e content for each user) classified as is or inNOT your group of interest
#' 
#'
#' @examples
#' 
#' see lines 25:50 of training_data.R
LDA_training_data <- function(data, group_name){
  
  
  #reclassify groups into ranks
  group_id <- data %>% 
    mutate(group = case_when(!str_detect(group, paste(group_name)) ~ -100,
                             str_detect(group, paste(group_name)) ~ 100)) 
  
  
  
  ### subset training data ###
  
  ## remove cases where a user is both 'group' and 'not group'
  #~ first seperate into is and is not
  is_group <- filter(group_id, group == 100)
  not_group <- filter(group_id, group == -100)
  
  
  # anti_join to remove cases of overlap - we do this so we dont use the same user content to train both the is and is not groups
  not_group <- anti_join(not_group, is_group, by = 'screen_name')
  
  
  
# 
#   # now make sure lengths of each are the same
#   # if they are not, randomly sample users from the smaller group and duplicate their content
#   if(nrow(is_group) > nrow(not_group)){
# 
#     #calculate the different in length between the data
#     difference <- nrow(is_group) - nrow(not_group)
#     # sample from not_group to generate extra data
#     not_group_equal <- sample_n(not_group, difference, replace = TRUE)
#     # recombine
#     training_data <- bind_rows(is_group, not_group, not_group_equal)
# 
#   } else if(nrow(is_group) < nrow(not_group)){
# 
#     difference <- nrow(not_group) - nrow(is_group)
# 
#     is_group_equal <- sample_n(not_group, difference, replace = TRUE)
#     training_data <- bind_rows(is_group_equal, is_group, not_group)
# 
#   } else {
# 
#     training_data <- bind_rows(is_group, not_group)
# 
#   }
  
  training_data <- bind_rows(is_group, not_group)
  
  ## add ID to each observation to filter by later - this will essentially become the document ID
  training_data$doc_ID <- seq(1, nrow(training_data), 1)
  
  return(training_data)

}



#################################~~~~~~~~~~~~~~~~~~~~~~~~~#######################################
################################# create_training_tokens #######################################
#################################~~~~~~~~~~~~~~~~~~~~~~~~#######################################

#' tokenizes words from user content - when generating training data, use the results of LDA_training_data() otherwise use the same inputs that you would use for LDA_training_data() 
#' if not using the output of lda_training_data , must also add the following code before using this function:
#' 
#' group_gathered$doc_ID <- seq(1, nrow(group_gathered), 1) so that the results of this function can be later used in create_document_list()
#'
#' @param data tidy format data that has users classified into groups based on their screen name and content ( see training_data.R for workflow on how to create this format)
#'
#' @return tokenized 
#' 
#'
#' @examples
#' for use with training data see training_data.R lines 24:60
#' 
#' for use with all content / for formating data for predictions see line 122
create_training_tokens <- function(training_data){
  ## create tokens for the training data
  training_tokens <- training_data %>% 
    mutate(content = str_replace_all(content, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>% # remvove anything associated with hyperlinks
    mutate(content = tolower(content)) %>% 
    unnest_tokens(word, content) %>% 
    filter(!word %in% stop_words$word)
  
  return(training_tokens)
  
}    

#################################~~~~~~~~~~~~~~~~~~#######################################

#' create vocab list for sLDA
#'
#' @param training_tokens output of the create_training_tokens() function
#'
#' @return a vector of words
#' @export
#'
#' @examples
create_vocab_list <- function(training_tokens) {

  # give each word a unique ID
  vocab <- data.frame(word = unique(training_tokens$word), 
                    word_ID = seq(1,length(unique(training_tokens$word)),1))
  
  vocab_vector <- as.character(vocab$word)
  
  return(vocab_vector)

}


#' creates a list of matrix's that represent the words/ word counts for each document 
#' these words are referenced by the vocab vector in the above functions (hence why the code is copied in here)
#'
#' @param training_tokens output of the create_training_tokens() function
#'
#' @return a list that has the words/ word counts for each document 
#' 
#'
#' @examples
create_document_list <- function(training_tokens) {
  
  vocab <- data.frame(word = unique(training_tokens$word), 
                      word_ID = seq(1,length(unique(training_tokens$word)),1))

id <- left_join(training_tokens, vocab, by = 'word')


#get counts of each word within each document
id_count <- id %>% 
  group_by(doc_ID, word_ID) %>% 
  summarise(
    count = n()
  ) %>% 
  ungroup()


## create document list for use in LDA

doc_list <- list()
for(i in 1:length(unique(id_count$doc_ID))) {
  
  doc <- id_count %>% 
    filter(doc_ID == i) %>% 
    select(-doc_ID) 
  
  doc_matrix <- as.matrix(t(doc))  
  
  doc_matrix <- unname(doc_matrix)
  
  storage.mode(doc_matrix) <- 'integer'
  
  doc_list[[i]] <- doc_matrix
  
}

return(doc_list)

}












