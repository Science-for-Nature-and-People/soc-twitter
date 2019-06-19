#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#               defining user groups                   #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Questions to answer:

#What tweets are most liked/retweeted by farmers? by non-profit staff? by academics?
##~~ approach: 
#~~~~ Id the users who retweeted the most RTed tweets
#~~~~ perform user search and try to separate them into "groups"
  


library(tidyverse)
library(stringr)
library(rtweet)
library(wordcloud)
library(tidytext)

source("../text_analysis_functions.R")

twitter_merged <- read.csv("twitter_merged.csv", stringsAsFactors = FALSE) %>% 
  distinct()
twitter_merged_noRT <- read.csv("twitter_merged_noRT.csv", stringsAsFactors = FALSE) %>% 
  distinct() %>% 
  arrange(-retweet_count)




##### creating a column to flag tweets from or about India ####
RT <- twitter_merged %>% 
  mutate(
    is_india = case_when(
      str_detect(tolower(text), "[\u0900-\u097F]+|india|crore|health card|rupee|narendramodi|managed to feed 1.25 billion people|akshaykumar") ~ 1,
      str_detect(tolower(screen_name), "[\u0900-\u097F]+|india|crore|health card|rupee|narendramodi") ~ 1,
      str_detect(tolower(hashtags), "[\u0900-\u097F]+|india|crore|health card|rupee|narendramodi") ~ 1))

#replace na w/ 0
RT$is_india[is.na(RT$is_india)] <- 0


RT <- RT %>% 
   filter(!str_detect(tolower(text), "pontifex"), #remove the popes RTs
          screen_name != "Pontifex") 




########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#######
#####      STEP 1: identify user groups       ####
########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#######

#ID 'active users' i.e those who have tweeted >32 times this equates to ~every other week over 16 months
active_users <- RT %>% 
  filter(is_india == 0) %>% 
  group_by(screen_name) %>% 
  summarise(
    total_tweets = n(),
    total_retweeted = sum(retweet_count, na.rm=T)
  ) %>% 
  filter(total_tweets >= 16)

#get tweet info on active users
user_list <- RT %>% 
  filter(screen_name %in% active_users$screen_name)

# see which users among TNC's partners are among the active user list
partner_overlap <- user_list %>% 
  filter(screen_name %in% partners$Handle | screen_name %in% partners$associatedHandles)
unique(partner_overlap$screen_name)
# [1] "NACDconserve"   "USDA_NRCS"      "soil_institute" "MidwestRowCrop"
# [5] "GrowingReturns" "AGreeAgPolicy"  "nobleresinst"   "USDA"          
# [9] "SoilPartners"   "FoundationFAR"  "ASA_CSSA_SSSA" 



#look up user info from twitter API
user_info <- lookup_users(unique(user_list$user_id))

########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#######
##### create word list for IDing user groups #### 
########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#######

#use partner list and associated user group `$Type` to create part of word list
partner_type <- partners %>% 
  select(Handle, associatedHandles, Type) %>% 
  separate(., associatedHandles, c("a", "b", "c", "d"), sep = "[;]") %>% 
  gather(x, handle, -Type) %>% 
  select(handle, Type) %>% 
  na.omit()
 


#function for getting words from each parterns description based on thei group type
usr_group_words <- function(x) {
  type <- partner_type %>% 
    filter(Type == x) # filter based on group categories of parnters
  
  usr <- lookup_users(type$handle) #use API to get user info
  
  # in order to use function `prepare_text` there must be a column called 'text'
  usr_desc <- usr %>% 
  select(description)
  names(usr_desc) <- "text" 
  
  words <- prepare_text(usr_desc) # see source(text_analysis_functions.R) for details
  
  return(words)
}

## get word list for each group type
research <- usr_group_words("Research")
media <- usr_group_words("Media")
lab <- usr_group_words("Laboratory")
govt <- usr_group_words("Government")
non_prof <- usr_group_words("Non-profit")
foundation <- usr_group_words("Foundation")
corperate <- usr_group_words("Corporate")


#view in df
search_terms <- data.frame(research[1:20,1], media[1:20,1], lab[1:20,1], govt[1:20,1], non_prof[1:20,1], foundation[1:20,1], corperate[1:20,1])
names(search_terms) <- c("rs", "media", "lab", "govt", "nonProfit", "foundation", "corporate")




##view descriptions for top active users (also get text from last tweet to help in IDing)
usr_desc <- user_info %>% 
  select(screen_name,description,text)


# explore: make wordcloud of descriptions
#rename descprtion column to text (this is required of the create_wordcloud function - may change this later)
cloud <- user_info %>% 
  select(description)
names(cloud) <- "text"
create_wordcloud(cloud)



#### build word list
science_WL <- c('ologist', 'science', 'scientist', 'university', 'data', 'studies', 'research', 'agronomy', 'institute')
farmer_WL <- c("farm", "rancher", "ranching", "cattle", "sheep")
govt_WL <- c("endorse", "nrcs", "usda", "gov", "public") 
business_WL <- c("company" , "business" ,"customer", "entrepreneur")
media_WL <- c("edit", "journalist")
environmental_WL <- c("conserv", "sustain", "water", "organic", "climate", "environment")






