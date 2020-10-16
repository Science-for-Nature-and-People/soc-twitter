#### calculation proportion of likes by each group for tweets ####


#' This function takes a given tweet, Identifies the users who retweeted it, 
#' queries the API for details on those users and uses their 'descriptions' to place each user into a 'group' 
#' (government, environmentalist, media, farmer, scientist, business)  
#' then returns a dataframe that shows the proportion of users from each group retweeted that tweet 
#' --- this is meant to be a proxy for what tweets appeal most to which user groups
#' 
#' ~~This function was created to be used along with lapply so that numerous tweets could be analyzed at once~~
#'
#' 
#' this function relies on :
#' ~~~having both RT and noRT dataframes in your environment, 
#' ~~~the noRT has the `is_india` flag included
#' 
#'
#' @param x selects the row that will be used for analysis - it was set up this way so that it could be easily used in conjunction with `lapply()` i.e x <- 1:100 will return the top 100 tweets arranged by retweet_count
#' @param y input either 0 or 1 to either exclude or include india respectively 
#' @param category enter search term to filter noRT by, so that we can perform this easily for diferent categories. default set to no filter
#'
#'
#'
#' @return data frame with columns: 
#' 'group' (see 'user_group.R' for how these were created)
#' 'prop_like' - which is the propotion that each respective group RTed that given tweet
#' tweet_og - the original tweet that was used for analysis
#' handle_og - the original handle that was used for analysis
#' n - gives number of users that retweeted the `tweet_og` 
#' 
#'
#' @examples
#' x <- 1:20
#' prop_list <- lapply(x, find_group_prop) #if this throws an HTTP error, simply re-run this line untill it works, sometime the API doesn't play nice
#' prop_df <- bind_rows(prop_list, .id = "ID")
#' 
#'
find_group_prop <- function(x, category = "", y = 1) {
  
  #### Word list for classifying users (built within user_groups.R) 
  science_WL <- c('ologist', 'science', 'scientist', 'university', 'data', 'studies', 'research', 'agronomy', 'institute')
  farmer_WL <- c("farm", "rancher", "ranching", "cattle", "sheep")
  govt_WL <- c("endorse", "nrcs", "usda", "gov", "public") 
  business_WL <- c("company" , "business" ,"customer", "entrepreneur")
  media_WL <- c("edit", "journalist")
  environmental_WL <- c("conserv", "sustain", "water", "organic", "climate", "environment")
  
  
  
  #filter for is_india & category - as defined by the function arguments
  noRT_filtered <- noRT %>% 
    filter(is_india == y |
             is_india == 0) %>% 
    filter(
      str_detect(tolower(text), category)) %>% 
    arrange(-retweet_count)
  
  #for the given tweet within the specified row (which is variable 'x' in the function), this will find each instance of a retweet - thereby giving us the users who retweeted it
  RT_users <- RT %>% 
    filter(
      str_detect(text,
                 fixed(
                   str_c(                                         
                     word(noRT_filtered[x,]$text, 1:6), collapse = ' '))))  #str_c() combines each word that has been individually selected by word() into a single string. This creates a six word string using the first six words of each tweet. this should be enough to uniquely ID instances of a RTs using str_detect
  
  
  #get user info via twitter API
  user_info <- lookup_users(unique(RT_users$user_id))
  
  #### categorize users into groups
  #detects any words from relevent word list (hence paste(x,collapse="|"))
  user_group <- user_info %>%
    select(screen_name, description) %>%
    mutate(
      is_scientist = case_when(str_detect(tolower(description), paste(science_WL, collapse = "|" )) ~ 1),
      is_farmer = case_when(str_detect(tolower(description), paste(farmer_WL, collapse = "|" )) ~ 1),
      is_govt = case_when(str_detect(tolower(description), paste(govt_WL, collapse = "|" )) ~ 1),
      is_business = case_when(str_detect(tolower(description), paste(business_WL, collapse = "|" )) ~ 1),
      is_media = case_when(str_detect(tolower(description), paste(media_WL, collapse = "|" )) ~ 1),
      is_envnmtal = case_when(str_detect(tolower(description), paste(environmental_WL, collapse = "|" )) ~ 1))
  
  #set NA's to 0 to allow summing
  user_group[is.na(user_group)] <- 0
  
  #create df showing proportion of each group that RTed the original tweet
  prop_likes <- user_group %>%
    mutate(                                  
      scientist = sum(is_scientist)/nrow(.),
      farmer = sum(is_farmer)/nrow(.),
      govt = sum(is_govt)/nrow(.),
      business = sum(is_business)/nrow(.),
      media = sum(is_media)/nrow(.),
      envnmtal = sum(is_envnmtal)/nrow(.)) %>%
    select(9:14) %>% #select only the newly mutated rows showing proportions
    head(1) %>% #the sums were put into each row -- only need one
    gather(group, prop_like) %>% # gather into tidy format
    mutate(
      tweet_og = noRT_filtered[x,]$text, #original tweet
      handle_og = noRT_filtered[x,]$screen_name, #Screen name of original tweet
      n = nrow(.)) #gives number of users that retweeted the `tweet_og` 
  
  
  return(prop_likes)
}