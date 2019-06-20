#### read in manually coded user group data ###

favs <- read_csv("manual_grouping_data/most_favorite_complete.csv")

follows <- read_csv("manual_grouping_data/most_followers_complete.csv")

querry <- read_csv("manual_grouping_data/most_querry_complete.csv")

status <- read_csv("manual_grouping_data/most_status_complete.csv")

full_df <- rbind(favs, follows, querry, status)

coded <- full_df %>% 
  select(-X1, -description, -NOTES)

### one row was miscoded:
# replace '`' with 1
coded$is_political[coded$screen_name == 'RepTimWalz'] <- 1
#return to being numeric
coded$is_political <- as.numeric(coded$is_political)


colSums(coded[,2:13], na.rm = T)
#   is_science           is_farmer           is_political            is_business 
#   30                     25                    101                     24 
#   is_media            is_training           is_nonprofit                is_food 
#   111                     43                     35                     10 
#   is_medical is_internationalagency        is_nature               is_other 
#   3                      6                     21                     39 



combine <- coded %>% 
  mutate(is_science =
    case_when(coded$is_medical == 1 ~ 1)) %>% 
  select(-is_medical)
