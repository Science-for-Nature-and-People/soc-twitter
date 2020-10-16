### create df of all mannually coded user groups

fav <- read.csv("most_favorite_complete.csv")
fav$is_political <- as.numeric(fav$is_political)
follow <- read.csv('most_followers_complete.csv')
query <- read.csv('most_querry_complete.csv')
status <- read.csv('most_status_complete.csv')

known_groups <- bind_rows(fav, follow, query, status) %>% 
  select(-X, -description, -NOTES)

write.csv(known_groups, "known_groups.csv")
