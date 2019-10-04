#### generate data for network analysis on content creators

library(rtweet)
library(tidygraph)
library(ggraph)

### use `content_creator` variable from the top_users.Rmd scripts

## get user_id's from screen names
tmp <- left_join(content_creator, noRT_no_india, by = "screen_name")
users <- unique(tmp$user_id)

friends <- list()

# start loop
for (i in 1:length(ids)){
  friends[[i]] <- get_friends(users[i])
  
  # pause if divisible by 15
  if (i %% 15 == 0){
    print(Sys.time())
    Sys.sleep(15*61) 
  }
}

# Combine data tables in list
friends <- bind_rows(friends) %>% 
  rename(friend = user_id)

friends <- bind_rows(friend_list) %>% 
  rename(friend = user_id)

write.csv(friends, "content_creator_friends.csv")


# net <- friends %>% 
#   group_by(friend) %>% 
#   mutate(count = n()) %>% 
#   ungroup() %>% 
#   filter(count > 1)
# 
# 
# g <- net %>% 
#   select(user, friend) %>%  # drop the count column
#   as_tbl_graph()
# 
# 
# 
# ggraph(g) +
#   geom_edge_link() +
#   geom_node_point(size = 3, colour = 'steelblue') +
#   theme_graph()