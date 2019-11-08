#### generate data for network analysis on content creators

library(rtweet)
library(tidygraph)
library(ggraph)
library(here)

### use `content_creator` variable from the top_users.Rmd scripts

## get user_id's from screen names
tmp <- left_join(content_creator, noRT_no_india, by = "screen_name")
users <- unique(tmp$user_id)




##### this was outside of aurora (on local R machine) to avoid issues associated w/ the rcurl package
# for (i in 1:length(users)){
#   if (i == 1){
#     friends <- get_friends(users[i])
#   } else {
#     tmp <- get_friends(users[i])
#     friends <- rbind(friends, tmp)
#   }
#   
#   
#   # pause if divisible by 15
#   if (i %% 15 == 0){
#     write_csv(friends, "user_friends.csv")
#     print(Sys.time())
#     Sys.sleep(15*61)
#   }
# }


