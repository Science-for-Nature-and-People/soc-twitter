##### This code creates RT networks for two conferences:
# 1) soilsummit 2019
# 2) RFSI 


library(tidyverse)
library(RColorBrewer)
library(tidytext)
library(stringr)
library(ggraph)
library(igraph)
library(tm)
library(NLP)
library(quanteda)
library(lubridate)
library(dplyr)
library(ggplot2)
source("text_analysis_functions.R")



# load data
noRT_no_india <- read.csv("/home/shares/soilcarbon/Twitter/cleaned_data/noRT_clean.csv", stringsAsFactors = FALSE)
RT_no_india <-  read.csv("/home/shares/soilcarbon/Twitter/cleaned_data/RT_clean.csv",stringsAsFactors = FALSE)



##############################
######## SoilSummit19 ########
##############################

# select tweets with hashtag
# #SoilSummit19

input <- noRT_no_india

# SoilSummit19 is used by three conferences (Soil Health Summit 19, TCM Soil Summit, Red River Soil Health Summit)
#!!!!! could not reproduce this using "soilsummit19" => removed the 19
SHS19 <- input %>% 
  dplyr::filter(str_detect(tolower(text), "soilsummit"))
# to remove possible tweets from two other conferences also using this hashtag
SHS19 <- SHS19 %>% filter(date(SHS19$created_at) < "2019-01-25")

SHS19$conference <- "SHS19"
# select the noRT with retweet count > 1
SHS19_n <- SHS19 %>% filter(retweet_count > 1)


# select the RT of SHS19
SHS19_RT <- RT_no_india %>% filter(str_detect(tolower(text), "soilsummit"))
SHS19_RT <- SHS19_RT %>% filter (is_retweet == TRUE)
SHS19_RT <- SHS19_RT  %>% filter (retweet_count > 1)

# to remove possible RT from two other conferences also using this hashtag
SHS19_RT <- SHS19_RT %>% filter(date(SHS19_RT$created_at) < "2019-02-11")


### function for IDing RTs
library(plyr)

find_rt <- function(rank, noRT_dataset, RT_dataset) {
  result_rt <- RT_dataset %>% 
    filter(substring(RT_dataset$text, 1, 30) == substring(noRT_dataset$text[rank], 1, 30))
  # aggregate tweets by date
  result.df <- ddply(result_rt, .(date(result_rt$created_at), result_rt$query), nrow)
  names(result.df) <- c("Date", "Query", "Number")
  result.df <- result.df %>% arrange (result.df$Date)
  # calculate day_since based on the original tweet (noRT) date
  result.df$time_since <- result.df$Date - date(noRT_dataset$created_at[rank])
  result.df$content <- substring(result_rt$text[1], 1, 30)
  names(result.df) <- c("Date", "Query", "Number", "Time_since", "Content")
  return(result.df)
}


# select input
# limiting the tweets for retweet_count > 5
SHS19_RT <- SHS19_RT %>% filter(SHS19_RT$retweet_count > 5)
tt <- data.frame(SHS19_RT$text, SHS19_RT$screen_name)
names(tt) <- c("text", "retweet_user")
tt$author <- SHS19$screen_name[match(tt$text, SHS19$text)]

# filter out the ones w/o authors
network <- tt %>% filter(tt$author != "NA")

#### visualiting RT network
# generate node list
authors <- network %>% distinct(author) 
names(authors) <- "label"
retweet_users <- network %>% distinct(retweet_user) 
names(retweet_users) <- "label"

nodes <- rbind(authors, retweet_users)
nodes <- nodes %>% distinct(label) #to get rid of duplicates in both authors and retweet user
nodes <- nodes %>% rowid_to_column("id")

# generate edge list
per_route <- ddply(network, .(network$author, network$retweet_user), nrow)
names(per_route) <- c("authors", "retweet_users", "weight")

edges <- per_route %>% left_join(nodes, by = c("authors" = "label"))
colnames(edges)[4] <- "from"
edges <- edges %>% 
  left_join(nodes, by = c("retweet_users" = "label"))
colnames(edges)[5] <- "to"

edges <- dplyr::select(edges, from, to, weight)

graph <- graph.data.frame(edges, directed = T)
edges$value <- edges$weight
degree_value <- degree(graph, mode = "in")
nodes$value <- degree_value[match(nodes$id, names(degree_value))]

# ggraph packages to show network diagram
library(tidygraph)
library(ggraph)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)


ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point(aes(size = value)) +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.1, 0.5)) +
  geom_node_text(aes(label = label), label.size = 0.25, repel = TRUE) +
  labs(edge_width = "Number") +
  theme_graph()



############################
###### RFSI RT network #####
############################
# select tweets with hashtag
# 

input <- noRT_no_india


RFSI <- input %>% 
  dplyr::filter(str_detect(tolower(text), "rfsi|rfsi19|invest_regenag|regenerative food systems investment") & 
                  !str_detect(text, "#rye|SRFSI")) 


RFSI$conference <- "RFSI"
# select the noRT with retweet count > 1
RFSI_n <- RFSI %>% filter(retweet_count > 1)


# select the RT of RFSI
RFSI_RT <- RT_no_india %>% filter(str_detect(tolower(text),"#rfsi|rfsi|rfsi19|invest_regenag|regenerative food systems investment"))
RFSI_RT <- RFSI_RT %>% filter (is_retweet == TRUE)
RFSI_RT <- RFSI_RT  %>% filter (retweet_count > 1)

# remove 3 tweets unrelated to conference
RFSI_RT <- RFSI_RT %>% filter(!str_detect(text, "#rye|SRFSI"))

# select input
# limiting the tweets for retweet_count > 1
RFSI_RT <- RFSI_RT %>% filter(RFSI_RT$retweet_count > 1)
tt <- data.frame(RFSI_RT$text, RFSI_RT$screen_name)
names(tt) <- c("text", "retweet_user")
tt$author <- RFSI$screen_name[match(tt$text, RFSI$text)]

# filter out the ones w/o authors
network <- tt %>% filter(tt$author != "NA")


# generate node list
authors <- network %>% distinct(author) 
names(authors) <- "label"
retweet_users <- network %>% distinct(retweet_user) 
names(retweet_users) <- "label"

nodes <- rbind(authors, retweet_users)
nodes <- nodes %>% distinct(label) #to get rid of duplicates in both authors and retweet user
nodes <- nodes %>% rowid_to_column("id")

# generate edge list
library(plyr)
per_route <- ddply(network, .(network$author, network$retweet_user), nrow)
names(per_route) <- c("authors", "retweet_users", "weight")

edges <- per_route %>% left_join(nodes, by = c("authors" = "label"))
colnames(edges)[4] <- "from"
edges <- edges %>% 
  left_join(nodes, by = c("retweet_users" = "label"))
colnames(edges)[5] <- "to"

edges <- dplyr::select(edges, from, to, weight)

graph <- graph.data.frame(edges, directed = T)
edges$value <- edges$weight
degree_value <- degree(graph, mode = "in")
nodes$value <- degree_value[match(nodes$id, names(degree_value))]

# ggraph packages to show network diagram
library(tidygraph)
library(ggraph)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

routes_tidy %>% 
  activate(edges) %>% 
  dplyr::arrange(desc(weight))

ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point(aes(size = value)) +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.1, 0.5)) +
  geom_node_text(aes(label = label), label.size = 0.25, repel = TRUE) +
  labs(edge_width = "Number") +
  theme_graph()















