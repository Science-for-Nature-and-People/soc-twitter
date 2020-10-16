########################################
# Visualize time series of query terms #
########################################

#### LOAD PACKAGES ####
library(tidyverse)


#### LOAD DATA ####
noRT <- read.csv("soc-twitter/data_for_analysis/cleaned_data/noRT_clean_india.csv", stringsAsFactors = FALSE)
RT <- read.csv("soc-twitter/data_for_analysis/cleaned_data/RT_clean.csv", stringsAsFactors = FALSE)


#### MAKE PLOT ####
RT$M_Y <- format(as.Date(RT$created_at), "%Y-%m")
RT$query <- tolower(RT$query)

RT_clean_up <- RT %>%
  mutate(
    query =
      str_replace_all(tolower(query), paste(c("soil health", "#soilhealth", "healthy soil", "#healthysoil", "healthysoil"), collapse = "|"), "soilhealth")
  ) %>%
  mutate(query = str_replace_all(tolower(query), paste(c("soil quality", "#soilquality"), collapse = "|"), "soilquality")) %>%
  mutate(query = str_replace_all(tolower(query), paste(c("soil fertility", "#soilfertility"), collapse = "|"), "soilfertility")) %>%
  mutate(query = str_replace_all(tolower(query), paste(c("rangeland health", "#rangelandhealth", "healthy rangelands", "#healthyrangelands", "healthyrangelands"), collapse = "|"), "rangelandhealth")) %>%
  mutate(query = str_replace_all(tolower(query), paste(c("regenerative agriculture", "#regenerativeagriculture"), collapse = "|"), "regenerativeagriculture")) %>%
  mutate(query = str_replace_all(tolower(query), "#sustainableagriculture", "sustainable agriculture")) %>%
  #### get rid of wierd overlapping duplicates
  group_by(created_at, screen_name, text) %>%
  filter(retweet_count == max(retweet_count)) %>%
  ungroup() %>%
  distinct()

full <- RT_clean_up %>%
  mutate(query = str_replace_all(query, "#| |\"", "")) %>%
  mutate(query = str_replace_all(tolower(query), "“soilquality”", "soilquality")) %>%
  mutate(query = str_replace_all(tolower(query), "“regenerativeagriculture”", "regenerativeagriculture")) %>%
  mutate(query = str_replace_all(tolower(query), "“rangelandhealth”", "rangelandhealth")) %>%
  mutate(query = str_replace_all(tolower(query), "“soilhealth”", "soilhealth")) %>%
  mutate(query = str_replace_all(tolower(query), "“soilfertility”", "soilfertility")) %>%
  group_by(M_Y, query) %>%
  tally() %>%
  ungroup()

all_counts <- full %>%
  group_by(M_Y) %>%
  dplyr::summarise(
    n = sum(n),
    query = "all_terms"
  ) %>%
  ungroup()

RA_SH <- full %>%
  filter(query %in% c("soilhealth", "regenerativeagriculture"))

bind <- bind_rows(RA_SH, all_counts) %>%
  arrange(M_Y) %>%
  filter(M_Y != "2020-04")

## combine duplicats that were introduced when converting healthy soil to soilhealth
bind$M_Y <- parse_date_time(bind$M_Y, orders = "ym")

pdf("soc-twitter/figures/figure-1.pdf",width=11,height=8)
ggplot(bind, aes(x = M_Y, y = n)) +
  geom_line(aes(color = query, group = query)) +
  scale_color_manual(labels = c("All terms", "Regenerative\nagriculture", "Soil health"), 
                     values=c("#23487a", "#90214a", "#49a942")) +
  labs(
    x = "\nMonth",
    y  = "Number of tweets\n"
  ) +
  theme_classic() +
  geom_vline(aes(xintercept = as.numeric(bind$M_Y[32])), linetype = "dashed") +
  geom_vline(aes(xintercept = as.numeric(bind$M_Y[74])), linetype = "dashed") +
  theme(axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
    axis.title.y = element_text(size = 13),
    axis.title.x = element_text(size = 13),
        legend.title = element_blank(),
    legend.text = element_text(size=12)) +
  scale_x_datetime(date_labels = "%b-%Y", date_breaks = "2 month", date_minor_breaks = "1 month", expand = c(0, 0)) 
dev.off() 


#####################
#### DEPRECATED ####
####################
# library(tidytext)
# library(stringr)
# library(ggraph)
# library(igraph)
# library(tm)
# library(NLP)
# library(quanteda)
# library(SnowballC)
# library(lubridate)
# library(dplyr)
# library(ggpubr)
# source("soc-twitter/text_analysis_functions.R")

#### MANIPULATE DATA ####
# # Look at natural breakpoints in the data
# hist(noRT$retweet_count,breaks=4000,xlim=c(1,100))
# # Select top 100 of tweets based on their retweet count
# top_100_noRT <- noRT %>% 
#   arrange(-retweet_count) %>% 
#   head(100)
# 
# # Select an input data file
# #input <- top_10_noRT
# #input <- noRT
# input <- RT
# 
# # Create dataset for each seach term
# # Combine similar terms in hits column
# ## Soil health
# soil_health_tweets <- input %>% 
#   filter(
#     str_detect(tolower(text), paste(c("soil health","#soilhealth","healthy soil","#healthysoil"), collapse = '|')))
# soil_health_tweets$hits <- "soil health"
# 
# #### soil quality
# soil_quality_tweets <- input %>% 
#   filter(
#     str_detect(tolower(text), paste(c("soil quality","#soilquality"),collapse = '|')))
# soil_quality_tweets$hits <- "soil quality"
# 
# ### soil fertility
# soil_fertility_tweets <- input %>% 
#   filter(
#     str_detect(tolower(text), paste(c("soil fertility","#soilfertility"),collapse = '|')))
# soil_fertility_tweets$hits <- "soil fertility"
# 
# #### rangeland health
# rangeland_health_tweets <- input %>% 
#   filter(
#     str_detect(tolower(text), paste(c("rangeland health","#rangelandhealth","healthy rangelands", "#healthyrangelands"),
#                                     collapse = '|')))
# rangeland_health_tweets$hits <- "rangeland health"
# 
# ## Regenerative agriculture
# regen_agri_tweets <- input %>% 
#   filter(
#     str_detect(tolower(text), paste(c("regenerative agriculture","#regenerativeagriculture"), collapse = '|')))
# regen_agri_tweets$hits <- "regenerative agriculture"
# 
# # Combine soil health and regenerative into one data frame
# df <- soil_health_tweets %>% 
#   bind_rows(soil_quality_tweets) %>% 
#   bind_rows(soil_fertility_tweets) %>% 
#   bind_rows(rangeland_health_tweets) %>% 
#   bind_rows(regen_agri_tweets)
# 
# # Split hits into seperate rows
# hit_split <- separate_rows(df, hits, sep = ";") %>% filter(!is.na(hits)) 
# 
# # Split year, month, week and date data for later aggregation
# hit_split$created_at <- date(hit_split$created_at)
# hit_split$created_year <- year(hit_split$created_at)
# hit_split$created_month <- month(hit_split$created_at)
# hit_split$created_week <- week(hit_split$created_at)


# ### plot time series based on retweet_count
# # Figure 1. scatter plot
# theme_set(theme_minimal())
# ggplot(hit_split, aes(x = date(created_at), y = hits)) + 
#   geom_point(aes(size = retweet_count))
# 
# # Figure 2 & 3. line graph and barplot
# ggplot(hit_split, aes(y = retweet_count, x = week(created_at), color = hits)) + 
#   geom_line() + ggtitle('time series')
# 
# ggplot(hit_split, aes(y = retweet_count, x = week(created_at), fill = hits)) +
#   geom_bar(stat = "identity") + ggtitle('time series')
# 
# # Figure 4. heat map
# # add small value to 0's to avoid getting NAs in log transform
# hit_split$retweet_count[hit_split$retweet_count == 0] <- 0.00001
# 
# # find min and max of retweet count to set the limit
# min_rt = min(hit_split$retweet_count)
# max_rt = max(hit_split$retweet_count)
# 
# library(gtable)
# library(grid)
# 
# base_size <- 8
# p_17 <- ggplot(hit_split%>% filter(created_year == 2017), aes(week(created_at), y = hits)) +
#   geom_tile(aes(fill = retweet_count), color = "white") +
#   scale_fill_gradient(low = "white", high = "steelblue", trans = "log", 
#                       name = "retweet count (log scale)", 
#                       limit = c(min_rt,max_rt)) + 
#   labs(title = "Heat Map 2017", x = "week", y = "query term") + theme_grey(base_size = base_size)
# 
# p_18 <- ggplot(hit_split%>% filter(created_year == 2018), aes(week(created_at), y = hits)) +
#   geom_tile(aes(fill = retweet_count), color = "white") +
#   scale_fill_gradient(low = "white", high = "steelblue", trans = "log",
#                       name = "retweet count (log scale)", 
#                       limit = c(min_rt,max_rt)) + 
#   labs(title = "Heat Map 2018", x = "week", y = "query term") + theme_grey(base_size = base_size)
# 
# p_19 <- ggplot(hit_split%>% filter(created_year == 2019), aes(week(created_at), y = hits)) +
#   geom_tile(aes(fill = retweet_count), color = "white") +
#   scale_fill_gradient(low = "white", high = "steelblue", trans = "log",
#                       name = "retweet count (log scale)", 
#                       limit = c(min_rt,max_rt)) + 
#   labs(title = "Heat Map 2019", x = "week", y = "query term") + theme_grey(base_size = base_size)
# 
# p17 <- ggplotGrob(p_17)
# p18 <- ggplotGrob(p_18)
# p19 <- ggplotGrob(p_19)
# 
# ## combine three years data into one graph
# heatmap_top_100 <- rbind(p17, p18, p19, size = "last")
# grid.newpage()
# grid.draw(heatmap_top_100)
# 
# # Figure 5. bar plot separated by years
# b_17 <- ggplot(hit_split %>% filter(created_year == 2017), 
#                aes(y = retweet_count, x = week(created_at), fill = hits)) + 
#   geom_bar(stat = "identity") + ggtitle('Retweet count time series 2017')
# 
# b_18 <- ggplot(hit_split %>% filter(created_year == 2018), 
#                aes(y = retweet_count, x = week(created_at), fill = hits)) + 
#   geom_bar(stat = "identity") + ggtitle('Retweet count time series 2018')
# 
# b_19 <- ggplot(hit_split %>% filter(created_year == 2019), 
#                aes(y = retweet_count, x = week(created_at), fill = hits)) + 
#   geom_bar(stat = "identity") + ggtitle('Retweet count time series 2019')
# 
# b17 <- ggplotGrob(b_17)
# b18 <- ggplotGrob(b_18)
# b19 <- ggplotGrob(b_19)
# 
# RT_full <- rbind(b17, b18, b19, size = "last")
# grid.newpage()
# grid.draw(RT_full)
# 
# 
# ### plot time series based on total tweets count
# # group tweets by week and get counts
# # x is grouped week, y is counts of tweets, color coded by query words
# 
# # get counts of the occurence of each query word
# group <- data.frame(hit_split$created_year, hit_split$created_week, hit_split$hits)
# names(group) <- c("Year","Week","Hits")
# 
# #count value based on week and query words
# library(plyr)
# counts.df <- ddply(group, .(group$Year, group$Week, group$Hits), nrow)
# names(counts.df) <- c("Year", "Week", "Hits", "Freq")
# 
# 
# # Figure 1. scatter plot
# theme_set(theme_minimal())
# ggplot(counts.df, aes(x = Week, y = Hits)) + geom_point(aes(size = Freq))
# 
# ### to include only soil health and regen ag run the following:
# counts.df <- filter(counts.df, Hits %in% c("soil health", "regenerative agriculture"))
# 
# # Figure 2 & 3. line and bar graph
# ggplot(counts.df, aes(y = Freq, x = Week, color = Hits)) + 
#   geom_line() + ggtitle('time series')
# 
# ggplot(counts.df, aes(y = Freq, x = Week, fill = Hits)) +
#   geom_bar(stat = "identity") + ggtitle('time series')
# 
# 
# 
# # Figure 4. heat map
# #find min and max of retweet count to set the limit
# min_f = min(counts.df$Freq)
# max_f = max(counts.df$Freq)
# 
# library(gtable)
# library(grid)
# 
# base_size <- 8
# p_17 <- ggplot(counts.df%>% filter(Year == 2017), aes(Week, y = Hits)) +
#   geom_tile(aes(fill = Freq), color = "white") +
#   scale_fill_gradient(low = "white", high = "steelblue", trans = "log", 
#                       name = "retweet count (log scale)", 
#                       limit = c(min_f,max_f)) + 
#   labs(title = "Heat Map 2017", x = "week", y = "query term") + theme_grey(base_size = base_size)
# 
# p_18 <- ggplot(counts.df%>% filter(Year == 2018), aes(Week, y = Hits)) +
#   geom_tile(aes(fill = Freq), color = "white") +
#   scale_fill_gradient(low = "white", high = "steelblue", trans = "log",
#                       name = "retweet count (log scale)", 
#                       limit = c(min_f,max_f)) + 
#   labs(title = "Heat Map 2018", x = "week", y = "query term") + theme_grey(base_size = base_size)
# 
# p_19 <- ggplot(counts.df%>% filter(Year == 2019), aes(Week, y = Hits)) +
#   geom_tile(aes(fill = Freq), color = "white") +
#   scale_fill_gradient(low = "white", high = "steelblue", trans = "log",
#                       name = "retweet count (log scale)", 
#                       limit = c(min_f,max_f)) + 
#   labs(title = "Heat Map 2019", x = "week", y = "query term") + theme_grey(base_size = base_size)
# 
# p17 <- ggplotGrob(p_17)
# p18 <- ggplotGrob(p_18)
# p19 <- ggplotGrob(p_19)
# 
# heatmap_top_100_count <- rbind(p17, p18, p19, size = "last")
# grid.newpage()
# grid.draw(heatmap_top_100_count)
# 
# # Figure 5. bar plot separate by years
# count_17 <- ggplot(counts.df %>% filter(Year == 2017), aes(y = Freq, x = Week, fill = Hits)) + 
#   geom_bar(stat = "identity") + ggtitle('Total Tweet Count time series 2017')  
# count_18 <- ggplot(counts.df %>% filter(Year == 2018), aes(y = Freq, x = Week, fill = Hits)) + 
#   geom_bar(stat = "identity") + ggtitle('Total Tweet Count time series 2018')
# count_19 <- ggplot(counts.df %>% filter(Year == 2019), aes(y = Freq, x = Week, fill = Hits)) + 
#   geom_bar(stat = "identity") + ggtitle('Total Tweet Count time series 2019')
# 
# c17 <- ggplotGrob(count_17)
# c18 <- ggplotGrob(count_18)
# c19 <- ggplotGrob(count_19)
# 
# count_full<- rbind(c17, c18, c19, size = "last")
# grid.newpage()
# grid.draw(count_full)
# 
# 
# 
# 
# 
# ### only SH and RA
# counts <- ggplot(filter(bind, query %in% c("soilhealth","regenerativeagriculture") & M_Y >= as.Date('2018-10-01') & M_Y < as.Date('2020-04-01')), aes(x = M_Y, y = n)) +
#   geom_line(aes(color = query, group = query)) +
#   labs(
#     x = "month",
#     y = "number of tweets",
#     title = "monthly count for each search tearm"
#   ) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = c(.2,.9)) +
#   scale_x_datetime(date_labels = "%b-%Y", date_breaks = "1 month", date_minor_breaks = "1 month", expand = c(0,0)) 
# counts
# 
# 
# counts_all <- noRT_clean_up %>% 
#   mutate(query = str_replace_all(query, "#| |\"", "")) %>% 
#   group_by(M_Y, query) %>% 
#   tally() %>% 
#   ungroup() %>% 
#   group_by(M_Y) %>% 
#   dplyr::mutate(monthly_total = sum(n)) %>% 
#   ungroup() %>% 
#   mutate(prop = n/monthly_total) %>% 
#   filter(query %in% c("soilhealth","regenerativeagriculture"))
# 
# 
# 
# counts_all$M_Y <- parse_date_time(counts_all$M_Y, orders = "ym")
# 
# tmp <- counts_all %>% 
#   filter(M_Y >= as.Date('2018-10-01') & M_Y < as.Date('2020-04-01'))
# 
# prop <- ggplot(tmp, aes(M_Y, prop)) +
#   geom_line(aes(color = query, group = query)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "none") +
#   labs(x = "month",
#        y = "proportion of all tweets",
#        title = "proportion of all tweets over time") +
#   scale_x_datetime(date_labels = "%b-%Y", date_breaks = "1 month", date_minor_breaks = "1 month", expand = c(0,0)) 
# 
# ggarrange(counts, prop, labels = c("A", "B"))
# 
# full$M_Y <- parse_date_time(full$M_Y, orders = "ym")
# 
# 
# 
# ggplot(full, aes(M_Y, n, group = query, color = query)) +
#   geom_line() +  
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = c(.2,.8)) +
#   scale_x_datetime(date_labels = "%b-%Y", date_breaks = "1 month", date_minor_breaks = "1 month", expand = c(0,0)) 
# 
# ggplot(full, aes(M_Y, n, fill = query, color = query)) +
#   geom_bar(stat = "identity") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = c(.2,.8)) +
#   scale_x_datetime(date_labels = "%b-%Y", date_breaks = "1 month", date_minor_breaks = "1 month", expand = c(0,0)) 
