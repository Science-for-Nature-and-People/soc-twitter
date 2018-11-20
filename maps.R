###################################
# Creating a map regarding tweets #
###################################

# === Libraries === 
library(tmap)
library(rworldmap)

# === Datasets === 
## uncomment if not loaded into environment
#twitter_merged <- read.csv("twitter_merged.csv", stringsAsFactors = FALSE)
#twitter_merged_noRT <- read.csv("twitter_merged_noRT.csv", stringsAsFactors = FALSE)

count_location <- twitter_merged %>%
                        group_by(country) %>%
                            summarise(count = n())

count_location$count <- as.numeric(count_location)
count_location$count[2]

tm_shape(count_location) +
  tm_polygons(count)

World$HPI
count_location$count
