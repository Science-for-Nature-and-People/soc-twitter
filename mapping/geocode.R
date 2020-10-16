library(tidyverse)
library(ggmap)


## most up to date data
RT <- read.csv("/home/shares/soilcarbon/Twitter/Merged_v4/twitter_merged_v4.csv",stringsAsFactors = FALSE)

## select attributes of interest (place name and query is most important)
RT_sub <- RT %>% 
  dplyr::select(provenance, created_at, screen_name, favorite_count, retweet_count, query, place_name, country) %>% 
  filter(!is.na(place_name)) %>% 
  mutate(
    place_full = paste(place_name, country, sep = ", ")
  )

# get vectore of unique place names
unique_loc <- unique(RT_sub$place_full)

# geocode
coords <- geocode(unique_loc)

# combine locations with coordinates
coord_loc <- data.frame(place_full = unique_loc, coords)

# join back to data
locations <- left_join(RT_sub, coord_loc)

## write this data
write_csv(locations, "locations_v4.csv")
locations <- read_csv("locations_v4.csv")

### update to include only SH and Regen Ag
#remove spaces and hashtags from query terms
locations <- locations %>% 
  mutate(query = tolower(str_replace_all(query, "#| |\"", "")))

locations$query[locations$query == "healthysoil"] <- "soilhealth"

SH_RA_locs <- filter(locations, query %in% c("soilhealth", "regenerativeagriculture"))
write_csv(SH_RA_locs, "SH_RA_locs_v4.csv")

## also write to aurora
write_csv(SH_RA_locs, "/home/shares/soilcarbon/Twitter/SH_RA_locs_v4.csv")
