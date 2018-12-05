###################################
# Creating a map regarding tweets #
###################################

# === Libraries === 
library(tmap)
library(rworldmap)
library(tidyverse)
library(sf)
library(countrycode)
library(spdplyr)
library(classInt)

# === Datasets === 

## uncomment if not correct working directory
#setwd('/home/nolasco/soc-twitter')
#list.files()

## uncomment if not loaded into environment
twitter_merged <- read.csv("twitter_merged.csv", stringsAsFactors = FALSE)
twitter_merged_noRT <- read.csv("twitter_merged_noRT.csv", stringsAsFactors = FALSE)

# Creating a Country Code column for the datasets 
twitter_merged['countryCode'] <- countrycode(twitter_merged$country, 'country.name', 'iso3c')
twitter_merged_noRT['countryCode'] <- countrycode(twitter_merged_noRT$country, 'country.name', 'iso3c')


# Not all countries had a proper match, hence I had to add it manually
no_matchCountry <- c('Belgique', 'Deutschland', 'España', 'Kenia', 'Nederland', 
                     'Österreich', 'Panamá', 'Roumanie', 'Spanje', '台灣', 'भारत')
no_matchCode <- c('BEL', 'DEU', 'ESP', 'KEN', 'NLD', 
                  'AUT', 'PAN', 'ROU', 'ESP', 'TWN', 'IND')

for (i in 1:length(no_matchCountry)){
  twitter_merged$countryCode[twitter_merged$country == no_matchCountry[i]] <- no_matchCode[i]
  twitter_merged_noRT$countryCode[twitter_merged_noRT$country == no_matchCountry[i]] <- no_matchCode[i]
}

# Creating a count dataframe
count.RT <- twitter_merged %>%
                          count(countryCode) %>%
                              na.omit()

count.noRT <- twitter_merged_noRT %>%
                          count(countryCode) %>%
                              na.omit()


# Changing column name so it isn't similar to function name
colnames(count.RT)[2] <- 'Frequency'
colnames(count.noRT)[2] <- 'Frequency'


# Converting the count dataframe into sp class
count.RTsp <- joinCountryData2Map(count.RT, joinCode = "ISO3", nameJoinColumn = "countryCode")
count.noRTsp <- joinCountryData2Map(count.noRT, joinCode = "ISO3", nameJoinColumn = "countryCode")

## Dataframes not including US (might be more efficient code involving spdplyr)
# Creating a count dataframe
count.RT_US <- twitter_merged %>%
  count(countryCode) %>%
  na.omit() %>%
  filter(countryCode != 'USA')

count.noRT_US <- twitter_merged_noRT %>%
  count(countryCode) %>%
  na.omit() %>%
  filter(countryCode != 'USA')


# Changing column name so it isn't similar to function name
colnames(count.RT_US )[2] <- 'Frequency'
colnames(count.noRT_US )[2] <- 'Frequency'


# Converting the count dataframe into sp class
count.RT_USsp <- joinCountryData2Map(count.RT_US, joinCode = "ISO3", nameJoinColumn = "countryCode")
count.noRT_USsp <- joinCountryData2Map(count.noRT_US, joinCode = "ISO3", nameJoinColumn = "countryCode")

# === Interactive Maps ===
tm_shape(count.RTsp) +
  tm_polygons('Frequency') + tm_layout(main.title = "With RT")

tm_shape(count.noRTsp) + 
  tm_polygons('Frequency') + tm_layout(main.title = "No RT")


## As we can tell, US has the most number of tweets regarding soil
# Since US is such a huge outlier, I'm going to remove it to see 
tm_shape(count.RT_USsp) +
  tm_polygons('Frequency') + tm_layout(main.title = "With RT (not including US)")

tm_shape(count.noRT_USsp) +
  tm_polygons('Frequency') + tm_layout(main.title = "No RT (not including US)")

# === Changing the frequency level to include US and more info about other states ===
tm_shape(count.noRTsp) +
  tm_polygons('Frequency', style = 'fixed', breaks = c(1,2,5,10,100,929)) + tm_layout(main.title = "No RT")

tm_shape(count.RTsp) +
  tm_polygons('Frequency', style = 'fixed', breaks = c(1,2,5,10,100,929)) + tm_layout(main.title = "RT")

# === Final map ===
# Because RT and no RT dataset has same frequency count, we just used one map

tm_shape(count.RTsp) +
  tm_polygons('Frequency', style = 'fixed', breaks = c(1,2,5,10,100,929)) + tm_layout(main.title = "Tweet Frequency per Country")
