## visualizing tweet content  

The three .Rmd files in this folder are a first attempt at visuallzing the content of tweets from our dataset

* [bigram_plots](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/tweet_content/bigram_plots.Rmd) uses bigrams create network graphs that visualize the common flow of words among all tweets.  These visualizations are separated into:  
  - the full noRT dataset. 
  - tweets about:
      - soil 
      - forest 
      - rangeland health
   - then repeats this for the top 100 tweets based on their RT count
     
     
     
* [word_assoc](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/tweet_content/word_assoc.Rmd) looks at the most common words that are associated with our different categories of interest. i.e.:
  - soil, soil health, soil quality
  - rangeland, rangeland health, rangeland quality
  - forest, forest health, forest quality 
    
      
* [word_clouds](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/tweet_content/word_clouds.Rmd) creates wordclouds for the tweets within these same ^ categories 

 
 
The code in each of these .Rmd's relies on functions that are stored and documented in [text_analysis_functions.R](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/tweet_content/text_analysis_functions.R)
