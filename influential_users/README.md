## what tweets appeal to different groups?

The **word_list_method** folder contains the code for the first attempt at answering this question by relying on a mining users descriptions and identifying them based on key words within their descriptions.
  
This approach did not do a great job at accurately identifying all users as the short length and wide variety of content within descriptions are insuficient for characterizing users.    
  
  See the [word_list_method](https://github.com/Science-for-Nature-and-People/soc-twitter/tree/master/influencers/word_list_method) README for more detail
  
  ***
    
The **LDA_approach** folder uses supervised topical modeling that relies on both user's descriptions, as well as their past 100 tweets to attempt to classify them into groups.  

See the [LDA_approach](https://github.com/Science-for-Nature-and-People/soc-twitter/tree/master/influencers/lda_approach) README for for details
