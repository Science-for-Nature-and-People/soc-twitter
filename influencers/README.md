## what tweets appeal to different groups?

The **word_list_method** folder contains the code for the first attempt at answering this question. The general apprach here was to:
  1) view the user descriptions of users from known groups to create a world list of terms that could be used to categorize users into different groups based on the respective user descriptions. The code for this is in [user_groups.R](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/influencers/word_list_method/user_groups.R)
  2) use the twitter API to ID all the users who retweeted each tweet, then categorize these users based by applying the world list (created above) to their descriptions. 
  3) Could then calculate which group was most influenced by each tweet and create word clouds (or other visualizations) of the type of content that apealed most to each group. see: [group_favorites.R](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/influencers/word_list_method/group_favorites.R)
