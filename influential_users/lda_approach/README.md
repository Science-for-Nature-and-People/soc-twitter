## Topic Modeling - Latent Dirichlet Allocation (LDA) approach
using LDA to classify users

##### Tidy Text approach
[TidyText_LDA](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/influencers/lda_approach/TidyText_LDA.R) applys the Tidy Text topical modeling approach to our dataset. This is an unsupervized LDA and an initial runthrough yielded poor results. Might be worth further exploration though as the LDA:: package is pretty complicated
  
##### LDA:: package for supervised LDA

1. manually identify ~400 users (the data for which is in the [manual_grouping_data]() folder)
2. generate user content to be used as 'documents' within the sLDA
     - [generate_content.R](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/influencers/lda_approach/generate_content.R) has the code for querying the Twitter API on all ~130,000 unique users and creating a table that has their user descriptions + past 100 tweets. The idea here was to gather enough information from each user that would allow the LDA model to begin to distinguish between user groups. Because of query limits on the API, this code will take about 7 days to run, and so if we want to get info on new users that arise within our dataframe, you should recreate this code for those specific users alone.
       - this content is writen to **user_content.csv** so this doesn't have to be run again
       - **note to next user:** at the bottom of this script is a 'work-in-progress' bit of code that is trying to continously write the API data so if the loop crashes, not all is lost. check the comment at the top of the script for thoughts on how to proceed.
     - [generate_training_content](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/influencers/lda_approach/generate_training_content.R) is the exact same code but only for the users that were manually identified. This was so we could begin testing the model while the other user content was downloading.
        - content written to: **training_content.csv**
3. [training_data.R](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/influencers/lda_approach/training_data.R) This code combines the grouping data from **step 1** and the training content from **step 2** and formats this data for use the the slda.em() function. Then runs the model and uses some withheld data to cross-validate the model results/predictions. Formating the data relies on functions stored in [format_LDA_functions.R](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/influencers/lda_approach/format_LDA_functions.R)
    - this workflow this is designed to be run for each user group (i.e run iteratively to predict if a user is media, political, scientist, etc.). the last section of this code runs the model for both 'is_media' and 'is_politcal' users and initial results are not very promising. however there is a lot of parameter tweaking that can (and should) be done before ruling out this method.
    - run demo(slda) in the consol for the package creators' example workflow on how to run the model
    
 This is still a work in progress and some effort should be put into fulling understand the model inputs (i.e. alpha, param, etc, interations) as well as how to interprete the results
