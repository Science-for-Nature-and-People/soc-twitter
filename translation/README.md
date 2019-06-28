# Hindi Translation
code for initial attempts at translating hindi text within tweets


So far there have been two attempts:
  1. [hindi_words.R](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/translation/hindi_words.R) approach was to isolate each hindi word ([hind_terms.csv](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/translation/hindi_terms.csv)) and then to translate each of them individually, then replace each hindi word with its english translation [hindi-translated.csv](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/translation/hindi-translated.csv)
      -this failed, likely b/c hindi has some modifier characters that got dropped during issolation, so when we replaced the terms, the results was lots of gibberish 
  2. [translate_hindi.R](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/translation/translate_hindi.R) the goal here was to create a .txt file ([]) of all the tweets that contain hindi, and then to translate the entire .txt file ([hindi_tweets_translated.txt](https://github.com/Science-for-Nature-and-People/soc-twitter/blob/master/translation/hindi_tweets_translated.txt)) via google translate, then import back into R and replace the hindi tweets with their english translation
      - unfortunately google takes liberties with its syntax and did not comform to translating within/between delimiters ('||") so when I relayed the translated text back into a df, the tweets were misaligned...
  
  
  Next attempt should involve direct use of the google API
  

