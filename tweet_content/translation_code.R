#### Translate from hindi to english #####
library(translateR)
library(stringi)
library(stringr)


# google_api <- ''

#sample text
noRT[67,]$text







  
foo <- str_extract_all(noRT[67,]$text, '[\u0900-\u097F]+') #extract all hindi characters
bar <- unlist(foo)


word <- sapply(strsplit(bar[1], NULL)[[1L]], utf8ToInt)


stri_trans_general(bar, "hi")

stri_enc_toutf8(bar)


translated <- translate(content.vec =  stri_enc_toascii(bar),
                 google.api.key = google_api,
                 source.lang = 'hi',
                 target.lang = 'en')








