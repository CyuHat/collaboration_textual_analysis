# library----
library(readtext)
library(quanteda)
library(dplyr)

# data----
# NOTE: Takes 20 seconds
mytext <- readtext("Raw_Data/",
                   docvarsfrom = "filepaths",
                   docvarnames = c("folder", "empty", "category", "actors", "year", "file"),
                   dvsep = "/")

# Save the data
save(mytext, file = "Clean_Data/mytext.rda")

# Cleaning----
##  Corpus----
mycorpus <- corpus(mytext) 

# Save the corpus
save(mycorpus, file = "Clean_Data/mycorpus.rda")

## Document-Feature matrix----
## dfm
## Note: Take 1 minute
mydfm <- 
  mycorpus %>% 
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE,
         remove_url = TRUE) %>% 
  dfm() %>% 
  dfm_remove(c(stopwords("spanish"), stopwords("english"))) %>% 
  dfm_tolower()

# Save the dfm
save(mydfm, file = "Clean_data/mydfm.rda")

# Convert to stm
mystm <- convert(mydfm, to = "stm")

# Save stm format
save(mystm, file = "Clean_Data/mystm.rda")

# Next step ----
# TODO [Vestin] (optional) install the "reprex" package for easly sharing bug
# TODO [Vestin] (optional) rename the file to know better what they are about (can be confusing in topic modeling)
# TODO [Vestin] Transform "mytext" into corpus
# TODO [Vestin] Tokenize the corpus (cleaning part)
# TODO [Vestin] Transform the tokens into dfm (document feature matrix)
# TODO [Vestin] Transform the dfm into a stm 
# TODO [Vestin] Search the right number of topics
# TODO [Vestin] DO the topic modeling
# TODO [Vestin] Analyse the result, correct and redo if needed
# TODO [Vestin] Create vizualisations