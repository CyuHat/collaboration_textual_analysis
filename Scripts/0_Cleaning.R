# library----
pacman::p_load(rio, readtext, quanteda, dplyr,
               stringr, tidytext, readr, purrr,
               textcat)

clean <- modules::use("Scripts/Fun/fun_cleaning.R")

# Option----


# data----
# NOTE: Takes 20 seconds
# mytext raw raw
mytext_raw_raw <- 
  clean$import_pdf(the_path = "Raw_Data/",
                   the_delim = "/",
                   names_vec = c("category",
                                 "actors",
                                 "year",
                                 "file"))

# Save
export(mytext_raw_raw, file = "Clean_Data/mytext_raw_raw.rds")

# mytext raw
mytext_raw <- 
  mytext_raw_raw %>% 
  clean$clean_it() %>% 
  mutate(lang = textcat(text),
         lang = if_else(lang != "english", "[spanish]", "[english]"))
  
# Save the data
# Text object in case
export(mytext_raw, file = "Clean_Data/mytext_raw.rds")

# Export each text as single file
## Original
clean$export_text(mytext_raw,
                  "Translation/Original/")

## English
clean$export_text(mytext_raw,
                  "Translation/English/")

# Cleaning----
##  Corpus----
mycorpus <- corpus(mytext) 

# kwic(mycorpus, "bome")

# Save the corpus
export(mycorpus, file = "Clean_Data/mycorpus.rds")

## Document-Feature matrix----
## List of element to remove using regex
regex_list = list("n\\º\\.",
                  "^\\d\\w+",
                  "\\w+\\d$",
                  "_",
                  "\\w+(\\.|\\,)\\w+")

## dfm
## Note: Take 1 minute
mydfm <- 
  mycorpus %>% 
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE,
         remove_url = TRUE,
         remove_separators = TRUE,
         split_hyphens = TRUE,
         split_tags = TRUE) %>% 
  dfm() %>% 
  dfm_remove(c(stopwords("spanish"), stopwords("english"))) %>% 
  dfm_select(selection = "remove",
             valuetype = "regex",
             min_nchar = 3,
             pattern = regex_list) %>% 
  dfm_tolower()

# Save the dfm
export(mydfm, file = "Clean_data/mydfm.rds")

# Convert to stm
mystm <- convert(mydfm, to = "stm")

# Save stm format
export(mystm, file = "Clean_Data/mystm.rds")

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