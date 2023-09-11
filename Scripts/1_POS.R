# library----
pacman::p_load(rio, readtext, dplyr,
               tidyr, udpipe, stringr,
               purrr, furrr, tictoc)

pos <- modules::use("Scripts/Fun/fun_pos.R")

# options ----
plan(multisession, workers = 3)

# Data ----
# Untranslated version
mytext <- import("Clean_Data/mytext.rds")

# Translated version
mytext_trans <-
  pos$import_txt("Translation/English/") %>%
  separate_wider_delim(doc_id, delim = "__",
                       names = c("category", "file", "lang")) %>% 
  mutate(lang = str_remove(lang, "\\.txt"),
         text = if_else(Encoding(text) != "UTF-8",
                        str_c(text, " é"),
                        text),
         text = str_conv(text, "UTF-8"))

# Saving the data
export(mytext_trans, file = "Clean_Data/mytext_trans.rds")

# Cleaning all
mytext_clean <- 
  mytext %>% 
  select(-text) %>% 
  left_join(mytext_trans)

# Saving it
export(mytext_clean, file = "Clean_Data/mytext_clean.rds")

# Removing the data that I don't need anymore
rm(mytext, mytext_trans)

# PART OF SPEECH tagging
  # We use englishLines model because it has the best performance for
  # part of pseech tagging (UPOS: 94.9) and lemmatization (lemma 97.1) in
  # english (10.09.2023).
  # Source: https://github.com/jwijffels/udpipe.models.ud.2.5/blob/master/inst/udpipe-ud-2.5-191206/README

tic() # Take ~24 minutes
mytext_pos <- 
  mytext_clean %>% 
  mutate(text = future_map(text, pos$pos, .progress = TRUE)) %>% 
  unnest(text)
toc()

rm(mytext_clean)

export(mytext_pos, file = "Clean_Data/mytext_pos.parquet")
export(mytext_pos, file = "Clean_Data/mytext_pos.rds")

rm(mytext_pos)