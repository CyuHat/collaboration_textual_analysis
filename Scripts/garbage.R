# Libraries----
pacman::p_load(rio, dplyr, stringr, tidyr)

clean <- modules::use("Scripts/Fun/fun_clean.R")

# Data----
mytext_pos <- import("Clean_Data/mytext_pos.rds")


# TODO: Find the right number for the max n-gram.

# From POS to Corpus ---- 
mytext_group <- 
  mytext_pos %>% 
  drop_na(lemma) %>% 
  filter(upos %in% c("NOUN", "ADJ", "VERB")) %>% 
  select(doc_id, category, actors, year, lang, lemma) %>%
  summarize(text = str_c(lemma, collapse = " "),
            .by = doc_id:lang) %>% 
  select(-doc_id) %>%
  mutate(category = case_when(
    str_detect(category, "^Acta .* Ceuta$") ~ "Ceuta Municipality",
    str_detect(category, "^Acta .* Melilla$") ~ "Melilla Municipality",
    TRUE ~ category
  ))
  # mutate(text = str_replace_all(text, "\\s{2,}", " "),
  #        text = str_remove_all(text, "(Autonomous City\\s?)?validation document Autonomous city available citizen folder \\.ceuta\\.es/validacion document incorporate"),
  #        text = str_remove_all(text, "date \\d{2}/\\d{2}/\\d{4} de Verification code EXP Csv ensure integrity information"),
  #        text = str_remove_all(text, "(secure|de) Verification code EXP Csv ensure integrity information"),
  #        text = str_remove_all(text, "check Minute(\\s?ordinary\\s)?by city ASSEMBLY city first(\\s?CONVENING|call)"),
  #        text = str_remove_all(text, "first call|Department governing secretary file NÂº Expd minute"),
  #        text = str_remove_all(text, "nÂº Ref Minutes ordinary meet governing Council|Attendance excellent"),
  #        text = str_remove_all(text, "City Decree Councilor (environment Urban service|education Culture)"),
  #        text = str_remove_all(text, "AUTONOMOUS 30 agreement governing council Autonomous city"),
  #        text = str_remove_all(text, "Melilla secretariageneral@melilla\\.e(\\s?DE\\?)?general Secretariat reference procedure Session general Secretariat Minutes executive"),
  #        text = str_remove_all(text, "30 presidencia@melilla\\.e govern reference procedure Session governing (C|c)ouncil PTS"),
  #        text = str_remove_all(text, "interested party representative"),
  #        text = str_remove_all(text, "Secretariat governing council definitive minute governing council"),
  #        text = str_remove_all(text, "number Melilla friday(\\s?page AUTONOMOUS department)?"),
  #        text = str_remove_all(text, "validation document Autonomous city available citizen folder \\.ceuta\\.es/validacion document incorporate"),
  #        text = str_remove_all(text, "(check\\s?)?Minute(\\s?ordinary)?|by|1Âª|PÃ‰|D\\.Âª"),
  #        text = str_remove_all(text, "(Melilla|MELILLA)? secretariageneral@melilla\\.e(\\s?DE\\s?)?general Secretariat(\\s?Assembly\\s?)?reference procedure (Session general Secretariat)?(\\sdefinitive)?|meet  BUREAU City(\\s?hour)? minute|meeting Bureau"),
  #        text = str_remove_all(text, "meeting Bureau general Secretariat meet by BUREAU City"),
  #        text = str_remove_all(text, "number Melilla tuesday page AUTONOMOUS(\\s?departement)?"),
  #        text = str_remove_all(text, "(Melilla|MELILLA)?\\s?secretariageneral@melilla\\.e(\\s?DE)?\\s?general Secretariat(\\s?Assembly)?\\s?reference procedure\\s?\\s?(Session)?\\s?general Secretariat"),
  #        text = str_to_lower(text),
  #        text = str_remove_all(text, "de|â|[:punct:]|[:symbol:]|[:digit:]")) %>% 
  # select(text) %>% 
  # tidytext::unnest_ngrams("words", "text", n = 800L) %>% 
  # count(words, sort = TRUE) %>% 
  # print(n = 20)

export(mytext_group, "Clean_Data/mytext_group.rds")

# Function ----
(vec_filter <- 
  mytext_group %>% 
    select(text) %>% 
  clean$rep_ngram())

export(vec_filter, "Clean_Data/vec_filter.rds")

vec_filter <- import("Clean_Data/vec_filter.rds")

queries <- 
  vec_filter[10000:19380] %>% 
  str_c(collapse = "|")

mytext_group %>% 
  mutate(text = str_remove_all(text, queries)) %>% 
  select(text) %>% View()
