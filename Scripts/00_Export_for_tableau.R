load("Clean_Data/word_topic_prob.rda")

library(tidyverse)
library(xlsx)

word_lookup <- 
  word_topic_prob %>% 
  select(-beta, -p, -topic_prev) %>% 
  arrange(term, -word_prop) %>% 
  group_by(term) %>% 
  slice(1)

word_filter <- 
  word_lookup %>% 
  mutate(alphabet = str_match(term, "^\\w")[1], .before = term) %>%
  filter(word_prop!=1) %>% 
  arrange(alphabet, theme, -word_prop) %>% 
  group_by(alphabet, theme) %>% 
  slice(1:3) %>% 
  ungroup() %>% 
  arrange(word_prop) %>% 
  slice(1:100) %>% 
  pull(term) %>% 
  .[c(-11, -12, -24, -63, -95)]

word_filter2 <- c(word_filter,
                  "migrant", "people", "illegal", "border",
                  "migrante", "personas", "ilegal", "frontera")

word_topic_prob %>% 
  select(Text = term, theme, word_prop) %>% 
  filter(Text %in% word_filter) %>% 
  mutate(theme = case_when(
    theme == "1.Security" ~ "pR",
    theme == "2.Human rights" ~ "pG",
    theme == "3.Administration" ~ "pB"
  )) %>% 
  spread(theme, word_prop) %>% 
  tibble::rowid_to_column("ID") %>% 
  select(ID, everything()) %>% 
  write.xlsx("Clean_Data/VestinData.xlsx")
  