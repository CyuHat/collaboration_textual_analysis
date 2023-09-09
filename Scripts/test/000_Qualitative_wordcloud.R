# Librairies----
pacman::p_load(readtext, tidytext, tidyverse, stopwords, ggwordcloud)

# data----
quali <- 
  readtext("Transcripts/") %>% 
  tibble()

# Token----
token_quali <- 
  quali %>% 
  mutate(doc_id = str_replace(doc_id,
                              "(t|T)ranscript interview (\\w+)\\.pdf",
                              "\\2")) %>% 
  unnest_tokens("words", "text") %>% 
  filter(!words %in% data_stopwords_stopwordsiso$en,
         !words %in% data_stopwords_stopwordsiso$it,
         !words %in% data_stopwords_stopwordsiso$es,
         !words %in% c("yeah"))

# Wordcloud----
set.seed(7)
token_quali %>% 
  count(words, name = "freq") %>% 
  slice_max(freq, n = 60) %>% 
  ggplot(aes(label = words, size = freq)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 24) +
  theme_minimal() +
  labs(title = "Word cloud from qualitative interviews")
