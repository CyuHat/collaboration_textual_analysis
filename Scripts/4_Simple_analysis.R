# Libraries----
pacman::p_load(rio, dplyr, ggplot2,
               tidyr, stm, stringr,
               tidytext, tibble,
               broom)

# Data----
topic_16 <- import("Clean_Data/topic_16.rda")
mystm <- import("Clean_Data/mystm.rda")

# Tidygamma
tidy_gamma_raw <- tidy(topic_16, matrix = "gamma")

tidy_gamma <-
  tidy_gamma_raw %>% 
  filter(topic != 3) %>% 
  mutate(theme = case_when(
    topic %in% c(2:4, 13) ~ "1.Security", # Topic 3 already removed
    topic %in% c(5, 9, 16) ~ "2.Migrant rights",
    TRUE ~ "3.Administration"
  ), .before = topic) %>% 
  group_by(document, theme) %>% 
  summarise(gamma = sum(gamma)) %>% 
  ungroup()

save(tidy_gamma, tidy_gamma_raw, file = "Clean_Data/tidy_gamma.rda")

# Tidy beta for word highest probability to appear in a given topic
tidy_beta_raw <- tidy(topic_16, matrix = "beta")

# Topic prevalence to weight topic since each topic appears more or less than the others
topic_prevalence <- 
  topic_16$theta %>% 
  colMeans() %>% 
  tibble(topic_prev =.) %>% 
  rowid_to_column("topic")

# Word probability knowing topic
tidy_beta <- 
  tidy_beta_raw %>% 
  left_join(topic_prevalence) %>%
  filter(topic != 3) %>% 
  mutate(theme = case_when(
    topic %in% c(2:4, 13) ~ "1.Security", # Topic 3 already removed
    topic %in% c(5, 9, 16) ~ "2.Migrant rights",
    TRUE ~ "3.Administration"
  ), .before = topic) %>% 
  group_by(term, theme) %>% 
  mutate(group_prev = topic_prev/sum(topic_prev),
         beta = group_prev * beta) %>%
  summarise(beta = sum(beta)) %>% 
  ungroup()

# Prevalence with 3 frames
frame_prevalence <- 
  topic_prevalence %>% 
  mutate(theme = case_when(
    topic %in% c(2, 4, 13) ~ "1.Security", # Topic 3 already removed
    topic %in% c(5, 9, 16) ~ "2.Migrant rights",
    topic == 3 ~ "None",
    TRUE ~ "3.Administration"
  ),
  topic = str_c("Topic ", topic),
  topic = reorder(topic, topic_prev))

save(tidy_beta, tidy_beta_raw, file = "Clean_Data/tidy_beta.rda")
save(frame_prevalence, file = "Clean_Data/frame_prevalence.rda")

# Word topic probability
word_topic_prob <- 
  tidy_beta %>% 
  left_join(
    frame_prevalence %>% 
      summarise(topic_prev = sum(topic_prev), .by = theme) %>% 
      filter(theme!="None") %>% 
      mutate(topic_prev = topic_prev/sum(topic_prev))) %>% 
  mutate(p = beta*topic_prev) %>% 
  mutate(word_prop = p/sum(p), .by = term)

save(word_topic_prob, file = "Clean_Data/word_topic_prob.rda")

# Full file
full_stm <- 
  mystm$meta %>% 
  tibble() %>% 
  rowid_to_column("document") %>% 
  select(document, year, category, actors) %>% 
  left_join(tidy_gamma, by = "document") %>% 
  left_join(tidy_beta, by = "theme")

export(full_stm, file = "Clean_Data/full_stm.rda")

# Next step----
# TODO Understand the topical model functionning
# TODO write about this method
# TODO Try the QR regression
# TODO Control the language effect
# TODO If needed try with translation to spanish of some text
# TODO Try the simple visualization (bi-modal)