# Libraries----
pacman::p_load(rio, dplyr, ggplot2,
               tidyr, stm, tictoc,
               tidytext, purrr, tibble,
               broom, car, betareg)

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
    topic %in% c(5, 9, 16) ~ "2.Human rights",
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

tidy_beta <- 
  tidy_beta_raw %>% 
  left_join(topic_prevalence) %>%
  filter(topic != 3) %>% 
  mutate(theme = case_when(
    topic %in% c(2:4, 13) ~ "1.Security", # Topic 3 already removed
    topic %in% c(5, 9, 16) ~ "2.Human rights",
    TRUE ~ "3.Administration"
  ), .before = topic) %>% 
  group_by(term, theme) %>% 
  mutate(group_prev = topic_prev/sum(topic_prev),
         beta = group_prev * beta) %>%
  summarise(beta = sum(beta)) %>% 
  ungroup()

save(tidy_beta, tidy_beta_raw, file = "Clean_Data/tidy_beta.rda")
save(topic_prevalence, file = "Clean_Data/topic_prevalence.rda")

# Full file
full_stm <- 
  mystm$meta %>% 
  tibble() %>% 
  rowid_to_column("document") %>% 
  select(document, year, category, actors) %>% 
  left_join(tidy_gamma, by = "document") %>% 
  left_join(tidy_beta, by = "theme")

export(full_stm, file = "Clean_Data/full_stm.rda")

# Test----
## Label topic
labelTopics(topic_16)

## Representatives document
findThoughts(topic_16, mystm$meta$file, n = 3)

## Topic correlation (useless)
topicCorr(topic_16) %>% 
  plot()

# Summary
plot(topic_16, type = "summary")

View(plot.STM)

# Regressions
# TODO Find a way to account for "Global" uncertainty
mystm$meta$fyear <- factor(mystm$meta$year)

reg <- estimateEffect(1:16 ~ fyear + category,
                      # uncertainty = "None",
                      topic_16, mystm$meta
                      )
summary(reg)

# tidygamma----
mystm$meta$document <- 1:nrow(mystm$meta)

(tidy_reg <- 
  tidy_gamma %>% 
  left_join(mystm$meta) %>% 
  group_by(topic) %>% 
  nest() %>% 
  mutate(model = map(data, ~ lm(gamma ~ fyear + category, data = .x)),
         summary = map(model, ~ summary(.x))))
# Result are really similar to the one of Estimate effect with uncertainty = "None"

# Regression model
sjPlot::tab_model(tidy_reg$model)

# Function just in case
for_bootstrap <- function(id, tidy_gamma, mystm){
  model <- 
    tidy_gamma %>% 
    left_join(mystm$meta) %>% 
    filter(topic == id)
  
  lm(gamma ~ fyear + category, model) %>% 
    Boot(ncores = 2) %>% 
    tidy(conf.int = TRUE)
}

all_map <- map(1:16, for_bootstrap, tidy_gamma, mystm)

# Next step----
# TODO Understand the topical model functionning
# TODO write about this method
# TODO Try the QR regression
# TODO Control the language effect
# TODO If needed try with translation to spanish of some text
# Try the simple visualization (bi-modal)
