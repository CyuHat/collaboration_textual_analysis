# library----
library(readtext)
library(quanteda)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stm)
library(tictoc)

# Options----
theme_set(theme_bw())

# Data----
load("Clean_Data/mydfm.rda")
load("Clean_Data/mystm.rda")

# Search the good number of topics
# Take 16 minutes
set.seed(7)
res_searchk <- searchK(mystm$documents, 
                       mystm$vocab, 
                       K = 3:20, 
                       prevalence = ~ year+category,
                       data = mystm$meta)

save(res_searchk, file = "Clean_Data/res_searchk.rda")

# Number of topics = 8-9
res_searchk$results %>%
  mutate_all(unlist) %>% 
  select(K, exclus, semcoh) %>% 
  mutate_at(.vars = c("exclus", "semcoh"), ~scale(.x)) %>% 
  gather("metrics", "value", exclus, semcoh) %>% 
  ggplot(aes(K, value, color = metrics)) +
  geom_line(size = 1.5) +
  labs(title = "Metrics values for the structural topic modeling",
       subtitle = "Standardized") +
  scale_x_continuous(breaks = 3:20)

# 10 topics
# take 1 minute
set.seed(7)
tic()
topic_10 <- stm(mystm$documents, 
                mystm$vocab, 
                K = 10, 
                prevalence = ~ year+category,
                data = mystm$meta)
toc()

# 15 topics
# take 1 minute
set.seed(7)
tic()
topic_15 <- stm(mystm$documents, 
                mystm$vocab, 
                K = 15, 
                prevalence = ~ year+category,
                data = mystm$meta)
toc()

# 16 topics
# take 1 minute
set.seed(7)
tic()
topic_16 <- stm(mystm$documents, 
                mystm$vocab, 
                K = 16, 
                prevalence = ~ year+category,
                data = mystm$meta)
toc()

save(topic_10, topic_15, topic_16, file = "Clean_Data/topic_tryout.rda")

# Comparison----
plot(topic_10)
plot(topic_15)
plot(topic_16)

res_selectM <- selectModel(mystm$documents, 
                           mystm$vocab, 
                           K = 3, 
                           prevalence = ~ year+category,
                           data = mystm$meta)

plotModels(res_selectM)
