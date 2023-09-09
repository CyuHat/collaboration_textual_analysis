# Libraries----
pacman::p_load(rio, tidyverse, quanteda,
               FactoMineR, factoextra,
               DescTools, tidytext,
               xlsx, tibble)

# Data ----
word_topic_prob <- import("Clean_Data/word_topic_prob.rda")
mydfm <- import("Clean_Data/mydfm.rda")

# Workflow----
## Multivariate search----

### Removing rare words----
word_count_vec <- colSums(mydfm)

word_count_vec %>% 
  log() %>% 
  tibble(a =.) %>% 
  ggplot(aes(a)) +
  geom_density(fill = "cyan", alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, 10, 0.25))

word_selection <- word_count_vec[word_count_vec >=100]

length(word_selection)

# filtering an reating the matrix fo the PCA
word_topic_matrix <- 
  word_topic_prob %>% 
  filter(term %in% names(word_selection)) %>% 
  select(term, theme, p = word_prop) %>%
  spread(theme, p) %>% 
  column_to_rownames("term")

### PCA----
word_PCA <- PCA(word_topic_matrix)

fviz_pca_ind(word_PCA)

### HCPC----
# 10 clusters pour avoir une bonne partition
word_HCPC <- HCPC(word_PCA, nb.clust = 20)

fviz_mclust(word_HCPC)


# The most representative word of each cluster
most_rep <- function(elem){
  names(elem[1])
}

top_word_cluster <- 
  map(word_HCPC$desc.ind$para[1:20], most_rep) %>% 
  flatten_chr()

fviz_pca_ind(word_PCA, 
             select.ind = list(name = c(top_word_cluster)))

### Top word Migrant rights area----
top_word_MR <- 
  word_topic_prob %>% 
  filter(term %in% names(word_selection)) %>% 
  select(term, theme, p = word_prop) %>%
  mutate(theme = str_remove(theme, "\\d\\.")) %>% 
  spread(theme, p) %>% 
  janitor::clean_names() %>% 
  filter(migrant_rights >=0.5) %>% 
  pull(term)

### Top word Security area----
top_word_SE <- 
  word_topic_prob %>% 
  filter(term %in% names(word_selection)) %>% 
  select(term, theme, p = word_prop) %>%
  mutate(theme = str_remove(theme, "\\d\\.")) %>% 
  spread(theme, p) %>% 
  janitor::clean_names() %>% 
  filter(security >=0.5) %>% 
  pull(term)

### Top word administration area----
set.seed(7)
top_word_AD <- 
  word_topic_prob %>% 
  filter(term %in% names(word_selection)) %>% 
  select(term, theme, p = word_prop) %>%
  mutate(theme = str_remove(theme, "\\d\\.")) %>% 
  spread(theme, p) %>% 
  janitor::clean_names() %>% 
  filter(administration >=0.5) %>% 
  slice_sample(n = 30) %>% 
  pull(term)

### Top even word (Gini)----
top_word_even <- 
  word_topic_prob %>% 
  summarize(p = Gini(word_prop), .by = term) %>% 
  arrange(p) %>% 
  slice(1:30) %>% 
  pull(term)

### Keywords
key_words <- c("in-migración", "in-migrante",
               "frontera", "marruecos",
               "migration", "migrant",
               "borders", "morocco", "ceuta",
               "melilla", "asilo", "menor",
               "minor")

### Intermediate graph----
fviz_pca_ind(word_PCA, 
             select.ind = list(name = c(top_word_MR,
                                        top_word_AD,
                                        top_word_SE,
                                        top_word_even,
                                        top_word_cluster)))

### Final selection----
final_selection <- c(top_word_MR,
                     top_word_AD,
                     top_word_SE,
                     top_word_even,
                     top_word_cluster,
                     key_words)

final_word_topic <-
  word_topic_prob %>% 
  filter(term %in% final_selection) %>% 
  select(Terms = term, theme, p = word_prop) %>%
  mutate(theme = str_remove(theme, "\\d\\.")) %>% 
  mutate(p = (p+0.05)/sum(p+0.05), .by = Terms) %>%
  spread(theme, p) %>% 
  # rename("Migrant rights" = "Human rights") %>% 
  mutate_at(vars("Administration", "Migrant rights", "Security"),
            round, 2)
  

final_word_topic %>% 
  janitor::adorn_pct_formatting() %>% 
  write.xlsx("Clean_Data/List_of_Terms_proximity_per_topic.xlsx")

# Finale filter----
## Data----
# Last list filtered manual by us
final_filter <- 
  read.xlsx("Results/Tables/List_of_Terms_proximity_per_topic_filtered.xlsx",
          sheetIndex = 1) %>% 
  pull(Terms)

mytext <- import("Clean_Data/mytext.rda")

# Last
final_word_topic_filtered <- 
  final_word_topic %>% 
  filter(Terms %in% final_filter,
         Terms != "mohatar")

exportable_table <-
  mytext %>% 
  tibble() %>% 
  select(Actor = category, text) %>% 
  unnest_tokens("Terms", "text") %>% 
  distinct() %>% 
  filter(Terms %in% final_filter,
         Terms != "mohatar") %>% 
  left_join(final_word_topic_filtered) %>% 
  rowid_to_column("ID") %>% 
  select(ID, Actor, 
         Text = Terms,
         pR = Security,
         pG = `Migrant rights`,
         pB = Administration) %>% 
  drop_na()

exportable_table %>% 
  write.xlsx("Results/Tables/VestinData.xlsx",
             sheetName = "Data")

# Good formating
# FIXME don't remove this
# word_topic_prob %>% 
#   select(Text = term, theme, word_prop) %>% 
#   filter(Text %in% word_filter) %>% 
#   mutate(theme = case_when(
#     theme == "1.Security" ~ "pR",
#     theme == "2.Human rights" ~ "pG",
#     theme == "3.Administration" ~ "pB"
#   )) %>% 
#   spread(theme, word_prop) %>% 
#   tibble::rowid_to_column("ID") %>% 
#   select(ID, everything()) %>% 
#   mutate_at(vars(pB, pG, pR), ~ format(.x,scientific = FALSE)) %>% 
#   write.xlsx("Clean_Data/VestinData.xlsx")
  