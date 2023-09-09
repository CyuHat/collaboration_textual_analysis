# Libraries----
pacman::p_load(dplyr, rio, textcat)

# Export Document list ----
here %>% 
  tibble() %>% 
  mutate(language = textcat(text),
         id = 1:nrow(.)) %>% 
  select(id, document = file, actor = category, year, language) %>% 
  rio::export(file = "CodeBook/Documents_list.xlsx")