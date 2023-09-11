# library----
pacman::p_load(rio, readtext, quanteda, dplyr,
               stringr, tidytext, readr, purrr,
               textcat)

clean <- modules::use("Scripts/Fun/fun_import.R")

# data----
# NOTE: Takes 20 seconds
# mytext raw raw
mytext_raw <- 
  clean$import_pdf(the_path = "Raw_Data/",
                   the_delim = "/",
                   names_vec = c("category",
                                 "actors",
                                 "year",
                                 "file"))

# Save
export(mytext_raw, file = "Clean_Data/mytext_raw.rds")

# mytext raw
mytext <- 
  mytext_raw %>% 
  clean$clean_it() %>% 
  mutate(lang = textcat(text),
         lang = if_else(lang != "english", "[spanish]", "[english]"))

# Remove the raw version
rm(mytext_raw)
 
# Save the data
# Text object in case
export(mytext, file = "Clean_Data/mytext.rds")

# Export each text as single file
## Original
clean$export_text(mytext,
                  "Translation/Original/",
                  sepa = "__")
