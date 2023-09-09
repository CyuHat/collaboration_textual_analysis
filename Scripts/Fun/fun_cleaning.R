# Libraries----
modules::import("magrittr")
modules::export("import_pdf", "clean_it", "export_text")

# read_pdf----
# Read pdf
  # A wrapper function that allow to read a pdf file using pdf_text() from {pdftools}.
read_pdf <- function(the_doc) {
  pdftools::pdf_text(the_doc) %>% 
    stringr::str_c(collapse = "\\n")
}

# import_pdf----
# Import pdf
  # Main function that allow to import multiple pdfs based on the main folder.
import_pdf <- function(the_path = ".", the_delim = "/", names_vec) {
  the_pdfs <- 
    list.files(the_path,
               recursive = TRUE,
               full.names = TRUE)
  
  future::plan(future::multisession, workers = 3)
  
  content_pdf <- 
    furrr::future_map_chr(the_pdfs, read_pdf,
                   .progress = TRUE)
  
  doc_id <- stringr::str_remove(the_pdfs,
                                stringr::str_c(the_path, "/", sep =""))
  
  dplyr::tibble(doc_id = doc_id,
                text = content_pdf,
                path = doc_id) %>% 
    tidyr::separate_wider_delim(path, 
                                delim = the_delim,
                                names = names_vec)
}

# clean_it----
# Clean it
  # A set of small cleaning before starting the important work
clean_it <- function(df){
  df %>% 
    dplyr::mutate(text = stringr::str_replace_all(text, "_", " "),
                  text = stringr::str_replace_all(text, "\\s{2,}|\\t{2,}", " "),
                  text = stringr::str_remove_all(text, "[\\.\\-\\,]{2,10}"),
                  text = stringr::str_remove_all(text, "[\\.\\-\\,]\\s[\\.\\-\\,]"),
                  text = stringr::str_remove_all(text, "[:punct:]{2,}"),
                  text = stringr::str_remove_all(text, "(http|www)\\w+"),
                  text = stringr::str_remove_all(text, "(BOLETN\\s|ARTCULO)?BOME-\\w-\\d{1,4}-\\d{1,5}(\\sCVE\\sverificable\\sen)?(\\sBOME)?"),
                  text = stringr::str_remove_all(text, "\\w{29,}"),
                  text = stringr::str_remove_all(text, "comprobada\\smediante(\\s+?CSV)?"))
}


# export_text----
# Export text
  # Export the text from a dataframe in a choosen place.
export_text <- function(mytext, where = ".") {
  future::plan(future::multisession, workers = 3)
  
  doc_alias <- 
    stringr::str_c(mytext$category, mytext$file, mytext$lang, sep = "_") %>% 
    stringr::str_c(where, ., ".txt", sep = "")
  
  furrr::future_walk2(mytext$text,
                      doc_alias,
                      ~readr::write_lines(.x, file = .y),
                      .progress = TRUE)
}
