modules::import("magrittr")
modules::export("import_txt", "pos")

# Read text ----
read_txt <- function(the_doc) {
  readLines(the_doc) %>% 
    stringr::str_c(collapse = " ")
}

# Import text ----
import_txt <- function(the_path = ".", the_delim = "/") {
  the_txts <- 
    list.files(the_path,
               recursive = TRUE,
               full.names = TRUE)
  
  doc_id <- 
    list.files(the_path,
               recursive = TRUE)
  
  future::plan(future::multisession, workers = 3)
  
  content_txt <- 
    furrr::future_map_chr(the_txts, read_txt,
                          .progress = TRUE)
  
  # doc_id <- stringr::str_remove(the_txts,
  #                               stringr::str_c(the_path, "/", sep =""))
  
  dplyr::tibble(doc_id = doc_id,
                text = content_txt,
                path = doc_id) 
#     tidyr::separate_wider_delim(path, 
#                                 delim = the_delim,
#                                 names = names_vec)
}


# POS tagging ----
pos <- function(text) {
  text %>% 
    udpipe::udpipe("english-lines") %>% 
    # as.data.frame() %>% 
    tibble::tibble() %>% 
    dplyr::select(lemma, upos)
}