modules::import("magrittr")
modules::export("rep_ngram")


rep_ngram <- function(df, ngram_min = 2, ngram_max = 100, val_first = 300, val_last = 1) {
  future::plan(future::multisession, workers = 3)
  
  n_gram <- seq(ngram_min, ngram_max)
  max_val <-
    seq(val_first, val_last,
        length.out = length(n_gram)) %>%
    round()
  
  furrr::future_map2(n_gram,
                         max_val,
                         word_vec,
                         df = df,
                         .progress = TRUE) %>% 
    purrr::flatten_chr()
}


word_vec <- function(n_gram, max_val, df) {
  df %>% 
    dplyr::select(text) %>% 
    tidytext::unnest_ngrams("words", "text", n = n_gram) %>% 
    dplyr::count(words, sort = TRUE) %>% 
    dplyr::filter(n > max_val) %>% 
    dplyr::pull(words)
}