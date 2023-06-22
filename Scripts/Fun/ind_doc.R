# Import
modules::import("dplyr")
modules::import("ggplot2")
modules::import("tidyr")
modules::import("stringr")
modules::import("ggwordcloud")
modules::import("DescTools")
modules::import("ggpage")
modules::import("quanteda")
modules::import("patchwork")
modules::import("tidytext")

# Export
modules::export("get_gini_doc", "viz_ind_doc")


# Get gini doc
get_gini_doc <- function(t_gamma){
  t_gamma %>% 
    group_by(document) %>% 
    summarise(gini = Gini(gamma)) %>% 
    arrange(gini) %>% 
    ungroup()
}

# Full viz
viz_ind_doc <- function(gini_doc,id, mydfm, mystm, tidy_beta, mytext, tidy_gamma, pose = TRUE, max_size = 24){
  
  even_document_term_frame <- get_even_document(gini_doc, id, mydfm, mystm, tidy_beta, pose = pose)
  
  viz_core(mytext, tidy_gamma, even_document_term_frame, max_size = max_size)
  
}

# Get even document
get_even_document <- function(gini_doc, id, mydfm, mystm, tidy_beta, pose = TRUE){
  
  if (pose){
    
    even_id <- 
      gini_doc %>% 
      filter(row_number() == id) %>% 
      pull(document)
  
  } else {
    
    even_id <- id
    
  }
  
  even_document <- mystm$meta$file[even_id]
  
  even_document_vocab <- 
    mydfm %>% 
    tidy() %>% 
    filter(str_detect(document, even_document)) %>% 
    pull(term)
  

  final_result <- 
    tidy_beta %>% 
    filter(term %in% even_document_vocab) %>% 
    group_by(term) %>% 
    top_n(beta, n = 1) %>% 
    ungroup()
  
  list(final_result, even_document, even_id)
}

# Viz
viz_core <- function(mytext, tidy_gamma, even_document_term_frame, max_size){
  
  usual_color <-c("#F8766D", "#00BA38", "#619CFF")
  
  # ggpage
  page_graph <-
    mytext %>% 
    tibble() %>% 
    select(doc_id, text) %>% 
    filter(str_detect(doc_id, even_document_term_frame[[2]])) %>% 
    unnest_tokens("text", "text", "lines") %>%
    # mutate(text = str_flatten(word)) %>% 
    ggpage_build(ncol = 2) %>% 
    filter(page <= 4) %>% 
    left_join(even_document_term_frame[[1]], by = join_by(word == term)) %>% 
    ggpage_plot(aes(fill = theme), paper.color = "white", paper.show = TRUE) +
    theme(legend.position = "none") +
    scale_fill_manual(values = usual_color)
  
  # Pie chart
  pie_graph <-
    tidy_gamma %>% 
    filter(document == even_document_term_frame[[3]]) %>% 
    ggplot(aes("", gamma, fill = theme)) +
    geom_col(color = "black", alpha = 0.7, position = "stack") +
    coord_polar(theta = "y") +
    scale_y_continuous(labels = NULL) +
    geom_label(aes(label = scales:::percent(gamma, 0.1)), position = position_stack(0.5), size = 2) +
    labs(x = NULL, y = NULL, fill = "Frame") +
    theme_void() +
    theme(legend.position = "none") +
    scale_fill_manual(values = usual_color)
  
  # Wordcloud
  set.seed(7)
  wordcloUd_graph <- 
    even_document_term_frame[[1]] %>% 
    group_by(theme) %>% 
    top_n(15, beta) %>% 
    mutate(beta = beta/sum(beta)) %>% 
    ungroup() %>% 
    arrange(-beta) %>%
    ggplot(aes(x = theme, label = term, size = beta, color = theme)) +
    geom_text_wordcloud_area() +
    scale_size_area(max_size = max_size) +
    theme_minimal() +
    scale_color_manual(values = usual_color) +
    labs(x = "Frames")
  
  list(wordcloUd_graph = wordcloUd_graph,
       pie_graph = pie_graph,
       page_graph = page_graph,
       param = even_document_term_frame)
  
}