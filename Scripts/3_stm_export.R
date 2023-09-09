# Export stm table
# TODO the *_0 versions are only for one table at the time, wheras the normal version are for multiple table see example at the end
pacman::p_load(rio, stm, quanteda, tidytext)

# SIDE FUNCTIONS ----
## Transform stm regressino to dataframe
reg_tab_0 <- function(table){
  
  table %>%
    as.data.frame() %>%
    tibble::rownames_to_column("coef") %>% 
    tibble::tibble() %>% 
    dplyr::select(coef,
           estm = Estimate,
           stder = `Std. Error`,
           p = `Pr(>|t|)`) %>% 
    dplyr::mutate(ci_low = estm - 1.96*stder,
                  ci_high = estm + 1.96*stder,
                  estm = round(estm, 2),
                  p = dplyr::case_when(
             p < 0.001 ~ "***",
             p >= 0.001 & p < 0.01 ~ "**",
             p >= 0.01 & p < 0.05 ~ "*",
             TRUE ~ ""
           ),
           dplyr::across(dplyr::starts_with("ci_"), round, 2)) %>% 
    tidyr::unite("ci", dplyr::starts_with("ci_"), sep = " | ") %>% 
    tidyr::unite("estm", c("estm", "p"), sep = "") %>%  
    dplyr::mutate(ci = stringr::str_replace(ci, "(.+)", "\\(\\1\\)")) %>% 
    tidyr::unite("estm", c("estm", "ci"), sep = "\n") %>% 
    dplyr::select(-stder)
}

## Export dataframe from stm regression to .docx
stm_export_0 <- function(stm_table, path = "", alias = "", id = "1"){
  
  df_regression <- 
    stm_table %>% 
    janitor::clean_names()
    # dplyr::mutate(type = dplyr::case_when(
    #   coef == "(Intercept)" ~ "",
    #   stringr::str_detect(coef, "\\d{4}") ~ "Year\nref: 2021",
    #   TRUE ~ "Author\nref: Ceuta"
    # ), .before = 1)
  
  tbl_regression <-
    df_regression %>% 
    # flextable::as_grouped_data(groups = "type") %>% 
    dplyr::slice(-1) %>% 
    flextable::flextable() %>% 
    flextable::merge_v(1) %>% 
    flextable::theme_box() %>% 
    flextable::align_text_col(align = "center") %>% 
    flextable::set_header_labels(values = list(type = "",
                                               coef = "variable")) %>% 
    flextable::add_header_lines(values = "QR decomposition regression") %>% 
    flextable::add_footer_lines(values = "p<0.05 ** p<0.01 *** p<0.001 ") %>% 
    flextable::autofit()
  
  if (path != ""){
    if (stringr::str_detect(path, "/$")){
      # do nothing
    } else {
      path <- stringr::str_c(path, "/")
    }
  } else if (stringr::str_c(path, alias) != ""){
    id <- stringr::str_c("_", id)
  }
  
  flextable::save_as_docx(tbl_regression,
                          path = stringr::str_c(path, alias, id, ".docx"))
}


# MAIN FUNCTIONS ----
## Transform multiple stm regressions to dataframe
reg_tab <- function(table_list){
  purrr::map(table_list, reg_tab_0)
}

## Export multiple dataframe from stm regressions to .docx
stm_export <- function(stm_table_list, path = "", alias = ""){
  
  purrr::imap(stm_table_list,
             \(x, idx) stm_export_0(stm_table = x,
                                    path = path,
                                    alias = alias,
                                    id = idx))
}

# EXAMPLES ----
## One regression table to dataframe
# stm_reg[[1]] %>% 
#   reg_tab_0()

## One regression table to dataframe to docx
# stm_reg[[1]] %>% 
#   reg_tab_0() %>% 
#   stm_export_0()

## One regression table to dataframe to docx
## With more options (saved in the "Data" folder with the name "tab")
# stm_reg[[1]] %>% 
#   reg_tab_0() %>% 
#   stm_export_0(path = "Data", "tab")

## Multiple regression tables to dataframe to docx (one file each)
## With more options (saved in the "Data" folder with the name "tab")
## File's name tab_1.docx / tab_2.docx / tab_3.docx
# stm_reg %>% 
#   reg_tab() %>% 
#   stm_export(path = "Data", "tab")