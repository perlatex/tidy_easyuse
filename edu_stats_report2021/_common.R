library(tidyverse)
library(stringr)





###########################################################################
parse_class_id <- function(x) {
  res <- NA_character_
  
  if ( stringr::str_length(x) < 3 ) {
    res <- x
  }
  
  if ( stringr::str_detect(x, "班$") ) {
    res <- stringr::str_extract(x, "\\d+(?=班)")
  }
  
  if ( stringr::str_detect(x, "\\.") ) {
    res <- stringr::str_extract(x, "(?<=\\.)\\d+")
  }
  
  if ( stringr::str_detect(x, "\\d{6}$") ) {
    res <- stringr::str_extract(x, "\\d{2}$")
  }
  
  if ( stringr::str_detect(x, "\\d{4}$") ) {
    res <- stringr::str_extract(x, "\\d{2}$")
  }
  
  res <- stringr::str_pad(res, width = 2, side = "left", pad = "0")
  return(res)
}
###########################################################################








###########################################################################
##### 打印出表格，高于区均值的要加背景色
colwise_color_fun <- function(x) {
  col <- character(length = length(x))
  col[] <- "transparent"
  col[x > x[length(x)]] <- "#F6A173"  # 因为区均值放在最后一行的
  col
}

flextable_print <- function(.data, cwidth = 1.3) {
  if (ncol(.data) > 4) {
    cwidth <- 0.9
  }
  
  .data %>%
    flextable::flextable(cwidth = cwidth) %>%
    bg(
      j = 2:ncol(.data),
      bg = colwise_color_fun
    ) %>%
    color(i = nrow(.data),           # 因为区均值放在最后一行的
          color = "red"
    ) %>% 
    align_nottext_col(align = "center") 
  
}
###########################################################################





###########################################################################
# 统计向量中不同分类的占比，然后横着放，主要是为了方便summarise成一行
tabyl_fun <- function(x) {
  janitor::tabyl(x) %>%
    select(x, percent) %>%
    pivot_wider(
      names_from = "x",
      names_prefix = "level_",
      values_from = "percent"
    ) 
  
}
###########################################################################