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
  col[] <- "white"
  col[x > x[length(x)]] <- "#dc322f"  # 因为区均值放在最后一行的
  col
}



def_bgx <- function(x) {
  cell_spec(x, bold = T,
            color = "black",
            background = colwise_color_fun(x)
  )
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




###########################################################################
# usage:
# df5_all %>%
#  correlate_plot(vars = hard_class)

correlate_plot <- function(df, vars, title = "") {
  library(ggthemr)
  ggthemr('solarized')
  
  rightlimit <- ifelse(range(df$score_test)[2] > 90, 95, 90)
  
  vars_name <- as_label(enquo(vars)) %>%
    stringr::str_replace_all(., pattern = pairs56)
  
  p <- 
    df %>%
    select(school, score_test, {{vars}} ) %>% 
    mutate(
      mean_score = score_test[school == "全区"],
      mean_rate = {{vars}}[school == "全区"]
    ) %>% 
    mutate(quadrant = case_when(
      score_test >= mean_score & {{vars}} >=  mean_rate  ~ "d1",
      score_test >= mean_score & {{vars}} <   mean_rate  ~ "d2",
      score_test <  mean_score & {{vars}} >=  mean_rate  ~ "d3",
      score_test <  mean_score & {{vars}} <   mean_rate  ~ "d4",
      TRUE ~  "other")
    ) %>%
    filter(school != "全区") %>% 
    ggplot(aes(x = {{vars}}, y = score_test, color = quadrant)) +
    geom_point(size = 5) +
    ggrepel::geom_text_repel(aes(label = school), size = 3) +
    geom_hline(aes(yintercept = unique(mean_score))) +
    geom_vline(aes(xintercept = unique(mean_rate))) +
    scale_y_continuous(limits = c(NA, rightlimit), 
                       expand = expansion(mult = c(0.05, 0))
    ) + 
    labs(x = glue::glue("{vars_name}得分率"),
         y = "学生平均成绩",
         title = glue::glue("{vars_name}得分率与学生成绩的关联({title})")
    ) +
    theme(legend.position = "none") 
  
  #ggthemr_reset()
  
  p
}

###########################################################################



###########################################################################
# usage:

# num_in_quadrant <- df5_all %>%
#   calc_num_in_quadrant(vars = hard_class) %>% 
#   pull(n)
# num_in_quadrant[1]

calc_num_in_quadrant <- function(df, vars) {
  
  df %>%
    select(school, score_test, {{vars}} ) %>% 
    mutate(
      mean_score = score_test[school == "全区"],
      mean_rate = {{vars}}[school == "全区"]
    ) %>% 
    mutate(quadrant = case_when(
      score_test >= mean_score & {{vars}} >= mean_rate  ~ "d1",
      score_test >= mean_score & {{vars}} <  mean_rate  ~ "d2",
      score_test <  mean_score & {{vars}} >= mean_rate  ~ "d3",
      score_test <  mean_score & {{vars}} <  mean_rate  ~ "d4",
      TRUE ~  "other")
    ) %>%
    filter(school != "全区") %>% 
    count(quadrant)
  
}

###########################################################################






###########################################################################
# usage:
# df5_all %>%
#  tidy_evaluate(.schoolname = cur_schoolname, name_pairs = pairs56)

tidy_evaluate <- function(.data, .schoolname, name_pairs) {
# name_pairs is named vector from external, e.g.
# name_pairs = pairs56
# pairs56 <- 
#  readxl::read_excel("./data/pairs.xlsx") %>%
#  tibble::deframe()
# mutate(index_cn = recode(index, !!!name_pairs)
  
  .data %>%
    filter(school == .schoolname | level == "district") %>% 
    select(-school, -class_id, -num_effect) %>% 
    
    tidyr::pivot_longer(
      cols = -level,
      names_to = "index",
      values_to = "value"
    ) %>%
    tidyr::pivot_wider(
      names_from = level,
      values_from = value
    ) %>% 
    mutate(index_cn = recode(index, !!!name_pairs), .after = index) %>%
    mutate(
      across(school, scales::label_number(suffix = "%", accuracy = 0.01), .names = "{.col}_"),
      glue_data = glue::glue("{index_cn}{school_}")
    ) %>% 
    mutate(predicate = case_when(
      str_detect(index, "^percent_") ~ school > district,
      str_detect(index, "^f_") ~ school > 80,
      TRUE ~ NA
    )) 
  
}
###########################################################################
