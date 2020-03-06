######################################################################
# usage
# df_four_degree %>% make_plot(vars = t4) +
# df_four_degree %>% make_plot(vars = t5, xjust = TRUE)

make_plot <- function(df, vars, flip = FALSE, xjust = FALSE, swatcher = FALSE) {
  
  library(ggthemr)

  if (swatcher) {
     ugly <- define_palette(
     swatch = c("#555555", 
                "#db735c", "#EFA86E", "#9A8A76", "#F3C57B", "#7A6752", 
                "#2A91A2", "#F7B32B", "#6EDCEF", "#ff935c", "#dc5353", "#FBB13C"),
     gradient = c(lower = 'red', upper = 'green')
    )
     ggthemr(ugly)
   } else {
     ggthemr('dust')
}

  # https://tidyeval.tidyverse.org/dplyr.html  
  simple_var <- as_label(enquo(vars))
  
  labels <- questionnaire_labels %>% 
    filter(index ==  simple_var) %>% 
    select(option, ssn) %>% 
    tibble::deframe()
  
  title <- questionnaire_labels %>%  
    filter(index ==  simple_var) %>% 
    distinct(name) %>% 
    pull(name) 
  
  addline_format <- function(x,...){
    gsub('CR','\n', x)
  }
  
  
  p <- df %>%
    select({{ vars }}) %>%
    ggplot(aes(x = {{ vars }}, 
               y = stat(count) / sum(stat(count)), 
               fill = factor({{ vars }})
    )) +
    geom_bar(width = 0.6)
  
  if (flip) {
    p <- p + geom_text(aes(label = scales::percent( stat(count) / sum(stat(count)) , accuracy = .01)), stat = "count", hjust = 1, color = "white") +
      coord_flip()
  } else {
    p <- p + geom_text(aes(label = scales::percent(stat(count) / sum(stat(count)), accuracy = .01)), stat = "count", vjust = -0.25)
  }
  
  p <- p +
    scale_x_discrete(labels = addline_format(labels)) +
    scale_y_continuous(
      expand = c(0.1, 0),
      labels = scales::percent_format(accuracy = 1L)) +
    labs(
      x = NULL, y = NULL,
      title = "", subtitle = title
    ) + {
      if (xjust) theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } +
    report_theme
  
  p
}
######################################################################







######################################################################
correlate_plot <- function(df, vars, score, xjust = FALSE) {
  
  library(ggthemr)
  ggthemr('dust')

  # https://tidyeval.tidyverse.org/dplyr.html  
  simple_var <- as_label(enquo(vars))
  
  labels <- questionnaire_labels %>% 
    filter(index ==  simple_var) %>% 
    select(option, ss) %>% 
    tibble::deframe()
  
  titles <- questionnaire_labels %>%  
    filter(index ==  simple_var) %>% 
    distinct(name) %>% 
    pull(name) 
  
  #titles <- glue::glue("{title}与考试成绩的关联")
  titles <- paste0(titles, "与考试成绩的关联")
  
  p <- df %>%
    select({{vars}}, {{score}}) %>% 
    group_by({{vars}}) %>% 
    summarise(mean_score = mean({{score}})) %>%
    ggplot(aes(x = {{vars}}, y = mean_score)) +
    geom_col(fill = "#008148", width = 0.8) +
    geom_text(aes(label = round(mean_score, 2), vjust = -0.25)) +
    scale_x_discrete(labels = labels) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(x = NULL, y = NULL,
         title = titles, subtitle = "数据来源问卷调查") +
    { 
      if (xjust) theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    } +
    report_theme 
  
  
  p
  
}
######################################################################







######################################################################
# usage
# fig.height= 13, fig.width=8
# df_complete0 %>% group_plot(vars = t33,  xjust = T)

group_plot <- function(df, vars, xjust = FALSE) {
  
  library(ggthemr)
  ggthemr('solarized')

  # https://tidyeval.tidyverse.org/dplyr.html  
  simple_var <- as_label(enquo(vars))
  
  labels <- questionnaire_labels %>% 
    filter(index ==  simple_var) %>% 
    select(option, ssn) %>% 
    tibble::deframe()
  
  title <- questionnaire_labels %>%  
    filter(index ==  simple_var) %>% 
    distinct(name) %>% 
    pull(name) 
  
 
  addline_format <- function(x,...){
    gsub('CR','\n', x)
  }
  
  
  p <- df %>%
    select(school, {{vars}}) %>%
    gather(index, option, -school) %>%
    group_by(school, index) %>%
    count(option) %>%
    mutate(percent = n / sum(n)) %>%
    group_by(option) %>% 
    mutate(above_mean = percent > mean(percent)) %>% 
    ggplot(aes(x = option, y = school, size = percent)) +
    geom_point(aes(color = above_mean) ) +
    geom_text(aes(label = scales::percent(percent, accuracy = .01)),
              color = "white", 
              size = 3) +
    scale_x_discrete(labels = addline_format(labels) ) +
    scale_size_continuous(range = c(6, 15)) +
    labs(
      x = NULL, y = NULL,
      title = glue::glue("各校学生主观学习{title}")#,
      #subtitle = "数据来源问卷调查: 红色代表高于平均分，蓝色代表低于平均分"
    ) +
    { 
      if (xjust) theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    } +
    report_theme 
  
  p
}
######################################################################






######################################################################
# usage
# fig.height= 6, fig.width=6
# df_complete0 %>% 
#  correlate2_plot(vars1 = t33, vars2 = t34, score = test, xjust = T)

correlate2_plot <- function(df, vars1, vars2, score, xjust = FALSE) {
  
  library(ggthemr)
  ggthemr('solarized')

  # https://tidyeval.tidyverse.org/dplyr.html  
  simple_var1 <- as_label(enquo(vars1))
  simple_var2 <- as_label(enquo(vars2))
  
  
  labels_x <- questionnaire_labels %>% 
    filter(index ==  simple_var1) %>% 
    select(option, ssn) %>% 
    tibble::deframe()
  
  labels_y <- questionnaire_labels %>% 
    filter(index == simple_var2) %>% 
    select(option, ssn) %>% 
    tibble::deframe()
  
  title_1 <- questionnaire_labels %>%  
    filter(index ==  simple_var1) %>% 
    distinct(name) %>% 
    pull(name) 
  
  title_2 <- questionnaire_labels %>%  
    filter(index ==  simple_var2) %>% 
    distinct(name) %>% 
    pull(name) 
  
  title <- glue::glue("{title_1}与{title_2}对应下的平均成绩")
  
    addline_format <- function(x,...){
    gsub('CR','\n', x)
  }
  
    
    
  p <- df %>% 
    select({{score}}, {{vars1}}, {{vars2}}) %>% 
    mutate(mean_score_globle = mean({{score}}, na.rm = T)) %>% 
    group_by({{vars1}}, {{vars2}}) %>% 
    summarise(
      grid_number = n(),
      mean_score = mean({{score}}, na.rm = T),
      mean_score_globle = mean(mean_score_globle, na.rm = T)
      ) %>%
    ungroup() %>%
    mutate(grid_percent = grid_number / sum(grid_number)) %>%
    ggplot(aes(x = {{vars1}}, y = {{vars2}})) +
    geom_point(aes(size = grid_percent,
                   color = mean_score > mean_score_globle
                   )
    ) +
    geom_text(aes(label = round(mean_score, 2)), size = 3, color = "white") +
    scale_x_discrete(labels = addline_format(labels_x) ) +
    scale_y_discrete(labels = addline_format(labels_y) ) +
    scale_size_continuous(range = c(10, 20)) +
    #scale_color_gradient(low = "gray", high = "tomato") +
    labs(
      x = NULL, y = NULL,
      subtitle = title,
      #subtitle = "数据来源问卷调查: 红色代表高于平均分，蓝色代表低于平均分"
      caption = "说明：点越大代表这种类别的人数占比越高，中间的数值代表平均成绩"
    ) +
    { 
      if (TRUE) theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
    } +
    report_theme
  
  
  p
  
}
######################################################################


