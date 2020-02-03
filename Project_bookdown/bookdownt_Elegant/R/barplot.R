######################################################################
# usage
# df_four_degree %>% bar_plot(vars = t4) +
# df_four_degree %>% bar_plot(vars = t5, xjust = TRUE)

bar_plot <- function(vars, var_name) {
  
  labels <- questionnaire_labels %>% 
    filter(index ==  var_name) %>% 
    select(option, ssn) %>% 
    tibble::deframe()
  
  title <- questionnaire_labels %>%  
    filter(index ==  var_name) %>% 
    distinct(name) %>% 
    pull(name) 
  
  addline_format <- function(x,...){
    gsub('CR','\n', x)
  }
  
  
  p <- data.frame(x =  vars) %>% 
    ggplot(aes(x =  vars, 
               y = stat(count) / sum(stat(count)), 
               fill = factor(vars)
    )) +
    geom_bar(width = 0.6) +
    geom_text(
      aes(label = scales::percent( stat(count) / sum(stat(count)), accuracy = .01)), 
      stat = "count", 
      vjust = -0.25) +
    scale_x_discrete(labels = addline_format(labels)) +
    scale_y_continuous(
      expand = c(0.1, 0),
      labels = scales::percent_format(accuracy = 1L)) +
    labs(
      x = NULL, y = NULL,
      title = title, subtitle = "数据来源问卷调查"
    ) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") 
  
  
}

######################################################################


