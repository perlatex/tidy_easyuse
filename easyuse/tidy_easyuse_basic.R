library(tidyverse)

library(easyuse)
data(chengdu)

chengdu %>%
  get_ran_vals(.var_school= school, 
               .var_class = class, 
               .var_score_pre = score_pre, 
               .var_score_post = score_post, 
               "class")
