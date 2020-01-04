library(tidyverse)
# 推荐先 ?mutate_at


# define standrad function
scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)




df %>% 
  mutate_at(vars(contains("_")), as.numeric) %>%
  mutate_at(
    vars(contains("_")),
    scale2,
    na.rm = TRUE
  ) 