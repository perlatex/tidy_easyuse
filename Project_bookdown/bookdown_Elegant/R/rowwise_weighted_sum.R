library(tidyverse)

# https://stackoverflow.com/questions/43602919/weighted-mean-in-dplyr-for-multiple-columns
rowwise_weighted_sum <- function(.data, vars, weights) {
  tmp <- .data %>%
    gather(item, values, {{vars}}) %>%
    mutate(point_weighted = values * weights[item]) %>%
    group_by(id) %>% 
    summarise(sum_weight = sum(point_weighted)) 
  
  .data %>% left_join(tmp, by = "id")
}


df <- tribble(
  ~id, ~x, ~y, ~z, ~g,
  #--|--|--|--|--
  "a", 13.1, 14, 4, 1,
  "b", 15.2, 7, 5, 0,
  "c", 12.5, 10, 1, 0,
  "d", 20, 11, 3, 1
)
df

weights <- c(
  x = 0.25,
  y = 0.25,
  z = 0.25,
  g = 0.25
)

########################################################
df %>% rowwise_weighted_sum(x:y, weights)
########################################################




########################################################
fun <- function(...) {weighted.mean(..., weights)}
# 注意这个weighted.sum 的含义
df %>% mutate(wt_sum = pmap_dbl(select_if(., is.numeric), lift_vd(fun)))
########################################################










########################################################
fun <- function(...) {
    sum( c(...) * weights) / 45
  }

df_four_degree %>%
  head(5) %>% 
  mutate_at(
    vars(t37:t45),
    list(~ case_when(
      . == "A" ~ 1,
      . == "B" ~ 2,
      . == "C" ~ 3,
      . == "D" ~ 4,
      . == "E" ~ 5,
      TRUE ~ NA_real_
    ))
  ) %>%
mutate(score_rate = pmap_dbl(select(., t37:t45), lift_vd(fun))) 
########################################################





########################################################
get_score_rate <- function(.data, weights) {
  ab <- .data %>%
  gather(item, values, t37:t45) %>%
  mutate(weighted_point = values * weights[item]) %>%
  group_by(school, id) %>%
  summarise(score_rate = sum(weighted_point) / 45) 

  .data %>% left_join(ab)
}


df_four_degree %>%
  head(5) %>% 
  mutate_at(
    vars(t37:t45),
    list(~ case_when(
      . == "A" ~ 1,
      . == "B" ~ 2,
      . == "C" ~ 3,
      . == "D" ~ 4,
      . == "E" ~ 5,
      TRUE ~ NA_real_
    ))
  ) %>%
  get_score_rate(weights)
########################################################

