library(tidyverse)
library(easyuse) # devtools::install_github("perlatex/easyuse")


df <- tribble(
  ~id, ~x, ~y, ~z, ~g,
  #--|--|--|--|--
  "a", 13.1, 14, 4, 1,
  "b", 11.2, 7, 5, 0,
  "c", 12.5, 10, 1, 0,
  "d", 20, 11, 3, 1
)


weights <- c(
  x = 0.25,
  y = 0.25,
  z = 0.25,
  g = 0.25
)

df %>% add_weighted_mean(x:g, .name = "wt_mean", .weights = weights)



iris %>%
  add_row_sums(starts_with("Sepal"), .name = "Sepal.Mean")

iris %>%
  add_row_means(starts_with("Sepal"), .name = "Sepal.Mean")

df
df %>% add_row_means(x:g, .name = "Sepal.Mean")
df %>% add_row_sums(x:g, .name = "Sepal.sum")

df %>% add_weighted_mean(x:g, .weights = weights)
df %>% add_weighted_mean(x:g, .name = "wt_mean", .weights = weights)


df %>% add_above_avg_num(x:g)
