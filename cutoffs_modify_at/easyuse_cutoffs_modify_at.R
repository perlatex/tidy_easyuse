library(tidyverse)
library(easyuse)


df <- tribble(
  ~id, ~x, ~y, ~z, ~g,
  #--|--|--|--|--
  "a", 13.1, 14, 4, 1,
  "b", 15.2, 7, 5, 0,
  "c", 12.5, 10, 1, 0,
  "d", 20, 11, 3, 1
)
df



cutoffs <- list(
  x = 13,
  y = 12,
  z = 3,
  g = 1
)



df %>%
  cutoffs_modify_at(.vars = c("x", "y", "z"), cutoffs = cutoffs)

df %>%
  cutoffs_modify_at(.vars = c("x", "y", "z"), cutoffs = c(x = 13, y = 12, z = 3))

df %>% 
  cutoffs_modify_at(c(x), cutoffs = c(x = 13))

df %>% 
  cutoffs_modify_at(x, cutoffs = c(x = 13))

df %>%
  cutoffs_modify_at(.vars = c(x, y), cutoffs = cutoffs)

df %>%
  cutoffs_modify_at(.vars = c(x, y, z), cutoffs = cutoffs)

df %>%
  cutoffs_modify_at(.vars = c(x, y, starts_with("z")), cutoffs = cutoffs)

df %>%
  cutoffs_modify_at(.vars = x:z, cutoffs = cutoffs)

df %>%
  cutoffs_modify_at(.vars = -id, cutoffs = cutoffs)

df %>%
  cutoffs_modify_at(.vars = c(x, y, ends_with("z")), cutoffs = cutoffs)

df %>%
  cutoffs_modify_at(.vars = c(x, y, starts_with("z")), cutoffs = cutoffs)

df %>%
  cutoffs_modify_at(.vars = c(x, y, contains("z")), cutoffs = cutoffs)

df %>%
  cutoffs_modify_at(.vars = c(x, y, matches("z")), cutoffs = cutoffs)

df %>%
  cutoffs_modify_at(.vars = one_of("x", "y", "z"), cutoffs = cutoffs)

df %>%
  cutoffs_modify_at(.vars = c("x", "y", "z"), cutoffs = cutoffs)



