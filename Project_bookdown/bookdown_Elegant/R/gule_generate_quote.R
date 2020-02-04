library(tidyverse)

df <- read_csv(here::here("data", "four_degree.csv")) %>%
  drop_na() %>% 
  distinct(school) %>% 
  mutate(title = row_number())

df




df %>% 
  transmute(schools = glue::glue("school ==  \"{school}\"  ~ \"学校{title}\",")) %>% 
  knitr::kable()

df %>% mutate(title = glue::glue("学校{title}"))
              
