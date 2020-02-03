# rmarkdown::render("main_reports.Rmd", params = list(
#   school = "scinu",
#   threshold = 10
# ))



library(tidyverse)

#######################################################
# df1 <- read_csv(here::here("data", "four_degree.csv")) %>%
#   drop_na() %>% 
#   distinct(school) %>% 
#   mutate(title = row_number())
# 
# df1     
# 
# 
# df2 <- read_csv(here::here("data", "fifth_degree.csv")) %>%
#   drop_na() %>% 
#   distinct(school) %>% 
#   mutate(title2 = 1)
# 
# df <- df1 %>% full_join(df2)

# 修改五年级的原始文件
# %s/泡小境界分校/泡桐树小学境界分校/g
# %s/实验小学青华分校/实小青华分校/g
#######################################################



#######################################################
df <- read_csv(here::here("../data", "four_degree.csv")) %>%
  drop_na() %>% 
  distinct(school) %>% 
  mutate(title = row_number())

df

render_report = function(school, title) {
  rmarkdown::render(
    "four_degree.Rmd", 
    params = list(set_school = school),
    output_file = paste0("./output/Report-", title, ".pdf")
  )
}
pmap(df, render_report)
#######################################################


rename_file <- function(title, school) {
  
  file.rename(from = file.path("./output", glue::glue("Report-{title}.pdf")), 
              to = file.path("./output", glue::glue("{school}.pdf"))
  )
  
}
purrr::walk2(df$title, df$school, rename_file)


df %>% 
  mutate_at(vars(title), ~str_pad(., 2, pad = "0") ) %>% 
  transmute(schools = glue::glue("school ==  \"{school}\"  ~ \"A{title}\",")) %>% 
  knitr::kable()


df %>% 
  mutate_at(vars(title), ~str_pad(., 2, pad = "0") ) %>% 
  mutate(title = glue::glue("A{title}")) %>% 
  write_csv("sub4_5.csv")
              