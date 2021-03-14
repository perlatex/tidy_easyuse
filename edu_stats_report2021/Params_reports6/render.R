library(tidyverse)

load("../data/myData_df6.Rdata")


df <- df6_start %>%
  distinct(school) %>% 
  mutate(title = row_number()) %>% 
  mutate_at(vars(title), ~str_pad(., 2, pad = "0") ) 

df

#######################################################
render_report = function(school, title) {
  rmarkdown::render(
    "main_reports_diverging.Rmd", 
    params = list(set_school = school),
    output_file = paste0("./output_6/", title, "-", school, ".docx")
  )
}

if (fs::dir_exists("output_6")) {
  fs::dir_delete("output_6")
}

fs::dir_create("output_6")
pmap(df, render_report)
#######################################################

