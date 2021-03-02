library(tidyverse)

load("../data/myData_df5.Rdata")


df <- df5_start %>%
  distinct(school) %>% 
  mutate(title = row_number()) %>% 
  mutate_at(vars(title), ~str_pad(., 2, pad = "0") ) 

df

#######################################################
render_report = function(school, title) {
  rmarkdown::render(
    "main_reports.Rmd", 
    params = list(set_school = school),
    output_file = paste0("./output_5/", title, "-", school, ".docx")
  )
}

if (fs::dir_exists("output_5")) {
  fs::dir_delete("output_5")
}

fs::dir_create("output_5")
pmap(df, render_report)
#######################################################

