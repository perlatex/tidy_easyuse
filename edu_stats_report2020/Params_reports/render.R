library(tidyverse)

load("../data/myData.Rdata")

df <- d %>%
  distinct(school) %>% 
  mutate(title = row_number()) %>% 
  mutate_at(vars(title), ~str_pad(., 2, pad = "0") ) 

df     

   

#######################################################
render_report = function(school, title) {
  rmarkdown::render(
    "main_reports.Rmd", 
    params = list(set_school = school),
    output_file = paste0("./output/", title, "-", school, ".docx")
  )
}

if(fs::dir_exists("output")){
  fs::dir_delete("output")
}

fs::dir_create("output")
pmap(df, render_report)
#######################################################

