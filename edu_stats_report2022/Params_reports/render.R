library(tidyverse)

load("../data/myData_df5.Rdata")


school_df <- df5_start %>%
  distinct(school) %>% 
  mutate(title = row_number()) %>% 
  mutate_at(vars(title), ~str_pad(., 2, pad = "0") ) 

school_df


rm(df5_start,
   df5_scoring_rate,
   df5_all,
   df5_set,
   pairs56,
   df5_burden_percent_combine)


#######################################################
render_report = function(school, title) {
  rmarkdown::render(
    input = "section_reports.Rmd", 
    params = list(set_school = school),
    output_file = paste0("./output_X/", title, "-", school, ".pdf")
  )
}

if (fs::dir_exists("output_X")) {
  fs::dir_delete("output_X")
}

fs::dir_create("output_X")



school_df %>% 
  slice(4:6) %>% 
  pmap(render_report)


school_df %>% 
  pmap(render_report)
#######################################################

