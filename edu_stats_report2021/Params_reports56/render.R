library(tidyverse)

load("../data/myData_df5.Rdata")


df <- df5_start %>%
  distinct(school) %>% 
  mutate(title = row_number()) %>% 
  mutate_at(vars(title), ~str_pad(., 2, pad = "0") )



if (fs::dir_exists("output_56")) {
  fs::dir_delete("output_56")
}
fs::dir_create("output_56")

# "草小子美分校" 只有5年级，没有6年级的数据
df_a <- df %>% 
  filter(school != "草小子美分校")

df_b <- df %>% 
  filter(school == "草小子美分校")




#######################################################
render_report_a  <- function(school, title) {
  rmarkdown::render(
    "main_reports_sequential.Rmd", 
    params = list(set_school = school),
    output_file = paste0("./output_56/", title, "-", school, ".docx")
  )
}



df_a %>% 
  pmap(render_report_a)
#######################################################






#######################################################
render_report_b <- function(school, title) {
  rmarkdown::render(
    "main_reports_diverging.Rmd", 
    params = list(set_school = school),
    output_file = paste0("./output_56/", title, "-", school, ".docx")
  )
}


df_b %>% 
  pmap(render_report_b)
#######################################################
