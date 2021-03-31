library(tidyverse)

# source(01_tidydata5.R)         # -> "data/myData_df5.Rdata"
# source(01_tidydata6.R)         # -> "data/myData_df6.Rdata"
# source(01_tidydata_anon.R)     # -> "data/myData_df56_anon.Rdata"



if (fs::file_exists("02_Rmd_officedown_sequential.docx")) {
  fs::file_delete("02_Rmd_officedown_sequential.docx")
}


rmarkdown::render("02_Rmd_officedown_sequential.Rmd",
  params = list(
    set_data_from1 = "data/myData_df5.Rdata",
    set_data_from2 = "data/myData_df6.Rdata"
  )
)
#######################################################





#######################################################
fs::file_copy("02_Rmd_officedown_sequential.Rmd",
  "02_Rmd_officedown_sequential_.Rmd",
  overwrite = TRUE
)

mytext <- read_file("02_Rmd_officedown_sequential_.Rmd")

anon_school_code <- 
  readxl::read_excel("./data/anon_code.xls") %>% 
  dplyr::select(school, code) %>% 
  dplyr::arrange(desc(code)) %>% 
  tibble::deframe() 

anon_school_code


"这是文翁实验小学，不是实验小学，因为文翁实验小学是A17， 实验小学是A07" %>% 
  str_replace_all(anon_school_code)

mytext %>% 
  str_replace_all(anon_school_code) %>% 
  write_file("02_Rmd_officedown_sequential_anon.Rmd")



rmarkdown::render("02_Rmd_officedown_sequential_anon.Rmd",
  params = list(
    set_data_from1 = "data/myData_df56_anon.Rdata",
    set_data_from2 = NULL
  )
)
#######################################################
