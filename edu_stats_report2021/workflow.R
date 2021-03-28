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


fs::file_copy("02_Rmd_officedown_sequential.Rmd",
  "02_Rmd_officedown_sequential_anon.Rmd",
  overwrite = TRUE
)

rmarkdown::render("02_Rmd_officedown_sequential_anon.Rmd",
  params = list(
    set_data_from1 = "data/myData_df56_anon.Rdata",
    set_data_from2 = NULL
  )
)
#######################################################
