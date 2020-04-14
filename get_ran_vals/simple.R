library(tidyverse)
library(readxl)
library(fs)
library(easyuse) # devtools::install_github("perlatex/easyuse")


dir_ls(path = "data", glob = "*.xlsx") %>%
  walk(
    ~ read_excel(.) %>%
      get_ran_vals(
        .var_school = school,
        .var_class = bj,
        .var_score_pre = score_pre,
        .var_score_post = score_after,
        effects = "class"
      ) %>%
      write_excel_csv(
        paste0(stringr::str_extract(.x, "(?<=data/).*?(?=\\.xlsx)"), "_class", ".csv")
      )
  )


dir_ls(path = "data", glob = "*.xlsx") %>%
  walk(
    ~ read_excel(.) %>%
      get_ran_vals(
        .var_school = school,
        .var_class = bj,
        .var_score_pre = score_pre,
        .var_score_post = score_after,
        effects = "school"
      ) %>%
      write_excel_csv(
        paste0(stringr::str_extract(.x, "(?<=data/).*?(?=\\.xlsx)"), "_school", ".csv")
      )
  )
