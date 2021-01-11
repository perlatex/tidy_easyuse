library(tidyverse)


###############################################################
# 读取整理好的数据，(boy--理科； girl--文科)
data_boy  <- read_rds(here::here("data", "data_boy.rds"))
data_girl <- read_rds(here::here("data", "data_girl.rds"))
###############################################################





###############################################################
# 检查重复的人中 有多少是姓名的/? (因为有姓名的学生，是最后考察的)
duplicate_ID_in_boy <- data_boy %>% 
  janitor::get_dupes(ID)
duplicate_ID_in_boy


duplicate_ID_in_girl <- data_girl %>% 
  janitor::get_dupes(ID)
duplicate_ID_in_girl


data_boy %>%
  tidyr::drop_na(ID) %>%
  dplyr::distinct(ID, .keep_all = TRUE) %>%
  filter(!is.na(name))  %>% 
  count(school, class)

data_girl %>%
  tidyr::drop_na(ID) %>%
  dplyr::distinct(ID, .keep_all = TRUE) %>%
  filter(!is.na(name))  %>% 
  count(school, class)
###############################################################






###############################################################
# usage:
#   data_boy %>%
#     get_discipline_SGP(.discipline = "chinese")
#
# 第一个参数是数据框，并且对变量名有一定要求：
#   1. 必须包含("ID", "语文成绩_2", "数学成绩_2", "语文成绩_1", "数学成绩_1")
#   2. 可以包括("姓名", "学校", "班级")
# 第二个参数，是指定学科，比如 "chinese"
# 程序会自动完成ID去重，只保留重复ID中第一次出现的那一行

get_discipline_SGP <- function(.data, .discipline) {
  
  require(dplyr)
  require(SGP)
  
  if (any(c("ID", "id") %in% colnames(.data))) {

  } else {
    abort(
      glue::glue(".data must have variable ID or id.")
    )
  }


  current_discipline <- paste0(.discipline, "_", 1:2)

  df_prepare <- .data %>%
    dplyr::mutate(
      grade_1 = 1L,
      grade_2 = 2L
    ) %>%
    dplyr::mutate(ID = stringr::str_trim(ID, side = "both")) %>%
    dplyr::select("ID", any_of(c("name", "school", "class")), "grade_1", "grade_2", all_of(current_discipline)) %>%
    tidyr::drop_na(ID) %>%
    dplyr::distinct(ID, .keep_all = TRUE) %>%
    dplyr::mutate(
      across(all_of(current_discipline), as.numeric)
    )


  eff_nrows <- df_prepare %>%
    dplyr::n_distinct(vars(ID)) 

  message(
    glue::glue("there are {eff_nrows} row in {.discipline}.")
  )

  df_SGP <- df_prepare %>%
    SGP::studentGrowthPercentiles(
      panel.data = .,
      sgp.labels = list(my.year = 2020, my.subject = "chengdu"),
      panel.data.vnames = c("ID", "grade_1", "grade_2", current_discipline),
      grade.progression = 1:2
    ) %>%
    purrr::pluck("SGPercentiles") %>%
    purrr::pluck("CHENGDU.2020") %>%
    tibble::as_tibble()


  df_output <- df_prepare %>%
    dplyr::left_join(
      df_SGP %>% dplyr::select(ID, SGP),
      by = "ID"
    ) %>%
    dplyr::mutate(discipline = .discipline, .before = 1) %>% 
    relocate(SGP, .before = grade_1)

  return(df_output)
}
###############################################################







###############################################################
# 
data_boy %>%
  get_discipline_SGP(.discipline = "chinese")

data_boy %>%
  get_discipline_SGP(.discipline = "mathematics")

data_boy %>%
  get_discipline_SGP(.discipline = "mathematics")

data_boy %>%
  get_discipline_SGP(.discipline = "chinese") %>%
  filter(!is.na(name))
###############################################################






###############################################################
# 各学科依次计算student growth percentiles(SGP)，
# 并保存到data/文件中，目的是方便下次使用
# 因此, 不用每次都从这里开始

big_result_boy <-
  c(
    "chinese", "mathematics", "english", 
    "physics", "chemistry", "biology", 
    "natural", "total"
    ) %>%
  map_df(
    ~ get_discipline_SGP(data_boy, .discipline = .x)
  )

big_result_boy %>% saveRDS(here::here("data", "big_result_boy.rds"))






big_result_girl <-
  c(
    "chinese", "mathematics", "english",
    "politics", "history", "geography",
    "social", "total"
  ) %>%
  map_df(
    ~ get_discipline_SGP(data_girl, .discipline = .x)
  )


big_result_girl %>% saveRDS(here::here("data", "big_result_girl.rds"))
###############################################################

