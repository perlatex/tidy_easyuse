library(tidyverse)


###############################################################
# 从excel中读入数据
# 读取的数据，保存数据data/文件夹的*.rds，方便后续使用
# 因此, 不用每次都从这里开始
# (boy--理科； girl--文科)

data_boy <-
  readxl::read_excel("./rawdata/data20210109.xls", sheet = 1, skip = 1) %>%
  purrr::set_names(
    c(
      "name", "ID", "school", "class",
      "chinese_2", "mathematics_2", "english_2", "physics_2", "chemistry_2", "biology_2", "natural_2", "total_2",
      "chinese_1", "mathematics_1", "english_1", "physics_1", "chemistry_1", "biology_1", "natural_1", "total_1"
    )
  )



data_girl <-
  readxl::read_excel("./rawdata/data20210109.xls", sheet = 2, skip = 3) %>%
  purrr::set_names(
    c(
      "name", "ID", "school", "class",
      "chinese_2", "mathematics_2", "english_2", "politics_2", "history_2", "geography_2", "social_2", "total_2",
      "chinese_1", "mathematics_1", "english_1", "politics_1", "history_1", "geography_1", "social_1", "total_1"
    )
  )

data_boy  %>% saveRDS(here::here("data", "data_boy.rds"))
data_girl %>% saveRDS(here::here("data", "data_girl.rds"))
###############################################################




