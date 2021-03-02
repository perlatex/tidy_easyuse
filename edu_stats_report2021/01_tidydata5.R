library(tidyverse)
library(stringr)


## 定义的函数
source("_common.R", encoding = "UTF-8")
#load("data/myData_df5.Rdata")




#########################################################
## 读取数据
raw_d5 <- readxl::read_excel("./data/rawdata_5.xlsx")


d5 <- raw_d5 %>%
  select(city, district, school, class, discipline, 
         name, pid, score, starts_with("t")
  ) %>%
  janitor::clean_names() 
d5


# check_class_id <- d5 %>% 
#   distinct(class) %>% 
#   mutate(
#     class_id = map_chr(class, ~parse_class_id(.)),
#     .after = class
#   )
# check_class_id


df5_start <- d5 %>% 
  mutate(
    class_id = map_chr(class, ~parse_class_id(.)),
    .after = class
  ) %>%
  mutate(
    across(score, as.numeric)
  ) %>% 
  filter(if_all(everything(), ~!is.na(.)))
#########################################################





#########################################################
#以下是测试用，不用执行
df5_start %>% 
  filter(if_any(everything(), is.na))


d5 %>% 
  filter(if_all(everything(), ~!is.na(.)))


tt <- d5 %>%
  drop_na() %>% 
  mutate(
    score_1 = as.numeric(score),
    .after = score
  ) %>%
  filter(if_any(everything(), is.na))
#########################################################









######################################################################
## 计算得分率
#- 转换为数值
#- 翻转反向计分
#- 行方向求均值，求得分率

df5_scoring_rate <- df5_start %>%
  mutate(
    across(c(t11_1:t19_7), as.numeric)
  ) %>% 
  mutate(
    across(c("t14_2", "t14_4", "t15_1", "t15_3"), ~ 5 - .x)
  ) %>% 
  rowwise() %>%
  mutate(
    hard_class        = 1 - t11_1 / 5,
    hard_homework     = 1 - t11_2 / 5,
    hard_test         = 1 - t11_3 / 5,
    
    f_internal_driven   = mean(c_across(t13_1:t13_5)) / 4,
    f_external_driven   = mean(c_across(t13_6:t13_7)) / 4,
    f_learning_power    = mean(c_across(starts_with("t14_"))) / 4,
    f_learning_value    = mean(c_across(starts_with("t15_"))) / 4,
    f_activity_inclass  = mean(c_across(starts_with("t16_"))) / 4,
    
    f_knowledge_memory = mean(c_across(t17_1:t17_2)) / 4,
    f_knowledge_mastery = mean(c_across(t17_3:t17_5)) / 4,
    f_knowledge_apply = mean(c_across(t17_6:t17_7)) / 4,
    f_knowledge_creative = mean(c_across(t17_8:t17_10)) / 4,
    
    f_learning_strategy = mean(c_across(starts_with("t18_"))) / 4,
    
    f_feeling_class_negative  = t19_1 / 4,
    f_feeling_class_positive  = t19_2 / 4,
    
    f_feeling_test_negative  = t19_3 / 4,
    f_feeling_test_positive  = t19_4 / 4,
    
    f_feeling_homework_negative  = mean(c_across(c("t19_5", "t19_7"))) / 4,
    f_feeling_homework_positive  = t19_6 / 4
    
    
  ) %>% 
   ungroup() 
######################################################################   
    




######################################################################
## 计算占比
## 用_common.R中tabyl_fun函数，统计作业难度等级(1-5等)的各占比，后要unpack下
## 统计各科老师拖课的比例, (0/1)
## 计算各得分率的均值
df5_mean_district <- df5_scoring_rate %>%
  summarise(
    effect_num = n(), 
    test_score = mean(score), 
    
    percent_sleep_time     = sum(t3 == "A") / n(),
    percent_homework_time  = sum(t4 %in% c("A", "B")) / n(),
    percent_music          = sum(t5 == "A") / n(),
    percent_art            = sum(t6 == "A") / n(),
    percent_sport          = sum(t7 == "A") / n(),
    percent_hours_exercise = sum(t8 == "A") / n(),
    percent_test_score     = sum(t9 %in% c("A", "B")) / n(),
    percent_score_rank     = sum(t10 == "B") / n(),
    
    across(starts_with("t12_"), ~ sum(.x)/n(), .names = "teacher_{.col}"),
    across(starts_with("f_"), mean, na.rm = TRUE),
    across(starts_with("hard_"), mean, na.rm = TRUE)
  ) %>%
  mutate(school = "全区", group = "district") %>%
  relocate(school, group)
df5_mean_district





df5_mean_school <- df5_scoring_rate %>%
  group_by(school) %>%
  summarise(
    effect_num = n(),
    test_score = mean(score), 
    
    percent_sleep_time     = sum(t3 == "A") / n(),
    percent_homework_time  = sum(t4 %in% c("A", "B")) / n(),
    percent_music          = sum(t5 == "A") / n(),
    percent_art            = sum(t6 == "A") / n(),
    percent_sport          = sum(t7 == "A") / n(),
    percent_hours_exercise = sum(t8 == "A") / n(),
    percent_test_score     = sum(t9 %in% c("A", "B")) / n(),
    percent_score_rank     = sum(t10 == "B") / n(),
    
    across(starts_with("t12_"), ~ sum(.x)/n(), .names = "teacher_{.col}"),
    across(starts_with("f_"), mean, na.rm = TRUE),
    across(starts_with("hard_"), mean, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(group = "school") %>%
  relocate(school, group)
df5_mean_school



df5_mean_school_class <- df5_scoring_rate %>%
  group_by(school, class_id) %>%
  summarise(
    effect_num = n(),
    test_score = mean(score), 
    
    percent_sleep_time     = sum(t3 == "A") / n(),
    percent_homework_time  = sum(t4 %in% c("A", "B")) / n(),
    percent_music          = sum(t5 == "A") / n(),
    percent_art            = sum(t6 == "A") / n(),
    percent_sport          = sum(t7 == "A") / n(),
    percent_hours_exercise = sum(t8 == "A") / n(),
    percent_test_score     = sum(t9 %in% c("A", "B")) / n(),
    percent_score_rank     = sum(t10 == "B") / n(),
    
    across(starts_with("t12_"), ~ sum(.x)/n(), .names = "teacher_{.col}"),
    across(starts_with("f_"), mean, na.rm = TRUE),
    across(starts_with("hard_"), mean, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(group = "class") %>%
  relocate(school, group)
df5_mean_school_class




df5_all <- list(df5_mean_school, df5_mean_district) %>%
   reduce(bind_rows) %>%
   mutate(
    across(c(starts_with("percent_"), starts_with("teacher_"),
             starts_with("f_"), starts_with("hard_")), 
           scales::label_number(scale = 100, accuracy = 0.01)
           )
  ) %>% 
  mutate(
    across(c(starts_with("percent_"), starts_with("teacher_"),
             starts_with("f_"), starts_with("hard_")), 
           as.numeric
    )
  ) 




df5_set <- list(df5_mean_school_class, 
                df5_mean_school %>% 
                  mutate(class_id = "全校", .after = group)
                ) %>%
  reduce(bind_rows) %>%
  mutate(
    across(c(starts_with("percent_"), starts_with("teacher_"),
             starts_with("f_"), starts_with("hard_")), 
           scales::label_number(scale = 100, accuracy = 0.01)
    )
  ) %>% 
  mutate(
    across(c(starts_with("percent_"), starts_with("teacher_"),
             starts_with("f_"), starts_with("hard_")), 
           as.numeric
    )
  ) 



df5_start %>%
  distinct(school)

df5_set %>%
  filter(school == "彩虹小学")


df5_all %>% colnames()
df5_all %>% select(school, group, starts_with("effect_"))
df5_all %>% select(school, group, starts_with("test_score"))
df5_all %>% select(school, group, starts_with("percent_"))
df5_all %>% select(school, group, starts_with("t11_"))
df5_all %>% select(school, group, starts_with("teacher_"))
df5_all %>% select(school, group, starts_with("f_"))
df5_all %>% select(school, group, starts_with("hard_"))
######################################################################





#########################################################
pairs56 <- 
  readxl::read_excel("./data/pairs56.xlsx") %>%
  tibble::deframe()

# 测试
df5_all %>% 
  select(starts_with("percent_")) %>% 
  rename_with(everything(), .fn = ~str_replace_all(.x, pattern = pairs56))

df5_set %>% 
  select(starts_with("percent_")) %>% 
  rename_with(everything(), .fn = ~str_replace_all(.x, pattern = pairs56))
#########################################################






#########################################################
#与2019年对比，今年的5年级对去年4年级，今年的6年级对去年5年级
# 2019年 34所 vs 2020年  36所

# - 清波小学校/ 万春小学
# + 草小子美分校/ 金沙清波分校/ 胜西高坎分校 / 石室蜀华小学部

school_name_pairs <- 
  readxl::read_excel("./data/school_name_match.xlsx") %>%
  tibble::deframe()


df_lastyear_4 <- readr::read_rds("./data/df_all2019_four.rds")
df_lastyear_4

df_lastyear_4_clean <- df_lastyear_4 %>% 
  mutate(school = recode(school, !!!school_name_pairs)) %>%
  select(school, group, 
         percent_sleep_time     = sleep_percent,
         percent_homework_time  = homework_time_percent,
         percent_music          = music_percent,  
         percent_art            = art_percent,
         percent_sport          = sport_percent, 
         percent_hours_exercise = sport_exercise_percent, 
         percent_score_rank     = score_rank_percent
  )

burden_percent2019 <- df_lastyear_4_clean %>% 
  pivot_longer(
    cols = starts_with("percent_"),
    names_to = "index",
    values_to = "value2019",
    values_transform = list(value2019 = scales::label_number(scale = 100, accuracy = 0.01))
  ) %>%
  select(-group)


burden_percent2020 <- df5_all %>% 
  select(school, group, starts_with("percent_")) %>% 
  select(school, group, 
         percent_sleep_time ,
         percent_homework_time,
         percent_music,  
         percent_art,
         percent_sport, 
         percent_hours_exercise, 
         percent_score_rank
  ) %>% 
  pivot_longer(
    cols = starts_with("percent_"),
    names_to = "index",
    values_to = "value2020"
  ) 


df5_burden_percent_combine <- burden_percent2020 %>% 
  left_join(burden_percent2019, by = c("school", "index")) %>% 
  mutate(index = recode(index, !!!pairs56)) %>% 
  mutate(
    across(starts_with("value"), as.numeric)
  )

df5_burden_percent_combine

#df5_burden_percent_combine %>% write.csv("compare.csv")

#########################################################





#########################################################
# save to Rdata
save(
  df5_start,
  df5_scoring_rate,
  df5_all,
  df5_set,
  pairs56,
  df5_burden_percent_combine,
  file = "./data/myData_df5.Rdata"
)
#########################################################


