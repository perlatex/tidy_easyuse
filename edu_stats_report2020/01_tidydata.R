library(tidyverse)
library(stringr)

## 定义的函数
source("_common.R", encoding = "UTF-8")



#########################################################
## 读取数据
d_questionnaire_labels <- readxl::read_excel("./data/questionnaire_labels.xlsx")


d <- readxl::read_excel("./data/rawdata.xlsx", skip = 1)
d <- d %>%
  rename_with(
    .cols = -c(1:13),
    .fn = ~ stringr::str_remove_all(., "[、\\d\\(\\)]")
  )


d <- d %>%
  mutate(
    across(ends_with("202007"), ~ na_if(., 0))
  ) %>%
  drop_na(ends_with("202007"))
#########################################################






# 以下是数据清洗探索过程，不用再执行了
#########################################################
e1 <- d %>%
  summarise(
    across(everything(), ~ sum(is.na(.)))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "values"
  )

e2 <- d %>% filter_all(any_vars(is.na(.)))

d %>%
  select(ends_with("202007")) %>%
  summarise(
    across(ends_with("202007"), ~ range(.))
  )

d %>%
  select(ends_with("202007")) %>%
  filter_all(any_vars(. == min(.)))

d %>%
  select(ends_with("202007")) %>%
  filter_all(any_vars(. != min(.))) %>%
  summarise(
    across(ends_with("202007"), ~ range(.))
  )

d %>%
  select(ends_with("202007")) %>%
  summarise(
    across(ends_with("202007"), ~ head(sort(.), 15))
  )

d %>%
  select(ends_with("202007")) %>%
  summarise(
    across(ends_with("202007"), ~ sum(. < 10))
  )

d %>%
  select(ends_with("202007")) %>%
  mutate(
    across(ends_with("202007"), ~ na_if(., 0))
  ) %>%
  summarise(
    across(everything(), ~ sum(is.na(.)))
  )


d %>%
  select(ends_with("202007")) %>%
  filter_all(any_vars(. == min(.)))

d %>%
  select(ends_with("202007")) %>%
  mutate(
    across(ends_with("202007"), ~ na_if(., 0))
  ) %>%
  filter_all(any_vars(is.na(.)))



d %>%
  select(ends_with("202007")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "item",
    values_to = "values"
  ) %>%
  ggplot(aes(x = values)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(vars(item))
#########################################################








#########################################################
# 这38个问题，考察的是3-4选项，为了方便计算，我们将其转换:
# 比如，”选项4“ 转换成（5-4）=选项1，然后考察”选项1“即可，这样是等价的。
# 其他选项同理

item_transform <- c(
  "我了解了知识的来龙去脉", #
  "我建立了不同知识之间的关联", #
  "我掌握了学科学习方法", #
  "我掌握了学科关键思想", #
  "我能琢磨和领会具有本质性和规律性的东西", #
  "我能理解知识背后蕴含的作用和价值", #
  "我有自己的价值和信念", #
  "我有自己的愿望和理想", #


  "我能将所学知识应用于解决类似的问题或变化的情境中", #
  "我能把生活中的现象与所学知识联系起来", #
  "我能运用所学知识来解决生活中的实际问题", #
  "我能将一个学科中学到的方法思想等运用到其他学科的学习中", #
  "我能将不同学科的知识结合起来", #
  "我能提出独特或创新的个人见解", #


  "我通常在一个可以集中精力的地方学习", #
  "我很好地安排了学习时间", #
  "我有自己的学习计划", #
  "我会反复阅读课堂笔记和教材", #
  "我会反复练习习题", #
  "我把重要知识点列成清单，并把清单背下来", #
  "即使我不喜欢正在学习的内容，我也会努力完成学习", #


  "学习满足了我对知识的好奇心", #
  "学习唤起我对美好事物的渴望", #
  "我觉得学习是一件很有趣的事", #
  "学习让我的价值得以体现", #
  "学习对今后的生活和工作很有用处", #
  "学习对自己的成长很重要", #
  "获得好成绩是我现在最满意的事", #
  "我想取得比其他大多数同学更好的成绩", #


  "如果我用适当的方式学习，我能够学会", #
  "如果我足够努力，学习完全难不倒我", #
  "我认为课堂上老师讲的最难内容我都能理解", #
  "我有信心能出色完成作业", #
  "我相信自己会取得好成绩", #


  "我能与他人进行积极的互动", #
  "我能倾听他人的发言", #
  "我能流畅分享自己的观点", #
  "我能通过合作确定或创造问题解决方案" #
)

length(item_transform)


d <- d %>% mutate(
  across(all_of(item_transform), ~ 5 - .x)
)
#########################################################








#########################################################
## 计算得分率
d_scoring_rate <- d %>%
  rowwise() %>%
  mutate(
    t_Practical_experience = positive_scoring(c_across(all_of(content_Practical_experience))),
    t_Higher_order_thinking = positive_scoring(c_across(all_of(content_Higher_order_thinking))),
    
    f_Higher_order_thinking_rethink = positive_scoring(c_across(all_of(content_Higher_order_thinking_rethink))),
    f_Higher_order_thinking_whole = positive_scoring(c_across(all_of(content_Higher_order_thinking_whole))),
    f_Higher_order_thinking_practice = positive_scoring(c_across(all_of(content_Higher_order_thinking_practice))),
    
    
    t_Learn_to_learn = positive_scoring(c_across(all_of(content_Learn_to_learn))),
    t_Cooperation = positive_scoring(c_across(all_of(content_Cooperation))),

    t_Understanding_nature_of_things = positive_scoring(c_across(all_of(content_Understanding_nature_of_things))),
    t_Understanding_meaning_of_knowledge = positive_scoring(c_across(all_of(content_Understanding_meaning_of_knowledge))),
    t_Application_of_knowledge = positive_scoring(c_across(all_of(content_Application_of_knowledge))),
    t_Learning_planning = positive_scoring(c_across(all_of(content_Learning_planning))),
    t_Learning_strategy = positive_scoring(c_across(all_of(content_Learning_strategy))),
    t_Learning_persistence = positive_scoring(c_across(all_of(content_Learning_persistence))),
    t_Learning_motivation = positive_scoring(c_across(all_of(content_Learning_motivation))),
    t_Learning_confidence = positive_scoring(c_across(all_of(content_Learning_confidence))),
    t_Examination_anxiety = positive_scoring(c_across(all_of(content_Examination_anxiety))),

    t_Exchange_and_share = positive_scoring(c_across(all_of(content_Exchange_and_share))),

    t_Knowledge_mastery = 100 * sum(c_across(ends_with("202007"))) / 450
  ) %>%
  select(pid, id, name, school, class, 
         t_Knowledge_mastery, 
         starts_with("t"), 
         starts_with("f")
  ) %>% 
  ungroup()
#########################################################










#########################################################
## 计算均值
df_mean_district <- d_scoring_rate %>%
  ungroup() %>%
  summarise(
    across(c(starts_with("t_"), starts_with("f_")), mean, na.rm = TRUE)
  ) %>%
  mutate(school = "全区", group = "district") %>%
  relocate(school, group)

df_mean_district




df_mean_school <- d_scoring_rate %>%
  ungroup() %>%
  group_by(school) %>%
  summarise(
    across(c(starts_with("t_"), starts_with("f_")), mean, na.rm = TRUE)
  ) %>%
  mutate(group = "school") %>%
  relocate(school, group)
df_mean_school



df_all <- list(df_mean_school, df_mean_district) %>%
  reduce(bind_rows)
df_all
#########################################################











#########################################################
# save to Rdata
save(d,
  d_questionnaire_labels,
  d_scoring_rate,
  df_mean_school,
  df_all,
  file = "./data/myData.Rdata"
)
#########################################################