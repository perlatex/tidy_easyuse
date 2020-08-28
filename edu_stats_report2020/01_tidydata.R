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
d_actual_num <- tibble::tribble(
  ~"学校",   ~"实际人数",
  "成博文武学校",  73L,
  "成都八中", 504L,
  "成都二十中", 522L,
  "成都三十六中", 337L,
  "成都三十三中", 282L,
  "成都十八中", 517L,
  "成都四十四中", 208L,
  "成都铁中", 604L,
  "花照中学", 292L,
  "交大附中", 168L,
  "金牛区博才学校",  56L,
  "金牛区腾飞学校", 296L,
  "金牛区希望学校", 105L,
  "金牛区新徽学校", 240L,
  "金牛区育梁学校",  94L,
  "金牛实验中学", 506L,
  "金牛中学", 562L,
  "锦西中学", 365L,
  "七中八一学校", 263L,
  "七中万达学校", 645L,
  "人北中学", 229L,
  "蜀西实验学校", 348L,
  "蜀星外语校",  64L,
  "天一学校", 115L,
  "铁中府河学校", 455L,
  "通锦中学", 302L,
  "五月花学校",  49L,
  "协同外语校", 389L
)

d_sample_num <- d %>% count(school) %>% 
  set_names(c("学校", "样本数"))
d_sample_num

df_sampling <- d_actual_num %>% 
  left_join(d_sample_num, by = "学校") %>% 
  mutate(`比例` = `样本数` / `实际人数`) %>% 
  mutate(`比例` = scales::percent(`比例`, accuracy = .01))
#########################################################






#########################################################
index_question <- tibble::tribble(
  ~index,                       ~question,
  "切身体验机会",               "直观感受事物，如观察聆听触摸等",
  "切身体验机会",              "用心感受和揣摩，如猜测疑惑好奇等",
  "切身体验机会",             "开展相应行动，如动手操作设计探索等",
  "反思与批评思维",                   "对老师讲的内容反复思考",
  "反思与批评思维",                  "就所学内容形成自己的想法",
  "反思与批评思维",                   "评估自己的观点正确与否",
  "反思与批评思维",                 "质疑他人的观点是否有说服力",
  "反思与批评思维",                 "评估不同观点，选择最优思路",
  "整体与辩证思维",                 "将新知识与已知知识联系起来",
  "整体与辩证思维",                  "将不同学科的知识联系起来",
  "整体与辩证思维",          "利用思维导图等形式理解不同知识之间的关系",
  "整体与辩证思维",              "在不同时间点上认识知识的发展变化",
  "整体与辩证思维",                  "会对所学知识进行一定总结",
  "实践与创新思维",                   "用非常规的目光审视问题",
  "实践与创新思维",                      "想到新的解决办法",
  "实践与创新思维",                     "提出自己独特的见解",
  "实践与创新思维",                  "问一些别人没有想到的问题",
  "实践与创新思维",                 "开展实际行动验证想法的正误",
  "实践与创新思维",                "将知识应用于实际问题的解决中",
  "学会学习指导",            "找到适合不同学科特点的学习方法和策略",
  "学会学习指导",              "当不能理解时，向其他学生寻求帮助",
  "学会学习指导",                "请老师澄清你不太理解的知识等",
  "学会学习指导",                 "带着问题来学习，并得到解答",
  "学会学习指导",              "对自己的学习过程进行有意识的调控",
  "学会学习指导",             "根据实际情况，调整学习进程和方法等",
  "交流合作机会",                "与其他同学合作完成作业/任务",
  "交流合作机会",       "同学之间对彼此的作业/任务完成情况提供反馈意见",
  "交流合作机会",                  "给同学讲解看法所学知识等",
  "交流合作机会",                "与班上同学进行相关问题的讨论",
  "评价反馈方式",                       "随堂测验的结果",
  "评价反馈方式",                    "课堂上回答问题的正误",
  "评价反馈方式",                "课堂上老师对我学习表现的点评",
  "评价反馈方式",                "课堂上同学对我学习表现的点评",
  "评价反馈方式",                    "课后作业老师批改信息",
  "评价反馈方式",                          "考试成绩",
  "评价反馈方式",                     "其他学习活动的结果",
  "新型教学形式",                   "跨学科基于项目式的学习",
  "新型教学形式",                  "跨学科基于问题解决的学习",
  "新型教学形式",                  "跨学科基于课题研究的学习",
  "新型教学形式",             "学科内上课，但会涉及其他学科的知识",
  "新型教学形式",              "学科内上课，重新组织教材内容上课",
  "新型教学形式",              "学科内上课，按照教材原有顺序上课",
  "对知识的理解",                   "我了解了知识的来龙去脉",
  "对知识的理解",                 "我建立了不同知识之间的关联",
  "对知识的理解",                    "我掌握了学科学习方法",
  "对知识的理解",                    "我掌握了学科关键思想",
  "对知识的理解",           "我能琢磨和领会具有本质性和规律性的东西",
  "对意义的理解",              "我能理解知识背后蕴含的作用和价值",
  "对意义的理解",                    "我有自己的价值和信念",
  "对意义的理解",                    "我有自己的愿望和理想",
  "迁移与创造",      "我能将所学知识应用于解决类似的问题或变化的情境中",
  "迁移与创造",            "我能把生活中的现象与所学知识联系起来",
  "迁移与创造",           "我能运用所学知识来解决生活中的实际问题",
  "迁移与创造",   "我能将一个学科中学到的方法思想等运用到其他学科的学习中",
  "迁移与创造",                "我能将不同学科的知识结合起来",
  "迁移与创造",                "我能提出独特或创新的个人见解",
  "学习规划",             "我通常在一个可以集中精力的地方学习",
  "学习规划",                   "我很好地安排了学习时间",
  "学习规划",                     "我有自己的学习计划",
  "学习策略",                 "我会反复阅读课堂笔记和教材",
  "学习策略",                      "我会反复练习习题",
  "学习策略",           "我把重要知识点列成清单，并把清单背下来",
  "学习策略",                  "我很少在考试前找时间复习",
  "学习毅力", "我经常感到很懒惰或无聊，以至于我还没有完成学习计划就放弃了",
  "学习毅力",       "即使我不喜欢正在学习的内容，我也会努力完成学习",
  "学习毅力",      "当学习有困难时，我要么放弃，要么只学习容易的部分",
  "学习动机",                 "学习满足了我对知识的好奇心",
  "学习动机",                 "学习唤起我对美好事物的渴望",
  "学习动机",                 "我觉得学习是一件很有趣的事",
  "学习动机",                   "学习让我的价值得以体现",
  "学习动机",               "学习对今后的生活和工作很有用处",
  "学习动机",                   "学习对自己的成长很重要",
  "学习动机",                "获得好成绩是我现在最满意的事",
  "学习动机",             "我想取得比其他大多数同学更好的成绩",
  "学习信心",             "如果我用适当的方式学习，我能够学会",
  "学习信心",              "如果我足够努力，学习完全难不倒我",
  "学习信心",           "我认为课堂上老师讲的最难内容我都能理解",
  "学习信心",                   "我有信心能出色完成作业",
  "学习信心",                   "我相信自己会取得好成绩",
  "考试焦虑",          "我会想到我和其他学生相比，我的成绩有多差",
  "考试焦虑",                 "我会想到很多题目我不能回答",
  "考试焦虑",                     "我想到了失败的后果",
  "考试焦虑",                   "我有一种不安心烦的感觉",
  "考试焦虑",                     "我觉得我的心跳很快",
  "交流合作效果",                  "我能与他人进行积极的互动",
  "交流合作效果",                     "我能倾听他人的发言",
  "交流合作效果",                   "我能流畅分享自己的观点",
  "交流合作效果",             "我能通过合作确定或创造问题解决方案"
)
#########################################################






#########################################################
# save to Rdata
save(d,
  d_questionnaire_labels,
  d_scoring_rate,
  df_mean_school,
  df_all,
  df_sampling,
  index_question,
  file = "./data/myData.Rdata"
)
#########################################################
