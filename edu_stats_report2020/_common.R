library(tidyverse)
library(flextable)



labels <- c(
  # part One
  # 切身体验机会
  "t_Practical_experience"               = "切身体验机会",
  
  # 高阶思维参与
  "t_Higher_order_thinking"              = "高阶思维参与",
  "f_Higher_order_thinking_rethink"      = "反思与批评思维",
  "f_Higher_order_thinking_whole"        = "整体与辩证思维",
  "f_Higher_order_thinking_practice"     = "实践与创新思维",
  
  # 学会学习指导
  "t_Learn_to_learn"                     = "学会学习指导",
  
  # 交流合作机会
  "t_Cooperation"                        = "交流合作机会",
  
  # part Two
  # 学业成绩
  "t_Knowledge_mastery"                  = "学业成绩",
  
  # 深层理解
  "t_Understanding_nature_of_things"     = "对知识的理解",
  "t_Understanding_meaning_of_knowledge" = "对意义的理解",
  
  # 迁移与创造
  "t_Application_of_knowledge"           = "迁移与创造",
  
  # 自我调节学习
  "t_Learning_planning"                  = "学习规划",
  "t_Learning_strategy"                  = "学习策略",
  "t_Learning_persistence"               = "学习毅力",
  
  # 学习情感动力
  "t_Learning_motivation"                = "学习动机",
  "t_Learning_confidence"                = "学习信心",
  "t_Examination_anxiety"                = "考试焦虑",
  
  # 交流合作效果
  "t_Exchange_and_share"                 = "交流合作效果",
  
  # 其余
  "chinese202007"                        = "语文得分率",
  "math202007"                           = "数学得分率",
  "english202007"                        = "英语得分率",
  "total_score"                          = "总得分率"
  
)




### 产生 minbar
### question - 金牛区- cur_schoolname
### 比较最后两列，大于区均值的柱子一个颜色，低于的另一个颜色
flextable_minbar <- function(dt) {
  dt %>%
    flextable(cwidth = 3) %>%
    compose(
      j = 3,
      value = as_paragraph(
        minibar(
          value = dt[[ncol(dt)]], max = 100,
          barcol = if_else(dt[[ncol(dt)]] > dt[[ncol(dt) - 1]], "#DD3322", "black")
        ),
        " ",
        as_chunk(dt[[ncol(dt)]], 
                 formatter = function(x) formatC(x, digits = 2, format = "f", flag = "0"),
                 props = fp_text(color = "black")
        )
      ),
      part = "body"
    ) %>%
    autofit() %>%
    width(j= 1, width = 3.5)
}


# flextable_minbar <- function(.data, cur_school) {
#   .data %>%
#     flextable(cwidth = 3) %>%
#     compose(
#       j = 3,
#       value = as_paragraph(
#         minibar(
#           value = .data[[cur_school]], max = 100,
#           barcol = if_else(.data[[cur_school]] > .data[["金牛区"]], "red", "blue")
#         ),
#         " ",
#         as_chunk(.data[[cur_school]],
#                  props = fp_text(color = "red")
#         )
#       ),
#       part = "body"
#     ) %>%
#     autofit()
# }

flextable_minbar2 <- function(dt) {
  dt %>%
    flextable(cwidth = 3) %>%
    theme_box() %>%
    compose(
      j = 4,
      value = as_paragraph(
        minibar(
          value = dt[[ncol(dt)]], max = 100,
          barcol = if_else(dt[[ncol(dt)]] > dt[[ncol(dt) - 1]], "#DD3322", "black")
        ),
        " ",
        as_chunk(dt[[ncol(dt)]], 
                 formatter = function(x) formatC(x, digits = 2, format = "f", flag = "0"),
                 props = fp_text(color = "black")
        )
      ),
      part = "body"
    ) %>% 
    merge_v(j = 1, part = "body") %>% 
    autofit() %>%
    width(j = 1, width = 1.4) 
}






### map list to flextable 相连的表格产生间隔， 注意在rmd中要 results='asis'
### usage
### map(iris_split, flextable_list_display)
flextable_list_display <- function(df) {
  cat("<w:p/>")
  ft <- flextable::flextable(df, cwidth = 1.3)  
  cat(knitr::knit_print(ft))
}



##### 统计选项占比：选择1和2的比例多高
stats_option_prop <- function(.data, cur_option) {
  cur_content <- cur_option

  r1_a <- .data %>%
    summarise(
      across(all_of(cur_content), ~ sum(. %in% c(1, 2)) * 100 / n())
    ) %>%
    mutate(school = "金牛区") %>%
    relocate(school)


  r1_b <- d %>%
    group_by(school) %>%
    summarise(
      across(
        all_of(cur_content), ~ sum(. %in% c(1, 2)) * 100 / n()
      )
    ) %>%
    ungroup()


  df <- bind_rows(r1_a, r1_b) %>%
    mutate(
      across(all_of(cur_content), list(RC = ~ . >= first(.)))
    ) %>%
    rowwise() %>%
    mutate(
      num_above_mean = sum(c_across(ends_with("_RC")))
    ) %>%
    ungroup() %>%
    select(-ends_with("_RC")) %>%
    arrange(desc(num_above_mean)) %>%
    select(-num_above_mean)

  return(df)
}


##### 打印出表格，高于区均值的要加背景色
colwise_color_fun <- function(x) {
  col <- character(length = length(x))
  col[] <- "transparent"
  col[x > x[1]] <- "gray"
  col
}

flextable_print <- function(.data, cwidth = 1.3) {
  if (ncol(.data) > 4) {
    cwidth <- 1
  }

  .data %>%
    flextable::flextable(cwidth = cwidth) %>%
    bg(
      j = 2:ncol(.data),
      bg = colwise_color_fun
    ) %>%
    color(i = 1, 
          color = "red"
    ) %>% 
    align_nottext_col(align = "center") 
    
}




##### 计分，选择1的记为4分，选择4的记为1分
positive_scoring <- function(vec) {
 100 * mean(abs(5 - vec)) / 4 # 考察1、2， 即选1得4分, 2 3 1  --> (2+3+1)/ 12
}






### 切实体会的机会
content_Practical_experience <- c(
  "直观感受事物，如观察聆听触摸等",
  "用心感受和揣摩，如猜测疑惑好奇等",
  "开展相应行动，如动手操作设计探索等"
)




### 高阶思维的机会
content_Higher_order_thinking <- c(
  "对老师讲的内容反复思考",
  "就所学内容形成自己的想法",
  "评估自己的观点正确与否",
  "质疑他人的观点是否有说服力",
  "评估不同观点，选择最优思路",

  "将新知识与已知知识联系起来",
  "将不同学科的知识联系起来",
  "利用思维导图等形式理解不同知识之间的关系",
  "在不同时间点上认识知识的发展变化",
  "会对所学知识进行一定总结",

  "用非常规的目光审视问题",
  "想到新的解决办法",
  "提出自己独特的见解",
  "问一些别人没有想到的问题",
  "开展实际行动验证想法的正误",
  "将知识应用于实际问题的解决中"
)


 

#### 高阶思维的机会-反思与批评思维
content_Higher_order_thinking_rethink <- c(
  "对老师讲的内容反复思考",
  "就所学内容形成自己的想法",
  "评估自己的观点正确与否",
  "质疑他人的观点是否有说服力",
  "评估不同观点，选择最优思路"
)

#### 高阶思维的机会-整体与辩证思维
content_Higher_order_thinking_whole <- c(
  "将新知识与已知知识联系起来",
  "将不同学科的知识联系起来",
  "利用思维导图等形式理解不同知识之间的关系",
  "在不同时间点上认识知识的发展变化",
  "会对所学知识进行一定总结"
)

#### 高阶思维的机会-实践与创新思维
content_Higher_order_thinking_practice <- c(  
  "用非常规的目光审视问题",
  "想到新的解决办法",
  "提出自己独特的见解",
  "问一些别人没有想到的问题",
  "开展实际行动验证想法的正误",
  "将知识应用于实际问题的解决中"
)




### 学会学习的机会
content_Learn_to_learn <- c(
  "找到适合不同学科特点的学习方法和策略",
  "当不能理解时，向其他学生寻求帮助",
  "请老师澄清你不太理解的知识等",
  "带着问题来学习，并得到解答",
  "对自己的学习过程进行有意识的调控",
  "根据实际情况，调整学习进程和方法等"
)



### 交流合作的机会
content_Cooperation <- c(
  "与其他同学合作完成作业/任务",
  "同学之间对彼此的作业/任务完成情况提供反馈意见",
  "给同学讲解看法所学知识等",
  "与班上同学进行相关问题的讨论"
)



### 反馈方式
content_Feedback <- c(
  "随堂测验的结果",
  "课堂上回答问题的正误",
  "课堂上老师对我学习表现的点评",
  "课堂上同学对我学习表现的点评",
  "课后作业老师批改信息",
  "考试成绩",
  "其他学习活动的结果"
)



### 教学方法
content_Teaching_methods <- c(
  "跨学科基于项目式的学习",
  "跨学科基于问题解决的学习",
  "跨学科基于课题研究的学习",
  "学科内上课，但会涉及其他学科的知识",
  "学科内上课，重新组织教材内容上课",
  "学科内上课，按照教材原有顺序上课"
)


## 学习效果
### 知识掌握
### content_Knowledge_mastery







### 知识与自我理解

#### 对事物或知识本质的理解
content_Understanding_nature_of_things <- c(
  "我了解了知识的来龙去脉",
  "我建立了不同知识之间的关联",
  "我掌握了学科学习方法",
  "我掌握了学科关键思想",
  "我能琢磨和领会具有本质性和规律性的东西"
)



#### 对知识和自我生命意义的理解
content_Understanding_meaning_of_knowledge <- c(
  "我能理解知识背后蕴含的作用和价值",
  "我有自己的价值和信念",
  "我有自己的愿望和理想"
)






### 知识迁移与创造
content_Application_of_knowledge <- c(
  "我能将所学知识应用于解决类似的问题或变化的情境中",
  "我能把生活中的现象与所学知识联系起来",
  "我能运用所学知识来解决生活中的实际问题",
  "我能将一个学科中学到的方法思想等运用到其他学科的学习中",
  "我能将不同学科的知识结合起来",
  "我能提出独特或创新的个人见解"
)







### 学习自我调节

#### 学习规划
content_Learning_planning <- c(
  "我通常在一个可以集中精力的地方学习",
  "我很好地安排了学习时间",
  "我有自己的学习计划"
)



#### 学习策略
content_Learning_strategy <- c(
  "我会反复阅读课堂笔记和教材",
  "我会反复练习习题",
  "我把重要知识点列成清单，并把清单背下来",
  "我很少在考试前找时间复习"
)




#### 学习毅力
content_Learning_persistence <- c(
  "我经常感到很懒惰或无聊，以至于我还没有完成学习计划就放弃了",
  "即使我不喜欢正在学习的内容，我也会努力完成学习",
  "当学习有困难时，我要么放弃，要么只学习容易的部分"
)





### 学习态度情感价值观

#### 学习动力
content_Learning_motivation <- c(
  "学习满足了我对知识的好奇心",
  "学习唤起我对美好事物的渴望",
  "我觉得学习是一件很有趣的事",

  "学习让我的价值得以体现",
  "学习对今后的生活和工作很有用处",
  "学习对自己的成长很重要",

  "获得好成绩是我现在最满意的事",
  "我想取得比其他大多数同学更好的成绩"
)







#### 学习信心
content_Learning_confidence <- c(
  "如果我用适当的方式学习，我能够学会",
  "如果我足够努力，学习完全难不倒我",
  "我认为课堂上老师讲的最难内容我都能理解",
  "我有信心能出色完成作业",
  "我相信自己会取得好成绩"
)





#### 考试焦虑
content_Examination_anxiety <- c(
  "我会想到我和其他学生相比，我的成绩有多差",
  "我会想到很多题目我不能回答",
  "我想到了失败的后果",
  "我有一种不安心烦的感觉",
  "我觉得我的心跳很快"
)




### 交流分享
content_Exchange_and_share <- c(
  "我能与他人进行积极的互动",
  "我能倾听他人的发言",
  "我能流畅分享自己的观点",
  "我能通过合作确定或创造问题解决方案"
)
