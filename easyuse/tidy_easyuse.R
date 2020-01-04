library(tidyverse)
library(readxl)
library(easyuse) # devtools::install_github("perlatex/easyuse")
library(ggrepel)

#######################################################################################
# define standrad function
scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

# convert enlish to chinese title for visulization
en2cn <- tibble::tribble(
  ~en, ~cn,
  "girl", "文科",
  "boy", "理科",
  "class", "班级层面",
  "school", "学校层面",
  "total", "总分",
  "chin", "语文",
  "math", "数学",
  "english", "英语",
  "politics", "政治",
  "history", "历史",
  "geo", "地理",
  "phy", "物理",
  "chem", "化学",
  "bio", "生物"
)

v <- en2cn %>% deframe()
#######################################################################################


# read file
df1 <- read_excel("./data/newdata.xlsx", sheet = 1)
df1 %>% colnames()

# 存在全班没成绩的情况，这里要剔除
r1 <- df1 %>% 
  group_by(school,class) %>% 
  summarise(
    num = n(),
    mis = sum(is.na(total_2))
  ) #%>% 
  #filter(num == mis)


# 剔除的班级
df1 %>% 
  group_by(school,class) %>% 
  mutate(
    mis = sum(is.na(total_2))
  ) %>% 
  filter(mis > 10) %>% 
  summarise(
    num = n(),
    mis = sum(is.na(total_2))
  )


# 一个班缺考人数小于10人，才纳入统计
d1 <- df1 %>% 
  group_by(school, class) %>% 
  mutate(
    mis = sum(is.na(total_2))
  ) %>% 
  filter(mis < 10) %>%
  ungroup() %>% 
  
  mutate_at(vars(contains("_")), as.numeric) %>%
  mutate_at(
    vars(contains("_")),
    scale2,
    na.rm = TRUE
  ) %>%
  mutate_at(
    vars(contains("_")),
    ~ . * 100 + 500
  ) %>%
  group_by(school, class) %>%
  mutate_at(
    vars(contains("_")),
    ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
  ) %>%
  ungroup()




# to csv file
c("total", "chin", "math", "english", "politics", "history", "geo") %>%
  walk(
    ~ get_ran_vals(
      .data = d1,
      .var_school = school,
      .var_class = class,
      .var_score_pre = glue::glue( {.x}, "_2"),
      .var_score_after = glue::glue( {.x}, "_1"),
      effects = "class"
    ) %>%
      write_excel_csv(
        glue::glue( "girl_", "class_", {.x}, ".csv")
      )
  )



c("total", "chin", "math", "english", "politics", "history", "geo") %>%
  walk(
    ~ get_ran_vals(
      .data = d1,
      .var_school = school,
      .var_class = class,
      .var_score_pre = glue::glue( {.x}, "_2"),
      .var_score_after = glue::glue( {.x}, "_1"),
      effects = "school"
    ) %>%
      write_excel_csv(
        glue::glue( "girl_", "school_", {.x}, ".csv")
      )
  )


# to png figures
c("total", "chin", "math", "english", "politics", "history", "geo") %>%
  map(
    ~ get_ran_vals(
      .data = d1,
      .var_school = school,
      .var_class = class,
      .var_score_pre = glue::glue( {.x}, "_2"),
      .var_score_after = glue::glue( {.x}, "_1"),
      effects = "school"
    ) %>%
      ggplot(aes(x = fct_reorder(level, estimate, .desc = TRUE))) +
      geom_line(
        aes(y = mean_score_pre, 
            colour = "mean_score", 
            group = "mean_score")
      ) +
      geom_point(
        aes(y = mean_score_pre, 
            colour = "mean_score", 
            group = "mean_score")
      ) +
      geom_text(aes(y = mean_score_pre, label = round(mean_score_pre, 0)),
                hjust = -0.15, vjust = -0.1, color = "black"
      ) +
      
      geom_line(
        aes(y = estimate + 500, colour = "estimate", group = "estimate" )
      ) + 
      geom_point(
        aes(y = estimate + 500, colour = "estimate", group = "estimate" )
      ) +
      geom_text(aes(y = estimate + 500, label = round(estimate, 1)),
                hjust = -0.15, vjust = -0.1, color ="red"
      ) +
      scale_y_continuous(sec.axis = sec_axis(~. -500, name = "")) +
      scale_colour_manual(values = c("red", "black"),
                          name = "",
                          labels = c("增值分数", "分数均值")
      ) +
      #labs(title =  glue::glue( "girl_", "school_", {.x}) , x = "", y = "") +
      labs(title = glue::glue(v["girl"], "-", v["school"], "-", v[.x]), x = "", y = "") +
      theme(legend.position = "bottom")  
    
    
  ) %>% 
  iwalk(
    ~ ggsave(
      filename = glue::glue( "girl_", "school_", {.y}, ".png"),
      plot = .x,
      device = "png",
      width = 6,
      height = 3
    )
  )





c("total", "chin", "math", "english", "politics", "history", "geo") %>%
  map(
    ~ get_ran_vals(
      .data = d1,
      .var_school = school,
      .var_class = class,
      .var_score_pre = glue::glue( {.x}, "_2"),
      .var_score_after = glue::glue( {.x}, "_1"),
      effects = "class"
    ) %>%
      ggplot(aes(x = fct_reorder(level, estimate, .desc = TRUE))) +
      geom_line(
        aes(y = mean_score_pre, 
            colour = "mean_score", 
            group = "mean_score")
      ) +
      geom_point(
        aes(y = mean_score_pre, 
            colour = "mean_score", 
            group = "mean_score")
      ) +
      geom_text(aes(y = mean_score_pre, label = round(mean_score_pre, 0)),
                hjust = -0.15, vjust = -0.1, color = "black"
      ) +
      
      geom_line(
        aes(y = estimate + 500, colour = "estimate", group = "estimate" )
      ) + 
      geom_point(
        aes(y = estimate + 500, colour = "estimate", group = "estimate" )
      ) +
      geom_text_repel(aes(y = estimate + 500, label = round(estimate, 1)),
                      hjust = 0.5, color = "red", size = 3, nudge_y = 1,
                      segment.color = "gray"
      ) +
      scale_y_continuous(sec.axis = sec_axis(~. -500, name = "")) +
      scale_colour_manual(values = c("red", "black"),
                          name = "",
                          labels = c("增值分数", "分数均值")
      ) +
      #labs(title =  glue::glue( "girl_", "class_", {.x}) , x = "", y = "") +
      labs(title = glue::glue(v["girl"], "-", v["class"], "-", v[.x]), x = "", y = "") +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    
  ) %>% 
  iwalk(
    ~ ggsave(
      filename = glue::glue( "girl_", "class_", {.y}, ".png"),
      plot = .x,
      device = "png",
      width = 10,
      height = 5
    )
  )


####################################################
####################################################
####################################################



df2 <- read_excel("./data/newdata.xlsx", sheet = 2)
df2 %>% colnames()

# 存在全班没成绩的情况，这里要剔除
r2 <- df2 %>% 
  group_by(school,class) %>% 
  summarise(
    num = n(),
    mis = sum(is.na(total_2))
  ) #%>% 
#filter(num == mis)


# 剔除的班级
df2 %>% 
  group_by(school, class) %>% 
  mutate(
    mis = sum(is.na(total_2))
  ) %>% 
  filter(mis > 9) %>% 
  summarise(
    num = n(),
    mis = sum(is.na(total_2))
  )

# 一个班缺考人数小于10人，才纳入统计
d2 <- df2 %>% 
  group_by(school, class) %>% 
  mutate(
    mis = sum(is.na(total_2))
  ) %>% 
  filter(mis < 10) %>%  
  ungroup() %>% 
  
  mutate_at(vars(contains("_")), as.numeric) %>%
  mutate_at(
    vars(contains("_")),
    scale2,
    na.rm = TRUE
  ) %>%
  mutate_at(
    vars(contains("_")),
    ~ . * 100 + 500
  ) %>%
  group_by(school, class) %>%
  mutate_at(
    vars(contains("_")),
    ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
  ) %>%
  ungroup()




c("total", "chin", "math", "english", "phy", "chem", "bio") %>%
  walk(
    ~ get_ran_vals(
      .data = d2,
      .var_school = school,
      .var_class = class,
      .var_score_pre = glue::glue( {.x}, "_2"),
      .var_score_after = glue::glue( {.x}, "_1"),
      effects = "class"
    ) %>%
      write_excel_csv(
        glue::glue( "boy_", "class_", {.x}, ".csv")
      )
  )



c("total", "chin", "math", "english", "phy", "chem", "bio") %>%
  walk(
    ~ get_ran_vals(
      .data = d2,
      .var_school = school,
      .var_class = class,
      .var_score_pre = glue::glue( {.x}, "_2"),
      .var_score_after = glue::glue( {.x}, "_1"),
      effects = "school"
    ) %>%
      write_excel_csv(
        glue::glue( "boy_", "school_", {.x}, ".csv")
      )
  )




c("total", "chin", "math", "english", "phy", "chem", "bio") %>%
  map(
    ~ get_ran_vals(
      .data = d2,
      .var_school = school,
      .var_class = class,
      .var_score_pre = glue::glue( {.x}, "_2"),
      .var_score_after = glue::glue( {.x}, "_1"),
      effects = "school"
    ) %>%
      ggplot(aes(x = fct_reorder(level, estimate, .desc = TRUE))) +
      geom_line(
        aes(y = mean_score_pre, 
            colour = "mean_score", 
            group = "mean_score")
      ) +
      geom_point(
        aes(y = mean_score_pre, 
            colour = "mean_score", 
            group = "mean_score")
      ) +
      geom_text(aes(y = mean_score_pre, label = round(mean_score_pre, 0)),
                hjust = -0.15
      ) +
      
      geom_line(
        aes(y = estimate + 500, colour = "estimate", group = "estimate" )
      ) + 
      geom_point(
        aes(y = estimate + 500, colour = "estimate", group = "estimate" )
      ) +
      geom_text(aes(y = estimate + 500, label = round(estimate, 1)),
                hjust = -0.15, vjust = -0.1, color ="red"
      ) +
      scale_y_continuous(sec.axis = sec_axis(~. -500, name = "")) +
      scale_colour_manual(values = c("red", "black"),
                          name = "",
                          labels = c("增值分数", "分数均值")
      ) +
      #labs(title =  glue::glue( "boy_", "school_", {.x}) , x = "", y = "") +
      labs(title = glue::glue(v["boy"], "-", v["school"], "-", v[.x]) , x = "", y = "") +
      theme(legend.position = "bottom")  
    
    
  ) %>% 
  iwalk(
    ~ ggsave(
      filename = glue::glue( "boy_", "school_", {.y}, ".png"),
      plot = .x,
      device = "png",
      width = 6,
      height = 3
    )
  )
  
  
  
 



c("total", "chin", "math", "english", "phy", "chem", "bio") %>%
  map(
    ~ get_ran_vals(
      .data = d2,
      .var_school = school,
      .var_class = class,
      .var_score_pre = glue::glue( {.x}, "_2"),
      .var_score_after = glue::glue( {.x}, "_1"),
      effects = "class"
    ) %>%
      ggplot(aes(x = fct_reorder(level, estimate, .desc = TRUE))) +
      geom_line(
        aes(y = mean_score_pre, 
            colour = "mean_score", 
            group = "mean_score")
      ) +
      geom_point(
        aes(y = mean_score_pre, 
            colour = "mean_score", 
            group = "mean_score")
      ) +
      geom_text(aes(y = mean_score_pre, label = round(mean_score_pre, 0)),
                hjust = -0.15, vjust = -0.1, color ="black"
      ) +
      
      geom_line(
        aes(y = estimate + 500, colour = "estimate", group = "estimate" )
      ) + 
      geom_point(
        aes(y = estimate + 500, colour = "estimate", group = "estimate" )
      ) +
      geom_text_repel(aes(y = estimate + 500, label = round(estimate, 1)),
                      hjust = 0.5, color = "red", size = 3, nudge_y = 1,
                      segment.color = "gray"
      ) +
      scale_y_continuous(sec.axis = sec_axis(~. -500, name = "")) +
      scale_colour_manual(values = c("red", "black"),
                          name = "",
                          labels = c("增值分数", "分数均值")
      ) +
      #labs(title =  glue::glue( "boy_", "class_", {.x}) , x = "", y = "") +
      labs(title =  glue::glue(v["boy"], "-", v["class"], "-", v[.x]) , x = "", y = "") +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    
  ) %>% 
  iwalk(
    ~ ggsave(
      filename = glue::glue( "boy_", "class_", {.y}, ".png"),
      plot = .x,
      device = "png",
      width = 10,
      height = 5
    )
  )














#  
# ##############  
# dt1 %>% 
#   ggplot(aes(x = level, y = mean_score_pre, group = 1 )) +
#   geom_point() +
#   geom_line() +
#   geom_point(aes(x = level, y = mean_score_pre + estimate), 
#              color = "red") +
#   geom_line(aes(x = level, y = mean_score_pre + estimate, group = 2), 
#              color = "red") +
#   labs(title = "boy_school_total" , x = "", y = "")
# 
# 
# 
# 
# dt1 %>% 
#   ggplot(aes(x = level)) +
#   
#   geom_line(
#     aes(y = mean_score_pre, 
#         colour = "mean_score", 
#         group = "mean_score")
#     ) +
#   geom_point(
#     aes(y = mean_score_pre, 
#         colour = "mean_score", 
#         group = "mean_score")
#   ) +
#   geom_text(aes(y = mean_score_pre, label = round(mean_score_pre,0)),
#             hjust = -0.15
#             ) +
#   
#   geom_line(
#     aes(y = estimate + 450, colour = "estimate", group = "estimate" )
#   ) + 
#   geom_point(
#     aes(y = estimate + 450, colour = "estimate", group = "estimate" )
#   ) +
#   scale_y_continuous(sec.axis = sec_axis(~. -450, name = "")) +
#   scale_colour_manual(values = c("red", "black")) +
#   labs(title = "boy_school_total" , x = "", y = "") +
#   theme(legend.position = "none")
# 
# 
# #scale_y_continuous(sec.axis
# 
# 
# dt2 <- read_csv("./res/boy_class_chem.csv")
# dt2 %>% colnames()
# 
# dt2 %>% 
#   ggplot(aes(x = level, y = mean_score_pre, group = 1 )) +
#   geom_point() +
#   geom_line() +
#   geom_point(aes(x = level, y = mean_score_pre + estimate), 
#              color = "red") +
#   geom_line(aes(x = level, y = mean_score_pre + estimate, group = 2), 
#             color = "red") +
#   labs(title = "boy_class_chem" , x = NULL, y = NULL) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))  
#   
#   
# dt2 %>% ggplot(aes(x = reorder(level,mean_score_pre), y = mean_score_pre )) +
#   geom_col() +
#   coord_flip()


# dt2 <- read_csv("./res/boy_class_chem.csv")
# dt2 %>% colnames()
# 
# dt2 %>%
# ggplot(aes(x = reorder(level, -estimate))) +
#   geom_line(
#     aes(y = mean_score_pre, 
#         colour = "mean_score", 
#         group = "mean_score")
#   ) +
#   geom_point(
#     aes(y = mean_score_pre, 
#         colour = "mean_score", 
#         group = "mean_score")
#   ) +
#   geom_text(aes(y = mean_score_pre, label = round(mean_score_pre, 0)),
#             hjust = -0.15, vjust = -0.1, color = "black"
#   ) +
#   
#   geom_line(
#     aes(y = estimate + 500, colour = "estimate", group = "estimate" )
#   ) + 
#   geom_point(
#     aes(y = estimate + 500, colour = "estimate", group = "estimate" )
#   ) +
#   geom_text_repel(aes(y = estimate + 500, label = round(estimate, 1)),
#                   hjust = 0.5, color = "red", size = 3, nudge_y = 1,
#                   segment.color = "gray"
#   ) +
#   scale_y_continuous(sec.axis = sec_axis(~. -500, name = "")) +
#   scale_colour_manual(values = c("red", "black")) +
#   labs(title =  glue::glue( "boy_", "class_") , x = "", y = "") +
#   theme(legend.position = "none",
#         axis.text.x = element_text(angle = 45, hjust = 1)
#   )
# 
# 
# ggsave(
#   filename = glue::glue( "boy_", "class_", ".png"),
#   device = "png",
#   width = 10,
#   height = 4
# )
