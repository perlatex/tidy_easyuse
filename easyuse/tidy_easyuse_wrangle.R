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
  group_by(school, class) %>% 
  summarise(
    num = n(),
    mis = sum(is.na(total_2))
  ) #%>% 
  #filter(num == mis)


# 剔除的班级
df1 %>% 
  group_by(school, class) %>% 
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





## 文科 + 班级层面
girl_class <- 
  c("total", "chin", "math", "english", "politics", "history", "geo") %>%
  map_dfr(
    ~ get_ran_vals(
      .data = d1,
      .var_school = school,
      .var_class = class,
      .var_score_pre = glue::glue( {.x}, "_2"),
      .var_score_post = glue::glue( {.x}, "_1"),
      effects = "class"
    ) %>% 
      mutate(
        type = "文科",
        effect = "班级层面",
        item = .x  ) %>% 
      separate(col = level, 
               into = c("school", "class"), 
               sep = "_", 
               remove = FALSE)
  )


fancy_plot <- function(.x, title_school, title_item ){
  
  st_point <- mean(.x$mean_score_pre)
  
  ggplot(data = .x, 
         aes(x = fct_reorder(class, estimate, .desc = TRUE))
  ) +
    geom_line(
      aes(y = mean_score_pre, colour = "mean_score", 
          group = "mean_score")
    ) +
    geom_point(aes(y = mean_score_pre, colour = "mean_score", 
                   group = "mean_score")
    ) +
    geom_text(aes(y = mean_score_pre, label = round(mean_score_pre, 0)),
              hjust = -0.15, vjust = -0.1, color = "black"
    ) +
    
    
    geom_line(
      aes(y = estimate + st_point, colour = "estimate", group = "estimate" )
    ) + 
    geom_point(
      aes(y = estimate + st_point, colour = "estimate", group = "estimate" )
    ) +
    geom_text_repel(aes(y = estimate + st_point, label = round(estimate, 1)),
                    hjust = 0.5, color = "red", size = 3, nudge_y = 1,
                    segment.color = "gray"
    ) +
    scale_y_continuous(sec.axis = sec_axis(~. - st_point, name = "")) +
    scale_colour_manual(values = c("red", "black"),
                        name = "",
                        labels = c("增值分数", "分数均值")
                        ) +
    labs(title = title_school,
         subtitle = paste0(.x$type, "-", v[title_item], "-", .x$effect), 
         x = NULL, 
         y = NULL ) +
    theme(#legend.title=element_blank(),
          legend.position = "bottom")
  
}










# girl_class %>% 
#   group_by(school, item) %>% 
#   group_split() %>% 
#   map(
#     ~ fancy_plot(.x)
#   ) 



girl_class %>% 
  group_by(school, item) %>% 
  group_map(
    #~ glue::glue(.y$school, "_", .y$item, ".png")
    ~ paste0(.y$school, "_", .y$item, ".png")
  )


girl_class %>% 
  group_by(school, item) %>% 
  group_walk(
    ~ ggsave(
      paste0(.y$school, "_", .y$item, ".png"),
      
      fancy_plot(.x, .y$school, .y$item),
      width = 6, 
      height = 4,
      dpi = 300,
      device = 'png'
    ) 
  ) %>% 
  invisible()









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



## 理科 + 班级层面

boy_class <- 
  c("total", "chin", "math", "english", "phy", "chem", "bio") %>%
  map_dfr(
    ~ get_ran_vals(
      .data = d2,
      .var_school = school,
      .var_class = class,
      .var_score_pre = glue::glue( {.x}, "_2"),
      .var_score_post = glue::glue( {.x}, "_1"),
      effects = "class"
    ) %>% 
      mutate(
        type = "理科",
        effect = "班级层面",
        item = .x  ) %>% 
      separate(col = level, 
               into = c("school", "class"), 
               sep = "_", 
               remove = FALSE)
  )


fancy_plot <- function(.x, title_school, title_item ){
  
  st_point <- mean(.x$mean_score_pre)
  
  ggplot(data = .x, 
         aes(x = fct_reorder(class, estimate, .desc = TRUE))
  ) +
    geom_line(
      aes(y = mean_score_pre, colour = "mean_score", 
          group = "mean_score")
    ) +
    geom_point(aes(y = mean_score_pre, colour = "mean_score", 
                   group = "mean_score")
    ) +
    geom_text(aes(y = mean_score_pre, label = round(mean_score_pre, 0)),
              hjust = -0.15, vjust = -0.1, color = "black"
    ) +
    
    
    geom_line(
      aes(y = estimate + st_point, colour = "estimate", group = "estimate" )
    ) + 
    geom_point(
      aes(y = estimate + st_point, colour = "estimate", group = "estimate" )
    ) +
    geom_text_repel(aes(y = estimate + st_point, label = round(estimate, 1)),
                    hjust = 0.5, color = "red", size = 3, nudge_y = 1,
                    segment.color = "gray"
    ) +
    scale_y_continuous(sec.axis = sec_axis(~. - st_point, name = "")) +
    scale_colour_manual(values = c("red", "black"),
                        name = "",
                        labels = c("增值分数", "分数均值")
    ) +
    labs(title = title_school,
         subtitle = paste0(.x$type, "-", v[title_item], "-", .x$effect), 
         x = NULL, 
         y = NULL ) +
    theme(legend.position = "bottom")
  
}









boy_class %>% 
  group_by(school, item) %>% 
  group_map(
    ~ paste0(.y$school, "_", .y$item, ".png")
  )


boy_class %>% 
  group_by(school, item) %>% 
  group_walk(
    ~ ggsave(
      paste0(.y$school, "_", .y$item, ".png"),
      
      fancy_plot(.x, .y$school, .y$item),
      width = 6, 
      height = 4,
      dpi = 300,
      device = 'png'
    ) 
  ) %>% 
  invisible()





