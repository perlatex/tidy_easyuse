library(tidyverse)
library(showtext)
showtext_auto()
# 结果计算完毕后，这里是分析阶段


###############################################################
# 读取已经计算好的数据文件

 big_result_boy <- read_rds(here::here("data", "big_result_boy.rds"))
big_result_girl <- read_rds(here::here("data", "big_result_girl.rds"))
###############################################################



###############################################################
# convert enlish to chinese title for visulization
en2cn <- tibble::tribble(
  ~en, ~cn,
  "total", "总分",
  "chinese", "语文",
  "mathematics", "数学",
  "english", "英语",
  "politics", "政治",
  "history", "历史",
  "geography", "地理",
  "physics", "物理",
  "chemistry", "化学",
  "biology", "生物",
  "natural", "理综",
  "social", "文综"
) %>% deframe()

###############################################################





###############################################################
# 对结果统计分析
big_result_boy  %>% count(discipline)
big_result_girl %>% count(discipline)

## 计算理科
big_result_boy %>% 
  filter(!is.na(name)) %>% 
  group_by(discipline, school, class) %>% 
  summarise(
    median_SGP = median(SGP, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  writexl::write_xlsx(here::here("result", "boy", "boy_school_class.xlsx"))


big_result_boy %>% 
  filter(!is.na(name)) %>% 
  group_by(discipline, school) %>% 
  summarise(
    median_SGP = median(SGP, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  writexl::write_xlsx(here::here("result", "boy", "boy_school.xlsx"))



big_result_boy %>% 
  filter(!is.na(name)) %>% 
  group_by(school, .drop = FALSE) %>%
  group_walk(
    ~ writexl::write_xlsx(.x, here::here("result", "boy", paste0(.y, ".xlsx")))
  )


# plot
big_result_boy %>% 
  filter(!is.na(name)) %>% 
  group_by(discipline, school) %>% 
  summarise(
    median_SGP = median(SGP, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  
  mutate(discipline = as.factor(discipline),
         school = tidytext::reorder_within(school, median_SGP, discipline)) %>%
  
  ggplot(
    aes(y = median_SGP, x = school)
      ) +
  geom_col(fill = "#58508d", show.legend = FALSE) +
  #geom_text(aes(label = round(median_SGP, 1)), hjust = 1) +
  facet_wrap(vars(discipline), 
             scales = "free_y", 
             ncol = 2,
             labeller = labeller(discipline = en2cn) 
             ) +
  coord_flip() +
  tidytext::scale_x_reordered() +
  labs(y = "学校SGP分数",
       x = NULL
       #title = "What were the most common baby names in each decade?",
       #subtitle = "Via US Social Security Administration"
       ) +
  scale_y_continuous(
    limits = c(0, 80),
    expand = c(0,0),
    breaks = c(0, 20, 40, 60, 80),
    labels = c(0, 20, 40, 60, 80)
  ) +
ggsave(
  here::here("result", "boy", "boy.pdf"), 
  width = 6, 
  height = 7
)

#####################################################################









#####################################################################
#####################################################################
# 计算文科
big_result_girl %>% 
  filter(!is.na(name)) %>% 
  group_by(discipline, school, class) %>% 
  summarise(
    median_SGP = median(SGP, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  writexl::write_xlsx(here::here("result", "girl", "girl_school_class.xlsx"))


big_result_girl %>% 
  filter(!is.na(name)) %>% 
  group_by(discipline, school) %>% 
  summarise(
    median_SGP = median(SGP, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  writexl::write_xlsx(here::here("result", "girl", "girl_school.xlsx"))



big_result_girl %>% 
  filter(!is.na(name)) %>% 
  group_by(school, .drop = FALSE) %>%
  group_walk(
    ~ writexl::write_xlsx(.x, here::here("result", "girl", paste0(.y, ".xlsx")))
  )


# plot
big_result_girl %>% 
  filter(!is.na(name)) %>% 
  group_by(discipline, school) %>% 
  summarise(
    median_SGP = median(SGP, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  
  mutate(discipline = as.factor(discipline),
         school = tidytext::reorder_within(school, median_SGP, discipline)) %>%
  
  ggplot(
    aes(y = median_SGP, x = school)
  ) +
  geom_col(fill = "#ff6361", show.legend = FALSE) +
  facet_wrap(vars(discipline), 
             scales = "free_y", 
             ncol = 2,
             labeller = labeller(discipline = en2cn) 
  ) +
  coord_flip() +
  tidytext::scale_x_reordered() +
  labs(y = "学校SGP分数",
       x = NULL
       #title = "What were the most common baby names in each decade?",
       #subtitle = "Via US Social Security Administration"
  ) +
  scale_y_continuous(
    limits = c(0, 80),
    expand = c(0,0),
    breaks = c(0, 20, 40, 60, 80),
    labels = c(0, 20, 40, 60, 80)
  ) +
 ggsave(
  here::here("result", "girl", "girl.pdf"), 
  width = 6, 
  height = 7
)
#######################################################################




#######################################################################
# 其它未使用



# 计算理科
# big_result_boy %>% 
#   filter(!is.na(name)) %>% 
#   group_by(discipline, school, class) %>% 
#   summarise(
#     mean_SGP = mean(SGP, na.rm = TRUE),
#     probs = seq(0, 1, 0.25), 
#     quantile_SGP = quantile(SGP, probs = probs, na.rm= TRUE)
#   )


big_result_girl %>% 
  filter(!is.na(name)) %>% 
  group_by(discipline) %>% 
  summarise(
    mean_SGP = mean(SGP, na.rm = TRUE)
  )

big_result_boy %>% 
  ggplot(aes(x = SGP)) +
  geom_density() +
  facet_wrap(vars(discipline))


big_result_girl %>% 
  ggplot(aes(x = SGP)) +
  #ggridges::geom_density_ridges() +
  geom_density() +
  facet_wrap(vars(discipline))
###############################################################




