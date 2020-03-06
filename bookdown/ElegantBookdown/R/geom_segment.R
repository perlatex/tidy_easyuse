# 各个学校数学考试成绩
df_test_score <- df_four_degree %>%
  group_by(school) %>%
  summarise(
    test_mean_score = mean(test),
    test_median_score = median(test)
  )


df_test_score %>%
  mutate(diff = test_mean_score - mean(test_mean_score)) %>%
  ggplot(aes(
    x = diff,
    y = forcats::fct_reorder(school, diff),
    color = (diff < 0)
  )) +
  geom_point(size = 7) +
  geom_segment(aes(
    x = 0,
    xend = diff,
    y = school,
    yend = school
  )) +
  geom_text(aes(label = round(diff, 2)),
    color = "white",
    size = 3
  ) +
  scale_x_continuous(breaks = seq(-20, 10, 2)) +
  labs(
    x = NULL, y = NULL,
    title = "各校数学考试成绩",
    subtitle = "数据来源问卷调查",
    caption = "红色代表高于平均分，蓝色代表低于平均分"
  ) +
  theme(legend.position = "none")
