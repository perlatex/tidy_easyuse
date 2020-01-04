tb2019 %>% 
  left_join(tb2018, by = c("school", "index")) %>% 
  mutate(rate_diff = rate2019 - rate2018) %>% 
  ggplot(aes(x = factor(index, levels),
             y = forcats::fct_relevel(school, "全区"),
             size = rate_diff, 
             color = (rate_diff > 0) 
             )) +
  geom_point() +
  geom_text(aes(label = round(rate_diff, 2)), color = "white" , size = 2) +
  scale_x_discrete(labels = labels) +
  scale_colour_manual(values = c("blue", "red")) +
  scale_size_continuous(range = c(0, 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = NULL, y = NULL,
    title = "各校各项指标与去年情况对比",
    subtitle = "数据来源问卷调查: 红色代表高于去年，蓝色代表低于去年"
  ) +
  theme(legend.position = "none")
