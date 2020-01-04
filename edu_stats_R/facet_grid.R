index <- tibble::tribble(
  ~index, ~option, ~index_c, ~option_c,
  "t29", "A", "负担原因", "学校的家庭作业",
  "t29", "B", "负担原因", "课外班的学习",
  "t29", "C", "负担原因", "都没有负担",
  "t29", "D", "负担原因", "加到一起才有负担",
  "t33", "A", "压力大小", "没有压力，感觉还比较轻松",
  "t33", "B", "压力大小", "有一些压力，但还是喜欢学习",
  "t33", "C", "压力大小", "压力很大，但自己必须坚持",
  "t33", "D", "压力大小", "压力难以承受，想放弃学习",
  "t34", "A", "压力来源", "同学",
  "t34", "B", "压力来源", "父母",
  "t34", "C", "压力来源", "老师",
  "t34", "D", "压力来源", "自己",
  "t35", "A", "父母支持", "忙工作、忙家务",
  "t35", "B", "父母支持", "陪伴学习",
  "t35", "C", "父母支持", "不在家",
  "t35", "D", "父母支持", "玩手机、看电视、睡觉等休闲活动"
)


df_index_c <- df_four_degree %>%
  select(school, t33, t29, t34, t35) %>%
  gather(index, option, -school) %>%
  group_by(school, index) %>%
  count(option) %>%
  mutate(percent = n / sum(n)) %>%
  left_join(index, by = c("index", "option"))




df_index_c %>%
  ggplot(aes(x = option_c, y = percent, fill = option_c)) +
  geom_col(width = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  #scale_y_percent() +
  facet_grid(vars(school), vars(index_c), scales = "free", switch = "y") +
  labs(
    x = NULL, y = NULL,
    title = "各校各项指标完成比例",
    subtitle = "数据来源问卷调查: 红色代表高于平均分，蓝色代表低于平均分"
  ) +
  theme(
    legend.position = "none",
    strip.text.y = element_text(angle = 180),
    strip.placement = "outside"
  )
