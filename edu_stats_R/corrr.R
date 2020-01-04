# 学校层面的总统计 与 学习成绩
# 相关系数矩阵
library(corrr)
df_complete2 %>%
  ungroup() %>%
  select(-school, -test_median_score) %>%
  correlate() %>%
  focus(test_mean_score) %>%
  arrange(-test_mean_score)
