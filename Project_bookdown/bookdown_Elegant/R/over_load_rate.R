df_complete0 %>% 
  select(school, id, test, hours_expect_per_week, hours_spent_per_week) %>% 
  summarise(
    hours_expect_per_week = mean(hours_expect_per_week),
    hours_spent_per_week = mean(hours_spent_per_week),
    over_load_rate = sum(hours_spent_per_week > hours_expect_per_week) / n() ) 


  df_complete0 %>% 
  select(school, id, test, hours_expect_per_week, hours_spent_per_week) %>% 
  summarise(
    hours_expect_per_week = mean(hours_expect_per_week),
    hours_spent_per_week = mean(hours_spent_per_week),
    over_load_rate = sum(hours_spent_per_week > hours_expect_per_week) / n() ) %>%
  mutate_at(vars(starts_with("hours_")), round, 2) %>% 
  mutate_at(vars(over_load_rate), scales::percent, accuracy = 0.01)
