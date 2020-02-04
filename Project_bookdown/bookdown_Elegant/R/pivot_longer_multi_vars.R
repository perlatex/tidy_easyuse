library(tidyverse)
df_all <- list(df_complete1, df_complete2, df_complete3) %>% 
  reduce(bind_rows) 

df_all %>% 
  filter(school == set_schoolname | school == "全区")

    # 是先gather()，然后mutate(value - last(value))
    # 还是先mutate(value - last(value)) 然后在gather()


# 先gather()，然后mutate(value - last(value)) 推荐
df_all %>%
  select(-group,
         -mean_test_score,
         -mean_hours_spent_per_week, 
         -mean_hours_expect_per_week, 
         -mean_score_rate) %>%
  pivot_longer(
    cols = ontime_percent:extra_lessons_percent,
    names_to = "index", values_to = "values"
  ) %>%
  group_by(index) %>%
  mutate(values_diff = values - last(values)) 



# 先mutate(value - last(value)) 然后在gather()
df_all %>% 
  filter(school == set_schoolname ) %>% 
	mutate_at(vars(ends_with("_percent")), 
			  list(RC = ~. - last(.) )   
			  ) %>% 
	pivot_longer(contains("_percent"),
				 names_to = c( "set", ".value"),
				 names_pattern = "(.*?)(_percent_RC|_percent)"
	)

# ontime   percent    diff_with_mean
# sport    percent    diff_with_mean
# music    percent    diff_with_mean


df_all %>% 
  filter(school == set_schoolname ) %>% 
  select(-school,
         -mean_test_score,
         -mean_hours_spent_per_week, 
         -mean_hours_expect_per_week, 
         -mean_score_rate) %>%
	mutate_at(vars(ends_with("_percent")), 
			  list(RC = ~. >= last(.) )
			  ) %>% 
	pivot_longer(contains("_percent"),
				 names_to = c( "set", ".value"),
				 names_pattern = "(.*?)(_percent_RC|_percent)"
	)

# ontime   percent    TRUE/FALSE_above_mean
# sport    percent    TRUE/FALSE_above_mean
# music    percent    TRUE/FALSE_above_mean

