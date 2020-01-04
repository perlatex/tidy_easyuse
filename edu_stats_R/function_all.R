## ------------------------------------------------------------------------
# 					schoolname    ~group_attr
# - df_complete_0 人    ..
# - df_complete_1 班    ..         class
# - df_complete_2 校    ..         school
# - df_complete_3 区   全区        district


# 某个学校的原始数据
df_seven_degree_one_school <- 
	df_complete0 %>% 
	filter(school == set_schoolname) 



# 统计出的18个变量： school, group, 12个_percent, 4个mean_
#  df_complete_1
#  df_complete_2
#  df_complete_3
df_all <- 
	list(df_complete1, df_complete2, df_complete3) %>% 
	reduce(bind_rows) 



#  df_complete_1 (set_schoolname)
#  df_complete_2 (set_schoolname)
#  df_complete_3 (全区)
df_all %>%
  filter(school == set_schoolname | school == "全区")



#  df_complete_2 (set_schoolname)
#  df_complete_3 (全区)
df_all %>%
  filter(school == set_schoolname | school == "全区") %>%
  filter(group %in% c("school", "district"))


#  df_complete_2 (set_schoolname)
df_all %>%
	filter(school == set_schoolname) %>%
	filter(group %in% c("school"))


## ------------------------------------------------------------------------
index <- df_all %>% 
  filter(school == set_schoolname | school == "全区") %>% 
  filter(group %in% c("school", "district")) %>% 	
  mutate_at(vars(ends_with("_percent")), 
			list(RC = ~. >= last(.) )
		    ) %>%
  mutate(num_above_mean = pmap_dbl(select(., ends_with("_percent_RC")), sum)) %>% 
  pull(num_above_mean) %>% 
  .[1]



## ------------------------------------------------------------------------
index_which <- df_all %>% 
	filter(school == set_schoolname | school == "全区") %>% 
	filter(group %in% c("school", "district")) %>% 
	select(ends_with("_percent")) %>% 
	select_if( ~first(.) < last(.)) %>% 
	colnames()

index_string <- labels[index_which] %>% unname() %>% stringr::str_c(collapse = ", ")





## ------------------------------------------------------------------------
times <- df_all %>%
  filter(school == set_schoolname) %>%
  filter(group %in% c("school"))

time_s <- times %>% pull(mean_hours_spent_per_week) %>% round(2)
time_e <- times %>% pull(mean_hours_expect_per_week) %>% round(2)





## ------------------------------------------------------------------------
df_labels_t35 <- questionnaire_labels %>% 
    filter(index ==  "t35") %>% 
    select(option, ssn) 


mean_pressure <- 
	df_seven_degree_one_school %>% 
	select(t35) %>% 
	left_join(df_labels_t35, by = c("t35" = "option")) %>% 
	mutate(pressure = as.numeric(ssn) ) %>% 
	summarise(mean_pressure = mean(pressure) %>% round(2))  %>% 
	pull(mean_pressure)









## ------------------------------------------------------------------------
deep_learn <- df_all %>% 
   filter(school == set_schoolname | school == "全区") %>% 
   filter(group %in% c("school", "district")) %>% 
   select(school, group, mean_score_rate) %>% 
   pull(mean_score_rate)
   
deep_learn_school    <- deep_learn[1]
deep_learn_district  <- deep_learn[2]
   




## ------------------------------------------------------------------------
test_score <- df_all %>% 
	filter(school == set_schoolname | school == "全区") %>% 
	filter(group %in% c("school", "district")) %>% 
	select(school, group, mean_test_score) %>% 
	pull(mean_test_score)


test_score_school    <- test_score[1] %>% round(2)
test_score_district  <- test_score[2] %>% round(2)





## ------------------------------------------------------------------------
df_labels_t35 <- questionnaire_labels %>% 
    filter(index == "t35") %>% 
    select(option, ssn) 


df_pressure_test <- df_seven_degree_one_school %>% 
	select(t35, test) %>% 
	group_by(t35) %>% 
	summarise(mean_test = mean(test)) %>% 
	ungroup() %>% 
	left_join(df_labels_t35, by = c("t35" = "option")) %>% 
	mutate(pressure = as.numeric(ssn))


p_value <- df_pressure_test %>% 
	lm(mean_test ~ pressure, data = .) %>% 
	broom::glance() %>% 
	pull(p.value)






