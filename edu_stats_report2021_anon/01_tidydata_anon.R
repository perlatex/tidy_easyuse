library(tidyverse)
library(stringr)


load("../edu_stats_report2021/data/myData_df5.Rdata")
load("../edu_stats_report2021/data/myData_df6.Rdata")



##########################################################
align5_text_fun <- purrr::compose(
  ~str_pad(.x, 2, pad = "0"),
  ~str_c("A", .x),    
  .dir = "forward"
)


df5_school_anon <- df5_start %>%
  distinct(school) %>% 
  mutate(title = row_number()) %>% 
  mutate(
    across(title, align5_text_fun)
  ) 

vec_df5_school_anon <- df5_school_anon %>% deframe() 
##########################################################





##########################################################
align6_text_fun <- purrr::compose(
  ~str_pad(.x, 2, pad = "0"),
  ~str_c("B", .x),    
  .dir = "forward"
)

df6_school_anon <- df6_start %>%
  distinct(school) %>% 
  mutate(title = row_number()) %>% 
  mutate(
    across(title, align6_text_fun)
  ) 

vec_df6_school_anon <- df6_school_anon %>% deframe() 
###################################################




###################################################
num_effect_df5_df6 <- df5_all %>% 
  select(school, starts_with("effect_num")) %>%
  full_join(df6_all %>% 
              select(school, starts_with("effect_num")),
            by = "school"
  ) %>% 
  mutate(school = recode(school, !!!vec_df5_school_anon ))
###################################################




###################################################
df5_all <- df5_all %>% 
  mutate(school = recode(school, !!!vec_df5_school_anon ))

df5_burden_percent_combine <- df5_burden_percent_combine %>% 
  mutate(school = recode(school, !!!vec_df5_school_anon ))
###################################################




###################################################
df6_all <- df6_all %>% 
  mutate(school = recode(school, !!!vec_df6_school_anon ))


df6_burden_percent_combine <- df6_burden_percent_combine %>% 
  mutate(school = recode(school, !!!vec_df6_school_anon ))
###################################################



#########################################################
# save to Rdata
save(
  df5_start,
  df5_scoring_rate,
  df5_all,
  df5_set,
  df5_burden_percent_combine,
  
  df6_start,
  df6_scoring_rate,
  df6_all,
  df6_set,
  df6_burden_percent_combine,
  
  pairs56,
  num_effect_df5_df6,
  file = "./data/myData_df56.Rdata"
)
#########################################################


