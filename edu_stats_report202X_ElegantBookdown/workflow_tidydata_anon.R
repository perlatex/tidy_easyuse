library(tidyverse)
library(stringr)


load("./data/myData_df5.Rdata")



###################################################
anon_school_code <- 
  readxl::read_excel("./data/anon_code.xls") %>% 
  select(school, code) %>% 
  deframe() 
anon_school_code
###################################################





###################################################
df5_all <- df5_all %>% 
  mutate(school = recode(school, !!!anon_school_code))



df5_burden_percent_combine <- df5_burden_percent_combine %>% 
  mutate(school = recode(school, !!!anon_school_code))
###################################################







#########################################################
# save to Rdata
save(
  df5_start,
  df5_scoring_rate,
  df5_all,
  df5_set,
  df5_burden_percent_combine,
 
  pairs56,
  file = "./data/myData_df56_anon.Rdata"
)
#########################################################


