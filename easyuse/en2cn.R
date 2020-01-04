library(tidyverse)

en2cn <- tibble::tribble(
         ~en,    ~cn,
      "girl",   "文科",
       "boy",   "理科",
     "class", "班级层面",
    "school", "学校层面",
     "total",   "总分",
      "chin",   "语文",
      "math",   "数学",
   "english",   "英语",
  "politics",   "政治",
   "history",   "历史",
       "geo",   "地理",
       "phy",   "物理",
      "chem",   "化学",
       "bio",   "生物"
  )

en2cn

v <- en2cn %>% deframe()
v["boy"]

glue::glue(v["boy"], "-", v["class"], "-", v["geo"])

c("total", "chin", "math", "english", "phy", "chem", "bio") %>%
  map(
    ~glue::glue(v["boy"], "-", v["class"], "-", v[.x])
  )
