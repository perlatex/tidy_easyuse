# rmarkdown::render("main_reports.Rmd", params = list(
#   school = "scinu",
#   threshold = 10
# ))
library(tidyverse)

render_report = function(set_title, school, threshold) {
  rmarkdown::render(
    "main_reports.Rmd", params = list(
      set_title = set_title,
      school = school,
      threshold = threshold
    ),
    output_file = paste0("Report-", school, "-", threshold, ".pdf")
  )
}


render_report(set_title = "川师附小", school = "scinu", threshold = 10)
render_report(set_title = "川师附小", school = "scinu", threshold = 12)

df <- tribble(
  ~set_title, ~school, ~ threshold,
  "川师附小", "scinu", 10,
  "川师附小", "scinu", 12
)

df 
pmap(df, render_report)
