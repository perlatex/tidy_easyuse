# usage

# source(workflow_tidydata.R)         # -> "data/myData_df.Rdata"
# source(workflow_tidydata_anon.R)    # -> "data/myData_df_anon.Rdata"

# 问题与解决
# run workflow_render.R within *.Rproj
# 不知道为什么每次更新内容，再次运行render_book，都需要**Restarting R session**
# 即 Session -> Restart R
# 否则会报错：LaTeX Error: Environment tabu undefined.
# 看<http://haozhu233.github.io/kableExtra/awesome_table_in_pdf.pdf>
# 在index.Rmd文件yaml处加入\usepackage{tabu}\usepackage{colortbl}解决
# 现在直接Source即可


#######################################################
if (fs::file_exists("_main.Rmd")) {
  fs::file_delete("_main.Rmd")
}
#######################################################



#######################################################
bookdown::render_book("index.Rmd", "bookdown::pdf_book",
                      params = list(
                        set_data_from1 = "data/myData_df5.Rdata",
                        set_data_from2 = NULL
                      ),
                      output_file = "elegantbookdown.pdf"
)
#######################################################




#######################################################
# 重启会话
# Session -> Restart R
#######################################################





#######################################################
bookdown::render_book("index.Rmd", "bookdown::pdf_book",
                      params = list(
                        set_data_from1 = "data/myData_df56_anon.Rdata",
                        set_data_from2 = NULL
                      ),
                      output_file = "elegantbookdown_anon.pdf"
)
#######################################################

