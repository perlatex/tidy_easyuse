# usage

# source(workflow_tidydata.R)         # -> "data/myData_df.Rdata"
# source(workflow_tidydata_anon.R)    # -> "data/myData_df_anon.Rdata"

# run workflow_render.R within Elegantbookdown.Rproj
# 不知道为什么每次更新内容，再次运行render_book，都要重启 Elegantbookdown.Rproj才行
# 否则会报错：LaTeX Error: Environment tabu undefined.


#######################################################
if (fs::file_exists("elegantbookdown.Rmd")) {
  fs::file_delete("elegantbookdown.Rmd")
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
bookdown::render_book("index.Rmd", "bookdown::pdf_book",
                      params = list(
                        set_data_from1 = "data/myData_df56_anon.Rdata",
                        set_data_from2 = NULL
                      ),
                      output_file = "elegantbookdown_anon.pdf"
)
#######################################################

