######################################################################
font_add("heiti", "simhei.ttf")
font_add("simsun", "simfang.ttf", italic = "simfang.ttf") #仿宋
# font_families() #check add successly

report_theme <- theme(
  legend.position = "none",
  plot.caption = element_text(family = "simsun", face = "italic")
)

######################################################################


