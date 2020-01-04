library(tidyverse)
library(showtext)
showtext_auto()


df1 <- tibble::tribble(
  ~item, ~prop,
  "非常不符合",   0.034,
  "一般",  0.172,
  "比较符合",   0.069,
  "非常符合",  0.724
) 

###########################################################
# 使用默认的配色方案
df1 %>% 
  ggplot(aes(x = reorder(item, prop), y = prop, fill = item)) +
  geom_col() +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1), hjust = -0.1)) +
  coord_flip() + 
  scale_y_continuous(limits = c(0, 0.8), expand = c(0, 0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "",
       title = "学校积极开展了师德师风建设活动") +
  theme_classic(base_size = 18) +
  theme(legend.position = "none")
##########################################################




###########################################################
# 使用配色方案
library(ggthemr) 
ggthemr('dust')
# 查看当前使用的颜色
swatch()
cols <- swatch()
scales::show_col(cols)


df1 %>% 
  ggplot(aes(x = reorder(item, prop), y = prop, fill = item)) +
  geom_col() +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1), hjust = -0.1)) +
  coord_flip() + 
  scale_y_continuous(limits = c(0, 0.8), expand = c(0, 0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "",
       title = "学校积极开展了师德师风建设活动") +
  theme_classic(base_size = 18) +
  theme(legend.position = "none")
###########################################################





###########################################################
# 调整色彩顺序
scales::show_col(swatch())
cols <- swatch()[-1]
scales::show_col(cols)

df1 %>% 
  ggplot(
    aes(x = fct_reorder(item, prop), y = prop, fill = fct_reorder(item, -prop))
    ) +
  geom_col() +
  scale_fill_manual(values = cols) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1), hjust = -0.1)) +
  coord_flip() + 
  scale_y_continuous(limits = c(0, 0.8), expand = c(0, 0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "",
       title = "学校积极开展了师德师风建设活动") +
  theme_classic(base_size = 18) +
  theme(legend.position = "none")

ggthemr_reset()
###########################################################



###########################################################
# 自定义色彩
cols <-  c("#003f5c", "#444e86", "#955196", "#dd5182", "#ff6e54", "#ffa600")
scales::show_col(cols)


df1 %>% 
  ggplot(
    aes(x = fct_reorder(item, prop), y = prop, fill = fct_reorder(item, -prop))
  ) +
  geom_col() +
  scale_fill_manual(values = cols) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1), hjust = -0.1)) +
  coord_flip() + 
  scale_y_continuous(limits = c(0, 0.8), expand = c(0, 0),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "", y = "",
       title = "学校积极开展了师德师风建设活动") +
  theme_classic(base_size = 18) +
  theme(legend.position = "none")
###########################################################





###########################################################
# 批量生成图片
# 最优方案
df <- read_csv("data_csv.csv")

df %>% mutate(prop = prop / 100)
df %>% group_nest(question)

# 参考 <https://stackoverflow.com/questions/57378486/how-to-add-a-loop-to-make-multiple-two-group-scatter-plots-and-then-automatic>

library(ggthemr) 
ggthemr('dust')
cols <- swatch()[-1]


df %>% 
  mutate(prop = prop / 100) %>% 
  group_by(question) %>% 
  group_walk(
    ~ggsave(
      paste0(.y, '.pdf'), 
      
      ggplot(data = .x, 
             aes(x = fct_reorder(item, prop), 
                 y = prop, 
                 fill = fct_reorder(item, -prop))
        ) +
        geom_col() +
        scale_fill_manual(values = cols) +
        geom_text(aes(label = scales::percent(prop, accuracy = 0.1), hjust = -0.1),
                  size = 6) +
        coord_flip() + 
        scale_y_continuous(limits = c(0, 0.8), expand = c(0, 0),
                           labels = scales::percent_format(accuracy = 1)) +
        labs(x = "", y = "") +
        ggtitle(.y) +
        theme_classic(base_size = 18) +
        theme(legend.position = "none"),
      # theme(
      #   legend.position = "none",
      #   plot.title = element_text(size=20, face="bold"),
      #   axis.text.x = element_text(size = 14),
      #   axis.text.y = element_text(size = 14)
      #       ),
      
      width = 10, 
      height = 4,
      dpi = 200,
      device = 'pdf'
    ) 
  ) %>% 
  invisible()
###########################################################






###########################################################
# 生成png图

df %>% 
  mutate(prop = prop / 100) %>% 
  group_by(question) %>% 
  group_walk(
    ~ggsave(
      paste0(.y, '.png'), 
      
      ggplot(data = .x, 
             aes(x = fct_reorder(item, prop), 
                 y = prop, 
                 fill = fct_reorder(item, -prop))
      ) +
        geom_col() +
        scale_fill_manual(values = cols) +
        geom_text(aes(label = scales::percent(prop, accuracy = 0.1), hjust = -0.1),
                    size = 6) +
        coord_flip() + 
        scale_y_continuous(limits = c(0, 0.8), expand = c(0, 0.2),
                             labels = scales::percent_format(accuracy = 1)) +
        labs(x = "", y = "") +
        ggtitle(.y) +
        theme_classic(base_size = 18) +
        theme(legend.position = "none"),
          # theme(
          #   legend.position = "none",
          #   plot.title = element_text(size=20, face="bold"),
          #   axis.text.x = element_text(size = 14),
          #   axis.text.y = element_text(size = 14)
          #       ),
      
      width = 4, 
      height = 2,
      dpi = 300,
      device = 'png'
     ) 
    ) %>% 
  invisible()
###########################################################
  
  

  
  


###########################################################
# 使用group_split() + map()方案
# 不是最优方案
gap_plot <- function(.x) {
  ggplot(data = .x, 
         aes(x = fct_reorder(item, prop), 
             y = prop, 
             fill = fct_reorder(item, -prop))
  ) +
    geom_col() +
    geom_text(aes(label = scales::percent(prop, accuracy = 0.1), hjust = -0.1),
              size = 6) +
    coord_flip() + 
    scale_y_continuous(limits = c(0, 0.8), expand = c(0, 0),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(x = "", y = "") +
    ggtitle(.x$question) +
    theme_classic(base_size = 18) +
    theme(legend.position = "none")
  
}



df %>% 
  mutate(prop = prop / 100) %>% 
  group_split(question) %>% 
  map(~ gap_plot(.x)) %>% 
  iwalk( ~ ggsave(plot = .x,
                  filename = paste0(.y, ".png"),
                  type = 'cairo', width = 4, height = 2, dpi = 300)
  )
###########################################################