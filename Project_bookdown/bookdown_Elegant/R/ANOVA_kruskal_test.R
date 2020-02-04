## 方差分析
## 学生压力大小与学习方法得分率绩的分组差异检验
# - https://towardsdatascience.com/a-gentle-guide-to-statistics-in-r-ccb91cc1177e
# - http://www.sthda.com/english/wiki/one-way-anova-test-in-r
# - https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
# - https://www.guru99.com/r-anova-tutorial.html
# - http://www.flutterbys.com.au/stats/tut/tut7.4a.html

# 单因素方差（One-way ANOVA）分析
df_aov <- df_four_degree %>% 
	select(t10, t33, t34, test) %>%
	mutate_at(vars(t10, t33, t34), ~forcats::fct_relevel(., "A")) 

fit <- df_aov %>% 
	aov(test ~ t10 , data = .) 

fit %>% broom::tidy()

summary(fit)






# call and save the pair.t.test
ad_pairwise <- pairwise.t.test(
  df_four_degree$test,
  df_four_degree$t33:df_four_degree$t34,
  p.adj = "none")

# tidy the post hoc
tidy_ad_pairwise <- broom::tidy(ad_pairwise)





# call and tidy the tukey posthoc
tidy_ad_tukey <- 
	TukeyHSD(fit ) %>% 
	tidy()

tidy_ad_tukey




# # Extract the residuals
# aov_residuals <- residuals(object = fit )
# # Run Shapiro-Wilk test
# shapiro.test(x = aov_residuals )



# 单因素方差分析中，如果变量下的分组，不是正态分布，
# 用kruscal-wallis秩和检验
kruskal.test(test ~ t10 , data = df_aov) 


