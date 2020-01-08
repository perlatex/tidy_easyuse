library(tidyverse)

df <- tribble(
	~id, ~x, ~y, ~z, ~g,
	#--|--|--|--|--
	"a", 13.1, 14, 4, 1,
	"b", 15.2, 7, 5, 0,
	"c", 12.5, 10, 1, 0,
	"d", 20, 11, 3, 1
)
cutoffs <- list(
	x = 13,
	y = 12,
	z = 3,
	g = 1
)

wt <- c(
	x = 0.25,
	y = 0.25,
	z = 0.25,
	g = 0.25
)

df_wt <- df %>% 
	gather(var, val, x:g) %>%
	mutate(below_cutoff = as.integer(val < cutoffs[var])) %>%
	mutate(weighted_point = below_cutoff * wt[var]) %>%
	group_by(id) %>%
    summarise(weighted_sum = sum(weighted_point) )

df %>% left_join(df_wt)
	
	
	
	