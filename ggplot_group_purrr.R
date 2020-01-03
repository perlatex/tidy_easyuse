library(tidyverse)


nobel_winners %>% 
  group_split(category) %>%
  map(
    ~ ggplot(data = .x, aes(x = prize_age)) +
      geom_density() +
      ggtitle(.x$category)
  )


nobel_winners %>% 
  group_by(category) %>%
  group_map(
    ~ ggplot(data = .x, aes(x = prize_age)) +
      geom_density() +
      ggtitle(.y)
  )



nobel_winners %>% 
  group_split(category) %>%
  group_walk(
    ~ ggsave(
      paste0(.y, '.png'), 
      ggplot(
        .x,
        aes(
          x      = TimePoint,
          y      = Readings,
          colour = Scale,
          group  = Scale
        )
      ) +
        geom_line() +
        geom_point(size = 1.5) +
        facet_wrap( ~ Groups, nrow = 1) +
        ggthemes::theme_few() +
        scale_x_continuous(
          breaks = c(42.5, 47.5, 52.5, 57.5, 62.5, 67.5, 72.5),
          labels = c("1", "2", "3", "4", "5", "6", "7")
        ) +
        scale_color_viridis_c(breaks = c(5, 10, 25, 50, 75, 90)) +
        theme(legend.key.height = unit(0.155, 'npc')) +
        ggtitle(.y), 
      device = 'png',
      path = temp
    )
  ) %>% 
  invisible()