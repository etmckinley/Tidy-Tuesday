library(tidyverse)
library(ggdist)

olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

medals = olympics %>% 
  group_by(event, year) %>% 
  summarise(N=n(),
            medals=sum(!is.na(medal))) %>% 
  filter(year>1980) %>% 
  mutate(percent_medal = medals/N)


olympics %>%
  filter(year >=1992,
         season == "Summer",
         !is.na(age)) %>% 
  ggplot(aes(x = fct_reorder(sport, age, median), y = age)) + 
  
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA
  ) +
  geom_boxplot(
    width = .1, 
    outlier.shape = NA
  ) +
  ## add justified jitter from the {gghalves} package
  gghalves::geom_half_point(
    ## draw jitter on the left
    side = "l", 
    ## control range of jitter
    range_scale = .4, 
    ## add some transparency
    alpha = .3
  ) +coord_flip()
  




  ggplot(aes(
    x = sport, dist = dist_normal(mean, sd), fill = group, color = group
  )) +
  stat_dist_halfeye(position = "dodge", color = "black") +
  geom_rect(
    aes(xmin = 1, xmax = 2, ymin = -3, ymax = 7),
    position = "dodge",
    alpha = 0.1
  ) +
  geom_point(
    aes(y = 0),
    position = position_dodge(width = 1),
    shape = 1, size = 4, stroke = 1.5
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2")

