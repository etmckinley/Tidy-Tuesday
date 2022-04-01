#Tidy Tuesday for 2022-01-11
#Eliot McKinley
#etmckinley@gmail.com


library(tidyverse)
library(ggrepel)
library(png)
library(geofacet)


bees = tidytuesdayR::tt_load('2022-01-11')
colony = bees$colony
stressor = bees$stressor

us_stressor = stressor %>% 
  filter(state == "United States",
         months != "2019")

stressor %>% 
  filter(months != "2019",
         stressor == "Varroa mites") %>% 
  
  ggplot(aes(x=months, y = stress_pct, group = year, color = year))+
  geom_point()+
  geom_line()+
  facet_geo(~state)

a= stressor %>% 
  filter(months != "2019") %>% 
  group_by(months, state, year) %>% 
  summarise(stressed = sum(stress_pct, na.rm=T))
