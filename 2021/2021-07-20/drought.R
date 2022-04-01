#Tidy Tuesday for 2021-07-20
#Eliot McKinley
#etmckinley@gmail.com

library(tidyverse)
library(geofacet)
library(ggpubr)


#data munging
area = read_csv("./2021/2021-07-20/dm_export_20000720_20210720 perc area.csv") %>% 
  mutate(severe= D2+D3+D4,
         non_severe=None +D0+D1) %>% 
  pivot_longer(c(None,D0,D1,D2,D3,D4, severe, non_severe), names_to = "drought_level", values_to = "perc_area")
  

population = read_csv("./2021/2021-07-20/dm_export_20000720_20210720 perc pop.csv") %>% 
  mutate(severe= D2+D3+D4,
         non_severe=None +D0+D1) %>% 
  pivot_longer(c(None,D0,D1,D2,D3,D4, severe, non_severe), names_to = "drought_level", values_to = "perc_pop")

drought=area %>% left_join(population) %>% 
  mutate(drought_level=fct_relevel(drought_level, "severe", "non_severe","None", "D0", "D1", "D2", "D3", "D4")) 

## plot one state first
ca=drought %>% 
  filter(StateAbbreviation == "CA")
  

ca %>% 
  filter(drought_level %in% c("severe", "non_severe")) %>% 
  ggplot(aes(x=ValidStart, y=perc_area, fill=drought_level))+
  geom_area()+
  scale_x_date(date_labels = "%y")+
  theme_minimal()+
  labs(x="Year", y="")+
  theme(axis.text.y = element_blank())+
  scale_fill_manual(values=c("non_severe" = "#0c7eb4", 
                             "severe" = "#e1aa70"))

# plot all in grid
p=drought %>% 
  filter(drought_level %in% c("severe", "non_severe")) %>% 
  mutate(drought_level=case_when(drought_level =="severe"~"Severe Drought",
                                 TRUE~"No Drought to Moderate Drought")) %>% 
  ggplot(aes(x=ValidStart, y=perc_area, fill=drought_level))+
  geom_area()+
  scale_x_date(date_labels = "%y")+
  theme_minimal()+
  labs(x="Year", y="", title= "Percent Land Area in Severe Drought",
       caption = "Data: U.S. Drought Monitor @etmckinley")+
  theme(axis.text.y = element_blank(),
        legend.position = "top")+
  scale_fill_manual(values=c("No Drought to Moderate Drought" = "#0c7eb4", 
                             "Severe Drought" = "#e1aa70"))+
  facet_geo(~StateAbbreviation, grid="us_state_grid1")

ggsave("./2021/2021-07-20/drought_grid.png", plot=p, width=10, height=8)

p2=drought %>% 
  filter(drought_level %in% c("severe", "non_severe")) %>% 
  mutate(drought_level=case_when(drought_level =="severe"~"Severe Drought",
                                 TRUE~"No Drought to Moderate Drought")) %>% 
  ggplot(aes(x=ValidStart, y=perc_pop, fill=drought_level))+
  #background_image(png::readPNG("./2021/2021-07-20/drought fade.png"))+
  geom_area()+
  scale_x_date(date_labels = "%y")+
  theme_minimal()+
  labs(x="Year", y="", title= "Percent Population in Severe Drought",
       caption = "Data: U.S. Drought Monitor @etmckinley")+
  theme(axis.text.y = element_blank(),
        legend.position = "top")+
  scale_fill_manual(values=c("No Drought to Moderate Drought" = "#0c7eb4", 
                             "Severe Drought" = "#e1aa70"))+
  facet_geo(~StateAbbreviation, grid="us_state_grid2")

ggsave("./2021/2021-07-20/drought_grid_pop.png", plot=p2, width=1920, height=1280, units= "px")


