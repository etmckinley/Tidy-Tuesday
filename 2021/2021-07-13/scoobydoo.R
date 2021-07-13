library(tidyverse)
library(ggrepel)
library(png)
library(grid)

tuesdata = tidytuesdayR::tt_load('2021-07-13')
scoobydoo = tuesdata$scoobydoo

#scooby doo font: https://www.dafont.com/scoobydoo.font


unmask = scoobydoo %>%
  select(index,
         series_name,
         season,
         title,
         date_aired,
         starts_with("unmask")) %>%
  mutate(unmask_other = as.character(unmask_other)) %>% 
  pivot_longer(starts_with("unmask")) %>%
  group_by(name) %>%
  arrange(date_aired) %>%
  mutate(cumulative = ifelse(value == TRUE, 1, 0),
         cumulative = cumsum(cumulative),
         name = str_to_title(gsub("unmask_", "", name)),
         name=case_when(name == "Daphnie"~"Daphne",
                        TRUE ~ name))


villain <- readPNG("./2021/2021-07-13/Pterodactyl_Ghost_New.png")


unmask %>% 
  arrange(date_aired) %>% 
  ggplot(aes(x=date_aired, y=cumulative))+
  annotation_custom(rasterGrob(villain),
                    ymin=35,
                    ymax=100,
                    xmin=as.Date("1930-01-01"))+
  geom_path(aes(color=name), size=1.25)+
  geom_text(data=. %>% group_by(name) %>% arrange(date_aired) %>% slice(n()) %>% 
              mutate(cumulative = case_when(name == "Daphne"~ cumulative+2,
                                            name == "Other"~ cumulative-1.5,
                     TRUE~cumulative)),
                  aes(color=name, label=name), hjust=0, family= "Scooby Doo",
            nudge_x = 100, size=6)+
  scale_x_date(limits=c(min(unmask$date_aired), max(unmask$date_aired)+1000))+
  labs(title= "Which Meddling Kid Unmasks the Villain?",
       x="Air Date",
       y= "Unmaskings",
       caption= "@etmckinley")+
  scale_color_manual(values=c("Fred" = "white", 
        "Daphne" = "#A091C6", 
        "Scooby" = "#B06E0E",
        "Shaggy" = "#79af30",
        "Velma" = "#F7921E",
        "Other" = "gray"))+
  theme(text=element_text(family= "Scooby Doo", color = "#D0D61B", size=24),
        panel.background = element_rect(fill="#020307", color="#020307"),
        plot.background = element_rect(fill="#020307", color="#020307"),
        legend.background = element_rect(fill="#020307", color="#020307"),
        axis.text.y =element_text(color="#D0D61B"),
        axis.text.x =element_text(color="#D0D61B"),
        axis.ticks = element_line(color="#6F6F6F"),
        panel.grid = element_line(color="#6F6F6F"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.caption = element_text(size=8))

ggsave(filename = "./2021/2021-07-13/tt_07-13-2021.png",width = 10, height=6)



