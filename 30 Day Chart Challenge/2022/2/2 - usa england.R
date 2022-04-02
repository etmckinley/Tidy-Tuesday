library(tidyverse)
library(ggimage)
library(ggtext)

gc = read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTRdGyJp1-wdjrE6ufpNJpW5FQEhfB4Ws2ns0kW8UL2EucJgaYE2WGOihZxAwR-r-W57Ltmyzm9KOTc/pub?output=csv")

gc_long=gc %>% 
  pivot_longer(3:4)

gc_long %>% 
  ggplot(aes(x=Year, y=name))+
  geom_image(aes(image = value), asp = 3.5)+
  theme_minimal()+
  facet_wrap(~Competition, ncol=1)+
  labs(x="",
       y="",
       caption = "Plot: @etmckinley",
       subtitle = "<span style = 'font-size:32pt; font-family:Oswald;'>The United States is <span style = 'color:#3C3B6E;'>**undefeated**</span> against England in the World Cup: 1-0 victory in 1950, 1-1 draw in 2010",
       title = "<span style = 'font-size:60pt; font-family:Oswald;'>As measured by combined Gold Cups & Euros won, the United States<br>is a far superior international <span style = 'color:#B22234;'>**soccer**</span> team than England")+
  theme(text = element_text(size = 46, family="Oswald"),
        strip.text = element_text(size=50),
        plot.caption = element_text(size=14),
        plot.subtitle = element_markdown(),
        plot.title = element_markdown(),
        plot.margin = margin(.2,.2,.2,.2, unit="in"))

ggsave("./30 Day Chart Challenge/2022/2/usa england trophies.png", width=24, height=16, bg="white", dpi=200)
