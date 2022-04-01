library(tidyverse)
library(worldfootballR)
library(gggibbous)

team_info=read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTiZfW7pSUWPttpHSMlAwgMyXwdAeLAW6HuoHwZa69FrNpfzqVkM_0DaeAveTG7hvbCSK-HBh31QxIM/pub?gid=95813594&single=true&output=csv")

big5_team_passing = fb_big5_advanced_season_stats(season_end_year= c(2022), stat_type= "passing_types", team_or_player= "team")

mls_team_passing =  get_season_team_stats(country = "USA", gender = "M", season_end_year = c(2021), tier = "1st", stat_type = "passing_types")


team_passing = bind_rows(big5_team_passing %>% filter(Team_or_Opponent == "team") %>%  select(Season_End_Year, Squad, Comp, Left=Left_Body, Right=Right_Body), 
                         mls_team_passing %>% filter(Team_or_Opponent == "team") %>%  select(Season_End_Year, Squad, Comp=Competition_Name, Left = Left_Body_Parts, Right=Right_Body_Parts)) %>% 
  mutate(total = Left + Right,
         ratio = Left/total)

comp_passing =bind_rows(big5_team_passing %>% select(Season_End_Year, Squad, Comp, Left=Left_Body, Right=Right_Body), 
                        mls_team_passing %>% filter(Team_or_Opponent == "Team") %>%  select(Season_End_Year, Squad, Comp=Competition_Name, Left = Left_Body_Parts, Right=Right_Body_Parts)) %>% 
  group_by(Comp) %>% 
  summarise(left = sum(Left),
            right = sum(Right),
            total = left+right,
            ratio = left/total)


  

limits=c(0,2)  
  
team_passing %>% 
  filter(Comp == "Premier League") %>% 
  left_join(team_info %>% select(Squad, Primary, Secondary)) %>% 
  #bind_rows(team_passing %>% 
  #            filter(Comp == "Premier League") %>% 
  #            mutate(ratio=1)) %>% 
  ggplot()+
  geom_moon(aes(ratio=ratio, fill=Secondary), x=1, y=1, right=FALSE, size=60)+
  geom_moon(aes(ratio=1-ratio, fill=Primary), x=1, y=1, right=TRUE, size=60)+
  geom_text(aes(label = paste0(format(round(ratio*100,1), nsmall=1), "%")), x=1.25, y=1, size=7, family="Oswald", color="white")+
  coord_cartesian(xlim=limits, ylim=limits)+
  facet_wrap(~reorder(Squad,-ratio))+
  scale_fill_identity()+
  theme_void()+
  theme(legend.position = "none",
        text=element_text(size=40, family = "Oswald"),
        plot.caption=element_text(size=14),
        plot.subtitle = element_text(size=18),
        plot.margin = margin(.2,.2,.2,.2, unit="in"))+
  labs(title= "What foot do Premier League teams pass with?",
       subtitle= "2021-2022 Premier League season\n",
       caption= "Plot: @etmckinley | data: StatsBomb via FBRef and {worldfootballr}")
  
ggsave("./30 Day Chart Challenge/2022/1/plot.png", bg="white", height=13, width=16)  
