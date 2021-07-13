tuesdata = tidytuesdayR::tt_load('2021-07-13')
scoobydoo = tuesdata$scoobydoo

captured = scoobydoo %>%
  select(index,
         series_name,
         season,
         title,
         date_aired,
         starts_with("captured")) %>%
  pivot_longer(starts_with("captured")) %>%
  group_by(name) %>%
  arrange(index) %>%
  mutate(cumulative = ifelse(value == TRUE, 1, 0),
         cumulative = cumsum(cumulative))

captured %>% 
  ggplot(aes(x=index, y=cumulative))+
  geom_path(aes(color=name))

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
  arrange(index) %>%
  mutate(cumulative = ifelse(value == TRUE, 1, 0),
         cumulative = cumsum(cumulative))

unmask %>% 
  ggplot(aes(x=date_aired, y=cumulative))+
  geom_path(aes(color=name))
