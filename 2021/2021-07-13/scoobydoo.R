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
  arrange(date_aired) %>%
  mutate(cumulative = ifelse(value == FALSE, 0, 1),
         cumulative = cumsum(cumulative))

captured %>% 
  ggplot(aes(x=index, y=cumulative))+
  geom_path(aes(color=name))
