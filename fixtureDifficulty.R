library(jsonlite)

fixtures <- fromJSON("https://fantasy.premierleague.com/drf/fixtures/")
team_key <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$teams

d <- rbind(
  # home teams
  left_join(fixtures, 
          select(team_key, id, name),
          by = c("team_h" = "id")) %>% 
  select(event, team = name, difficulty = team_h_difficulty) %>% 
  mutate(isHome = TRUE),
  # away teams          
  left_join(fixtures, 
            select(team_key, id, name),
            by = c("team_a" = "id")) %>% 
    select(event, team = name, difficulty = team_a_difficulty) %>% 
    mutate(isHome = FALSE)
) %>% 
  arrange(event)

# plot
d %>% 
  filter(event %in% 11:15) %>% 
  group_by(team) %>% 
  summarise(difficulty = mean(difficulty)) %>% 
  ggplot(aes(reorder(team, -difficulty), difficulty)) +
  geom_bar(stat = "identity", width = 0.6) +
  coord_flip() +
  scale_y_continuous(lim = c(1,4.1), expand = c(0,0), oob = scales::rescale_none) +
  theme_bw(base_size = 18, base_family = "Roboto Condensed") +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank()) +
  labs(subtitle = "FPL fixture difficulty, GWs 11-15")
