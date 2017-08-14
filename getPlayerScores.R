library(jsonlite)
library(dplyr)

#get player names and positions
player_details <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$elements %>%
  select(id, name = web_name, pos_code = element_type, team_code = team)

pos_key <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$element_types
team_key <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$teams

player_details$pos <- pos_key[match(player_details$pos_code, pos_key$id), "singular_name"]
player_details$team <- team_key[match(player_details$team_code, team_key$id), "name"]

#get player stats for each gameweek
n_elements <- max(fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$elements$id)

player_bygw <- lapply(1:n_elements, function(x) {
  print(paste0(x, " out of ", n_elements))
	Sys.sleep(sample(seq(0.4, 1, by=0.001), 1)) #scrape responsibly!
	url <- paste0("https://fantasy.premierleague.com/drf/element-summary/", x)
	df <- fromJSON(url)$history
	data.frame(player_id = x, df[,c(7, 2, 54, 6, 4, 5, 9:13, 8, 16:51)])
} ) %>%
plyr::rbind.fill() 	#flatten list to df

#merge dataframes
player_bygw <- data.frame(player_bygw,
           player_details[match(player_bygw$player_id, player_details$id), c("name", "team", "pos")])

#reformat dataframe
player_bygw$opponents <- team_key[match(player_bygw$opponent_team, team_key$id), "name"]
player_bygw$venue <- ifelse(player_bygw$was_home, "H", "A")
player_bygw$team_goals <- ifelse(player_bygw$venue == "H", player_bygw$team_h_score, player_bygw$team_a_score)
player_bygw$opposition_goals <- ifelse(player_bygw$venue == "A", player_bygw$team_a_score, player_bygw$team_h_score)
player_bygw$value <- player_bygw$value / 10

player_bygw <- player_bygw[,c(1, 50:52, 2, 53:54, 3, 55:56, 13:49, 8:12)]

rownames(player_bygw) <- NULL

#add variable for fixture number
player_bygw <- player_bygw %>%
  arrange(player_id, kickoff_time) %>%
  group_by(player_id) %>%
  mutate(game_id = seq_len(n()))

# write.csv(player_bygw, "~/Dropbox/github/FantasyPL/17-18.csv")

# plot
player_bygw <- addTeamColours(player_bygw)
player_bygw$team <- as.factor(player_bygw$team)
team_colours <- player_bygw %>% group_by(team) %>% filter(row_number()==1) %>% select(team, col1, col2)
player_bygw <- transform(player_bygw, pos = factor(pos, levels=c("Goalkeeper", "Defender", "Midfielder", "Forward")))
                         
ggplot(subset(player_bygw, minutes > 0), aes(x = bps, y = round(total_points / value / 10, 2))) +
  geom_point(alpha = 0.4) +
  ylab("Points per £m") +
  xlab("Bonus points system") +
  geom_label_repel(
    aes(label = name, fill = team, colour = team),
    segment.colour = "black",
    fontface = 'bold',
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines")
  ) +
  scale_fill_manual(values = team_colours$col1, breaks = unique(player_bygw$team)) +
  scale_colour_manual(values = team_colours$col2, breaks = unique(player_bygw$team)) +
  my_theme +
  facet_wrap(~pos)


player_details <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$elements

pos_key <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$element_types
team_key <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$teams

player_details$pos <- pos_key[match(player_details$element_type, pos_key$id), "singular_name"]
player_details$team <- team_key[match(player_details$team, team_key$id), "name"]


player_details <- matchTeamnames(player_details)
player_details <- addTeamColours(player_details)
player_details$team <- as.factor(player_details$team)
player_details <- transform(player_details, pos = factor(pos, levels=c("Goalkeeper", "Defender", "Midfielder", "Forward")))

team_colours <- player_details %>% group_by(team) %>% filter(row_number()==1) %>% select(team, col1, col2)

top10 <- player_details %>% group_by(pos) %>% arrange(desc(bps)) %>% head(10)

ggplot(subset(player_details, minutes > 0), aes(x = bps, y = round(total_points / (now_cost / 10), 2))) +
  geom_point(alpha = 0.7) +
  ylab("Points per £m") +
  xlab("Bonus points system") +
  geom_label_repel(
    data = top10,
    aes(label = web_name, fill = team, colour = team),
    segment.colour = "black",
    fontface = 'bold',
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines")
  ) +
  scale_fill_manual(values = c("#0053A0", "#DA020E", "#53162F", "white", "white", "white", "white", "white", "white"), breaks = unique(top10$team)) +
  scale_colour_manual(values = top10$col2, breaks = top10$team) +
  my_theme +
  facet_wrap(~pos)

