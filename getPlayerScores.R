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
	Sys.sleep(sample(seq(0.5, 1.2, by=0.001), 1)) #scrape responsibly!
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
