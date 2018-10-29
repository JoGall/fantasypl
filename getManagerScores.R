require(jsonlite)
require(dplyr)
require(plyr)

manager_IDs <- c(2022839, 1)

manager_scores <- getManagerScores(2022839)

#create a dataframe of player names and IDs
player_idx <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$elements[c("id", "web_name")]

#get player scores for each gameweek, e.g.:
player_scores <- read.csv('https://github.com/JoGall/FantasyPL/16-17-formatted.csv')
#or scrape yourself using getPlayerScores.R script

#get points for all managers
manager_scores %>%
  merge(., player_idx, by.x = c("element"), by.y = c("id")) %>%
  #add player scores
  merge(player_scores, by.x = c("element", "gameweek"), by.y = c("player_id", "round")) %>%
  mutate(player = web_name) %>%
  dplyr::select(manager, gameweek, position, player, total_points) %>%
  arrange(manager, gameweek, position)
