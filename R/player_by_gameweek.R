#' @import dplyr
#' @importFrom jsonlite fromJSON
NULL
#' Get points scored each gameweek for each player
#' @description Get gameweek-by-gameweek statistics for all players using the official FPL API.
#'
#' @return a dataframe with {n of gameweeks in season so far} rows per player containining all summary statistics related to player performance
#' @examples
#' player_points_by_gw()
#'
#' @export
player_points_by_gw <- function() {

  # get individual player stats for each gameweek
  n_elements <- max(fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$elements$id)

  player_bygw <- lapply(1:n_elements, function(x) {
    print(paste0(x, " out of ", n_elements))
    Sys.sleep(sample(seq(0.6, 1.5, by=0.005), 1)) #scrape responsibly!
    url <- paste0("https://fantasy.premierleague.com/api/element-summary/", x, "/")
    df <- fromJSON(url)$history
    if(length(df)) {
      data.frame(player_id = x, df)
    }
  } ) %>%
    plyr::rbind.fill() 	#flatten list to df

  # get all player names, teams, and positions
  player_details <- fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$elements %>%
    select(id, name = web_name, pos_code = element_type, team_code = team)

  pos_key <- fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$element_types
  team_key <- fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")$teams

  player_details$pos <- pos_key[match(player_details$pos_code, pos_key$id), "singular_name"]
  player_details$team <- team_key[match(player_details$team_code, team_key$id), "name"]

  player_bygw <- data.frame(player_bygw,
                            player_details[match(player_bygw$player_id, player_details$id), c("name", "team", "pos")])

  # get fixture difficulty
  fixtures <- fromJSON("https://fantasy.premierleague.com/api/fixtures/")

  player_bygw <- left_join(player_bygw,
                           fixtures %>% select(id, team_h_difficulty, team_a_difficulty),
                           by = c("fixture" = "id"))

  player_bygw <- player_bygw %>%
    mutate(difficulty = if_else(was_home, team_h_difficulty, team_a_difficulty)) %>%
    select(-team_h_difficulty, -team_a_difficulty)


  # reformat dataframe
  player_bygw$opponents <- team_key[match(player_bygw$opponent_team, team_key$id), "name"]
  player_bygw$venue <- ifelse(player_bygw$was_home, "H", "A")
  player_bygw$team_goals <- ifelse(player_bygw$venue == "H", player_bygw$team_h_score, player_bygw$team_a_score)
  player_bygw$opposition_goals <- ifelse(player_bygw$venue == "A", player_bygw$team_a_score, player_bygw$team_h_score)
  player_bygw$value <- player_bygw$value / 10

  # add variable for fixture number
  player_bygw <- player_bygw %>%
    arrange(player_id, kickoff_time) %>%
    group_by(player_id) %>%
    mutate(game_id = seq_len(n()))

  return(player_bygw)

}
