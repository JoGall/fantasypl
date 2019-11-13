#' @import dplyr
#' @importFrom jsonlite fromJSON
NULL
#' Get total points for each player
#' @description Get summary statistics of all players, totalled across all gameweeks in season so far using the official FPL API.
#'
#' @return a dataframe with one row per player containining all summary statistics related to player performance
#' @examples
#' player_points()
#'
#' @export
player_points <- function() {

  fpl_json <- fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")

  # get summarised player data
  player_details <- fpl_json$elements

  # add match player team and position to keys
  pos_key <- fpl_json$element_types
  team_key <- fpl_json$teams
  player_details$pos <- pos_key[match(player_details$element_type, pos_key$id), "singular_name"]
  player_details$team <- team_key[match(player_details$team, team_key$id), "name"]

  #change variable classes
  player_details$team <- as.factor(player_details$team)
  player_details$ict_index <- as.factor(player_details$ict_index)
  player_details <- transform(player_details, pos = factor(pos, levels=c("Goalkeeper", "Defender", "Midfielder", "Forward")))

  return(player_details)

}
