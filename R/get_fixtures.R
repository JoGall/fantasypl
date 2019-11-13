#' @import dplyr
#' @importFrom jsonlite fromJSON
NULL
#' Get PL fixtures played and/or yet to play
#' @description Get EPL fixtures played and/or yet to play in season, including fixture difficulty as estimated by the official FPL site.
#'
#' @param IDs a numeric with one or more team IDs
#' @param gameweeks range of gameweeks to retrieve picks for; if `NULL`, retrieves all gameweeks in season so far.
#' @return a dataframe with two rows per fixture, one for the home team and one for the away team, including fixture difficulty (`difficulty`; scale of 1 - 5) and completed status (`finished`; TRUE or FALSE).
#' @details Team ID can be found when examining points on the official FPL site, appearing after the `/entry/` part of the URL, e.g. `1` in `https://fantasy.premierleague.com/entry/1/event/12`
#' @examples
#' # get completed fixtures
#' get_fixtures() %>%
#'   filter(finished == TRUE)
#'
#' @export
get_fixtures <- function() {

  # get team names
  fpl_json <- fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")
  team_key <- fpl_json$teams

  # get fixtures and add team names
  fixtures <- fromJSON("https://fantasy.premierleague.com/api/fixtures/")

  # keep variables of interest
  fixtures <- fixtures %>%
    select(gamweek = event, home = team_h, away = team_a, home_difficulty = team_h_difficulty, away_difficulty = team_a_difficulty, finished, home_score = team_h_score, away_score = team_a_score)

  # join team names
  fixtures$home <- team_key[match(fixtures$home, team_key$id), "name"]
  fixtures$away <- team_key[match(fixtures$away, team_key$id), "name"]

  return(fixtures)

}
