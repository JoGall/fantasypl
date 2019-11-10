#' @import dplyr
#' @importFrom jsonlite fromJSON
NULL
#' Get players picked each gameweek by each FPL manager
#' @description Get total points scored across the season so far by one or more FPL managers
#' 
#' @param IDs a numeric with one or more team IDs
#' @param gameweeks range of gameweeks to retrieve picks for; if `NULL`, retrieves all gameweeks in season so far. 
#' @return a dataframe with each manager's team's information and total points
#' @details Team ID can be found when examining points on the official FPL
#' site, appearing after the `/entry/` part of the URL, e.g. `1` in 
#' `https://fantasy.premierleague.com/entry/1/event/12`
#' @examples
#' # first two gameweeks for two manager IDs
#' manager_picks(c(1, 2), 1:2)
#' 
#' @export
manager_picks <- function(IDs, gameweeks = NULL) {
  
  # get all gameweeks in season so far if none specified
  if(is.null(gameweeks)) {
    max_gameweek <- fromJSON("https://fantasy.premierleague.com/api/fixtures/") %>% 
      filter(finished == TRUE) %>% 
      pull(event) %>% 
      max
    
    gameweeks <- 1:max_gameweek
  }
  
  lapply(IDs, function(x) {
    print(paste0("ID: ", x))
    lapply(gameweeks, function(y) {
      print(paste0("Gameweek ", y, " out of ", length(gameweeks)))
      Sys.sleep(sample(seq(0.8, 1.5, by=0.001), 1)) #scrape responsibly!	
      url <- paste0("https://fantasy.premierleague.com/api/entry/", x, "/event/", y, "/picks/")
      df <- fromJSON(url)$picks
      data.frame(manager_id = x, gameweek = y, df)
    } ) %>%
      plyr::rbind.fill() #flatten list to df
  } ) %>%
    plyr::rbind.fill() #flatten list to df

}