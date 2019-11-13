#' @import dplyr
#' @importFrom jsonlite fromJSON
NULL
#' Get total points for each FPL manager
#' @description Get total points scored across the season so far by one or more FPL managers
#'
#' @param IDs a numeric with one or more team IDs
#' @return a dataframe with each manager's team's information and total points
#' @details Team ID can be found when examining points on the official FPL
#' site, appearing after the `/entry/` part of the URL, e.g. `1` in
#' `https://fantasy.premierleague.com/entry/1/event/12`
#' @examples
#' manager_points(c(1, 2))
#'
#' @export
manager_points <- function(IDs) {

  lapply(IDs, function(x) {
    print(paste0("ID: ", x))

    url <- paste0("https://fantasy.premierleague.com/api/entry/", x, "/")
    info_list <- fromJSON(url)

    data.frame(manager_id = info_list$id,
               manager_name = paste(info_list$player_first_name, info_list$player_last_name),
               team_name = info_list$name,
               total_pts = info_list$summary_overall_points)

  } ) %>%
    plyr::rbind.fill() #flatten list to df

}
