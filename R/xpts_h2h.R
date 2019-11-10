#' @import dplyr
#' @import fplscrapR
#' @importFrom jsonlite fromJSON
NULL
#' Calculate expected points for a head-to-head league table.
#' @description Calculate expected points (xPts) and expected rank (xRank) for
#' each team in a head-to-head table as the average points (i.e. 0, 1, 3)
#' expected against all teams in that league each gameweek (including AVERAGE 
#' team, present if odd number of teams in the league), not just the actual
#' opponent. Requires entering username and password in interactive dialogue
#' in order to access private leagues. Uses the [`fplscrapR`](https://github.com/wiscostret/fplscrapR).
#' 
#' @param leagueid an integer with a single valid head-to-head league ID
#' @return a dataframe with expected points (xPts) and expected rank (xRank)
#' added to actual points, actual rank, and team information
#' @details League ID can be found when examining the head-to-head table on the
#' official FPL site, appearing after the `/leagues/` part of the URL, e.g. `1`
#'in `https://fantasy.premierleague.com/leagues/1/standings/h`
#' @seealso `fplscrapR` package by [wiscostret](https://github.com/wiscostret/fplscrapR)
#' @examples
#' xpts_h2h(1)
#' 
#' @export
xpts_h2h <- function(leagueid) {
  
  # get actual league table
  actual_table <- get_league(leagueid, "h2h")$standings$results
  
  # get entryids
  eids <- actual_table$entry %>% na.omit
  
  # get gameweek scores for each entry
  scores <- get_entry_season(eids)
  
  # add AVERAGE score if present (i.e. odd number of teams in league)
  if(any(is.na(actual_table$entry))) {
    
    # get average score each gameweek
    ngws <- actual_table$matches_played[1]
    average_score <- fplscrapR::get_round_info(1:ngws) %>% 
      mutate(name = "AVERAGE") %>% 
      select(event = id, points = average_entry_score, name)
    
    # add average score to all scores
    scores <- rbind(average_score,
                    scores %>% 
                      select(event, points, name))
    
  }
  
  # expected points as average h2h points expected against all other players in each gameweek
  xpts_rounds <- lapply(unique(scores$event), function(i) {
    ss <- scores[scores$event == i,]
    lapply(unique(ss$name), function(j) {
      score <- ss[ss$name == j,]$points
      opp_scores <- ss[ss$name != j,]$points
      xpts <- (sum(as.numeric(score > opp_scores) * 3) + sum(as.numeric(score == opp_scores) * 1)) / length(eids)
      data.frame(player_name = j, gw = i, xpts)
    }) %>% 
      plyr::rbind.fill()
  }) %>% 
    plyr::rbind.fill()
  
  # table by expected points
  xpts_table <- xpts_rounds %>% 
    group_by(player_name) %>% 
    summarise(xPts = sum(xpts)) %>% 
    ungroup() %>% 
    arrange(-xPts) %>% 
    mutate(xRank = 1:n(),
           player_name = as.character(player_name))
  
  # actual + expected table order by expected points
  combined_table <- left_join(actual_table,
                              xpts_table,
                              by = "player_name") %>%
    select(player_name, rank, pts = total, xRank, xPts)
  
  return(combined_table)
  
}