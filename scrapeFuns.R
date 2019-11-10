require(dplyr)
require(jsonlite)

#--------------------------------------------------------------------------
# getFPLSummary()
#--------------------------------------------------------------------------
# Get summary of all FPL player statistics from official API
#--------------------------------------------------------------------------

getFPLSummary <- function() {
  
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


#--------------------------------------------------------------------------
# getFPLFull()
#--------------------------------------------------------------------------
# Get week-by-week breakdown of FPL player statistics from official API
#--------------------------------------------------------------------------

getFPLFull <- function() {
  
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


#--------------------------------------------------------------------------
# getManagerScores()
#--------------------------------------------------------------------------
# Get total points scored by manager IDs
#--------------------------------------------------------------------------

getManagerScores <- function(IDs) {
  
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


#--------------------------------------------------------------------------
# getManagerPicks()
#--------------------------------------------------------------------------
# Get players picked each gameweek by manager ID
# TODO: get score by merging with player breakdown per gameweek
#--------------------------------------------------------------------------

getManagerPicks <- function(IDs, gameweeks = NULL) {
  
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


#--------------------------------------------------------------------------
# getFixtures()
#--------------------------------------------------------------------------
# Get fixtures and their difficulty for remaining gameweeks
#--------------------------------------------------------------------------

getFixtures <- function() {
  
  fixtures <- fromJSON("https://fantasy.premierleague.com/api/fixtures/")
  fixtures$team_h <- team_key[match(fixtures$team_h, team_key$id), "name"]
  fixtures$team_a <- team_key[match(fixtures$team_a, team_key$id), "name"]
  
  fixture.list <- rbind(
    fixtures %>% 
      select(gameweek = event, team = team_h, opp = team_a, difficulty = team_h_difficulty) %>% 
      mutate(isHome = TRUE),
    fixtures %>% 
      select(gameweek = event, team = team_a, opp = team_h, difficulty = team_a_difficulty) %>% 
      mutate(isHome = FALSE)
  )
  
  return(fixture.list)

}


#--------------------------------------------------------------------------
# xPts_h2h()
#--------------------------------------------------------------------------
# Calculate expected points for each team in a head-to-head table as the
# average points (i.e. 0, 1, 3) expected against all teams in that league 
# (including AVERAGE, if present) each gameweek, not just the actual 
# opponent.
#--------------------------------------------------------------------------

xPts_h2h <- function(leagueid) {
  
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
