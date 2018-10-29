require(jsonlite)
require(dplyr)
require(ggplot2)
require(ggrepel)
require(engsoccerdata)

#--------------------------------------------------------------------------
# getFPLSummary()
#--------------------------------------------------------------------------
# Get summary of all FPL player statistics from official API
#--------------------------------------------------------------------------

getFPLSummary <- function() {
  
  # get summarised player data
  player_details <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$elements
  
  # match player team and position to keys
  pos_key <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$element_types
  team_key <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$teams
  player_details$pos <- pos_key[match(player_details$element_type, pos_key$id), "singular_name"]
  player_details$team <- team_key[match(player_details$team, team_key$id), "name"]
  
  # correct teamnames and add team colours 
  # load("~/Dropbox/github/engsoccerdata/data/teamnames.rda")
  # player_details <- matchTeamnames(player_details)
  # player_details <- addTeamColours(player_details)
  
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
  n_elements <- max(fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$elements$id)
  
  player_bygw <- lapply(1:n_elements, function(x) {
    print(paste0(x, " out of ", n_elements))
    Sys.sleep(sample(seq(0.6, 1.5, by=0.005), 1)) #scrape responsibly!
    url <- paste0("https://fantasy.premierleague.com/drf/element-summary/", x)
    df <- fromJSON(url)$history
    if(length(df)) {
      data.frame(player_id = x, df)
    }
  } ) %>%
    plyr::rbind.fill() 	#flatten list to df
  
  
  # get all player names, teams, and positions
  player_details <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$elements %>%
    select(id, name = web_name, pos_code = element_type, team_code = team)
  
  pos_key <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$element_types
  team_key <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$teams
  
  player_details$pos <- pos_key[match(player_details$pos_code, pos_key$id), "singular_name"]
  player_details$team <- team_key[match(player_details$team_code, team_key$id), "name"]
  
  player_bygw <- data.frame(player_bygw,
                            player_details[match(player_bygw$player_id, player_details$id), c("name", "team", "pos")])
    
  # get fixture difficulty
  fixtures <- fromJSON("https://fantasy.premierleague.com/drf/fixtures/")

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
# Get week-by-week scores of managers for specified IDs and across specified gameweeks
#--------------------------------------------------------------------------

getManagerScores <- function(IDs, gameweeks = c(1:38)) {
  lapply(IDs, function(x) {
    print(paste0("ID: ", x))
    lapply(gameweeks, function(y) {
      print(paste0("Gameweek ", y, " out of ", length(gameweeks)))
      Sys.sleep(sample(seq(0.8, 1.5, by=0.001), 1)) #scrape responsibly!	
      url <- paste0("https://fantasy.premierleague.com/drf/entry/", x, "/event/", y, "/picks")
      df <- fromJSON(url)$picks
      data.frame(manager = x, gameweek = y, df)
    } ) %>%
      plyr::rbind.fill() #flatten list to df
  } ) %>%
    plyr::rbind.fill() #flatten list to df
}
