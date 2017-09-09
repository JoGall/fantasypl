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
  require(jsonlite)
  
  # get summarised player data
  player_details <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$elements
  
  # match player team and position to keys
  pos_key <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$element_types
  team_key <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$teams
  player_details$pos <- pos_key[match(player_details$element_type, pos_key$id), "singular_name"]
  player_details$team <- team_key[match(player_details$team, team_key$id), "name"]
  
  # correct teamnames and add team colours 
  load("~/Dropbox/github/engsoccerdata/data/teamnames.rda")
  player_details <- matchTeamnames(player_details)
  player_details <- addTeamColours(player_details)
  
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
    Sys.sleep(sample(seq(0.4, 1, by=0.001), 1)) #scrape responsibly!
    url <- paste0("https://fantasy.premierleague.com/drf/element-summary/", x)
    df <- fromJSON(url)$history
    if(length(df)) {
      data.frame(player_id = x, df[,c(7, 2, 54, 6, 4, 5, 9:13, 8, 16:51)])
    }
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
  
  #add variable for fixture number
  player_bygw <- player_bygw %>%
    arrange(player_id, kickoff_time) %>%
    group_by(player_id) %>%
    mutate(game_id = seq_len(n()))
  
  return(player_bygw)
}


#--------------------------------------------------------------------------
# matchTeamnames()
#--------------------------------------------------------------------------
# Conforms team names to those used in engsoccerdata package by usin 
# highest similarity match from 'teamnames' dataframe
#--------------------------------------------------------------------------
# * Inputs a dataframe with a column 'team' column; outputs the original 
#   dataframe with new teamname in column 'team' and old teamname in column
#   'team_old'
# * If TRUE, 'checkResults' returns dataframe of best matches for validation
#--------------------------------------------------------------------------

matchTeamnames <- function(df, min_dist = 0.1, checkResults = FALSE) {
  require(engsoccerdata)
  require(RecordLinkage)
  
  # set teamnames as characters
  df$team <- as.character(df$team)
  df$team_old <- df$team
  
  old_new <- lapply(unique(df$team), function(x) {
    distance <- levenshteinSim(as.character(x), as.character(teamnames$name_other))
    # threshold on distance
    new_name <- ifelse(max(distance, na.rm=T) >= min_dist, as.character(teamnames[which.max(distance),]$name), "NA")
    
    old_new <- data.frame(old_name = x, new_name, distance = max(distance, na.rm=T), stringsAsFactors = FALSE)
  }) %>%
    plyr::rbind.fill()
  
  if(checkResults) {
    return(old_new)
  } else {
    df$team <- old_new$new_name[match(df$team, old_new$old_name)]
    return(df)
  }
}

#---------------------------------------------------------------------------
# addTeamColours()
#---------------------------------------------------------------------------
# Add hexcodes for EPL team colours to dataframe containing a column 'team'
#---------------------------------------------------------------------------
addTeamColours <- function(df) {
  team_col <- read.csv("~/Dropbox/blog_prep/team_cols.csv", stringsAsFactors = F)
  team_col$team <- as.factor(team_col$team)
  merge(df, team_col, on.x = "team", on.y = "team")
}

#---------------------------------------------------------------------------
# addTeamnames()
#---------------------------------------------------------------------------
# Add new team names to engsoccerdata 'teamnames' dataframe and overwrite
# local copy
#---------------------------------------------------------------------------
addTeamnames <- function(name, name_other, country = "England", most_recent = NA) {
  require(engsoccerdata)
  load("~/Dropbox/github/engsoccerdata/data/teamnames.rda")
  teamnames <- rbind(teamnames, data.frame(country = country, name = name, name_other = name_other, most_recent = most_recent))
  team_col$team <- as.factor(team_col$team)
  save(teamnames, file = "~/Dropbox/github/engsoccerdata/data/teamnames.rda")
  load("~/Dropbox/github/engsoccerdata/data/teamnames.rda")
}
