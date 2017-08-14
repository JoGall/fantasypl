#---------------------------------------------------------------------------
# getFPLSummary()
#---------------------------------------------------------------------------
# Get summary of all FPL player statistics from official API
#---------------------------------------------------------------------------
getFPLSummary <- function() {
  require(jsonlite)
  
  player_details <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$elements
  
  pos_key <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$element_types
  team_key <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$teams
  
  player_details$pos <- pos_key[match(player_details$element_type, pos_key$id), "singular_name"]
  player_details$team <- team_key[match(player_details$team, team_key$id), "name"]
  
  player_details <- matchTeamnames(player_details)
  player_details <- addTeamColours(player_details)
  player_details$team <- as.factor(player_details$team)
  player_details <- transform(player_details, pos = factor(pos, levels=c("Goalkeeper", "Defender", "Midfielder", "Forward")))
  
  return(player_details)
}

#---------------------------------------------------------------------------
# matchTeamnames()
#---------------------------------------------------------------------------
# Conforms team names to those used in engsoccerdata package by usin 
# highest similarity match from 'teamnames' dataframe
#---------------------------------------------------------------------------
# * Inputs a dataframe with a column 'team' column; outputs the original 
#   dataframe with new teamname in column 'team' and old teamname in column 
#   'team_old'
# * If TRUE, 'checkResults' returns dataframe of best matches for validation
#---------------------------------------------------------------------------
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
