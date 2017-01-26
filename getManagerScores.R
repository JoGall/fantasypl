require(jsonlite)
require(ddply)
require(plyr)
require(magrittr)

#-----------------------------------------

#set manager IDs and gameweek range, for example:
manager_IDs <- c(2022839, 1)
GWs <- 1:2

#-----------------------------------------

#get all manager scores for desired gameweeks
manager_scores <- 
	lapply(manager_IDs, function(x) {
		lapply(GWs, function(y) {
			Sys.sleep(sample(seq(1, 2, by=0.001), 1)) #scrape responsibly!	
			url <- paste0("https://fantasy.premierleague.com/drf/entry/", x, "/event/", y, "/picks")
			df <- fromJSON(url)$picks
			data.frame(manager = x, gameweek = y, df)
		} ) %>%
		plyr::rbind.fill()
	} ) %>%
	plyr::rbind.fill()

#create a dataframe of player names and IDs
player_idx <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$elements[c("id", "web_name")]

#get FPL scores for all players in all gameweeks for (players featured by managers only to save scraping >600 player APIs)
players <- unique(manager_scores$element)
player_scores <- lapply(players, function(x) {
	Sys.sleep(sample(seq(1, 2, by=0.001), 1))
	url <- paste0("https://fantasy.premierleague.com/drf/element-summary/", x)
	df <- fromJSON(url)$history
	data.frame(player_id = x, df[,c(gameweek = "round", pts = "total_points")])
} ) %>%
plyr::rbind.fill()


#get points for all managers
manager_scores %>%
merge(., player_idx, by.x = c("element"), by.y = c("id")) %>%
#add player scores
merge(player_scores, by.x = c("element", "gameweek"), by.y = c("player_id", "gameweek")) %>%
mutate(player = web_name) %>%
dplyr::select(manager, gameweek, position, player, pts) %>%
arrange(manager, gameweek, position)
