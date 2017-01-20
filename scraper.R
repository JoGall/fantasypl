require(jsonlite)
require(magrittr)

players_list <-

	# get total number of players from static FPL API
	1:max(fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$elements$id) %>%
	1:max(.) %>%
	
	#scrape data for each player for each gameweek
	lapply(function(x) {
		Sys.sleep(sample(seq(1, 2, by=0.001), 1))
		url <- paste0("https://fantasy.premierleague.com/drf/element-summary/", x)
		df <- fromJSON(url)$history
		data.frame(player_id = x, df[,c(7, 54, 4:6, 8, 16:51)])
	} ) %>%
	
	#flatten to df
	rbind.fill()
	