library(jsonlite)

# Static API for all FPL player data
d <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")

#find total number of players
n_players <- max(d$elements$id)

#scrape data for each player
players_list <- list()
for(i in 1:n_players){
	url <- paste0("https://fantasy.premierleague.com/drf/element-summary/", i)
	l <- fromJSON(url)
	players_list[[i]] <- l
	Sys.sleep(sample(seq(1, 2, by=0.001), 1))
}

#TODO: flatten list
# players_df <- do.call("rbind", players_list)
