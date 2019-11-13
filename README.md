# FantasyPL

Functions for retrieving player and manager scores from the Fantasy Premier League API for the current season.

Note: there are many more functions available in the better-maintained [fplscrapR](https://github.com/wiscostret/fplscrapR) package.


## Installation

You can install soccermatics from GitHub in R using devtools:

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("wiscostret/fplscrapR") #dependency
devtools::install_github("jogall/fantasypl")
```


## Example usage

To avoid confusion, 'player' here refers to football players (e.g. Salah) and 'manager' refers to FantasyPL teams (e.g. I'm manager of the glorious Birkenhead Albion).

```r
# load package and 
library(fantasypl)
library(dplyr)
```

### Get total player points

Get total points and other statistics for all players:

```r
player_points()
```

### Get player points by gameweek

Get points and other statistics for all players in each gameweek (note this function can take a while as the API must be called 559 times):

```r
player_points_by_gw()
```

### Get total manager scores

Get total points for specified managers.

e.g. Total points for team IDs `1`, `2`:

```r
manager_points(c(1, 2))
```

### Get manager picks

Get players picked by managers of specified teams and in specified gameweeks.

e.g. Picks for team IDs `1`, `2` in gameweeks 11, 12:

```r
manager_picks(c(1, 2), 11:12)
```

TODO: add player name to returned information.

### Expected manager points tables for head-to-head leagues

Calculate expected points (xPts) and expected rank (xRank) for each team in a head-to-head league as the average number of points (i.e. 0, 1, 3) expected against all teams in that league each gameweek (including AVERAGE team, present if odd number of teams in the league), not just the actual opponent.

Requires entering username and password in interactive dialogue in order to access private leagues, using the [`fplscrapR`](https://github.com/wiscostret/fplscrapR) package.

e.g. xPts for league ID `1`.

```r
xpts_h2h(1)
```

### Get results and fixture difficulty

```r
get_fixtures() %>% 
  filter(finished = T)
```

### See future fixtures and fixture difficulty

```r
get_fixtures() %>% 
  filter(finished = T)
```
