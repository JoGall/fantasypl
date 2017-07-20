library(dplyr)
library(ggplot2)
library(plotly)

#get raw player data
dat <- read.csv('~/Desktop/Dropbox/github/FantasyPL/16-17_formatted.csv')

#get 10 players with most minutes played this season
# footy100_names <- dat %>% 
#   group_by(player_id) %>% 
#   summarise(name = name[1], team = team[1], total_mins = sum(minutes), total_points = sum(total_points)) %>% 
#   arrange(desc(total_mins)) %>%
#   select(player_id) %>%
#   head(10) %>%
#   .$player_id

#get 10 players with most points this season
footy100_names <- dat %>% 
  group_by(player_id) %>% 
  summarise(name = name[1], team = team[1], total_mins = sum(minutes), total_points = sum(total_points)) %>% 
  arrange(desc(total_points)) %>%
  select(player_id) %>%
  head(10) %>%
  .$player_id

footy100 <- dat %>%
  subset(player_id %in% footy100_names)

unique(footy100$name)

#basic plot
ggplot(footy100, aes(x = game_id, y = total_points, colour = name)) +
  geom_line()

#interactive plot
plot_ly(footy100, x = ~game_id, y = ~total_points,
        color = ~paste0(name, " (", team, ")"),
        text = ~name,
        hoverinfo="text+y",
        mode="lines",
        width=2
) %>%
  add_trace() %>%
  layout(title = "Fantasy PL points (2016-17)",
         legend = list(y = 1.5, orientation = 'h', xanchor = 3),
         margin = list(r = 100))


#get first derivative of points
footy100 <- footy100 %>% mutate(diff = total_points - lag(total_points))

#...and plot
plot_ly(footy100, x = ~round, y = ~diff,
        color = ~paste0(name, " (", team, ")"),
        text = ~name,
        hoverinfo="text+y",
        mode="lines",
        width=2
) %>%
  add_trace() %>%
  layout(title = "Fantasy PL points (2016-17)",
         legend = list(y = 1.5, orientation = 'h', xanchor = 3),
         margin = list(r = 80))

#distribution of points
d <- dat %>% 
  group_by(player_id) %>% 
    summarise(name = name[1], team = team[1], 
              total_mins = sum(minutes), 
              total_points = sum(total_points), value = mean(value))

#looking at player summaries over season
d %>% 
  dplyr::filter(total_points > 0) %>%
  ggplot(aes(x = total_points)) +
    geom_histogram(bins = 19)

#looking at scores each week
dat %>% 
  dplyr::filter(minutes > 0) %>%
  ggplot(aes(x = total_points)) +
  geom_histogram(bins = 20)

# build dataset for machine learning w/ all important variables we can think of
# performance scores: last weeks score, running average of last 5 weeks score
# predictors: next teams recent form, points conceded etc..
# also what market is telling is: current price, transfers in, ownership

#for now let's just do this for one player, e.g.
d2 <- dat %>%
  dplyr::filter(name == "Sánchez")
  
newStats <- lapply(1:max(d2$game_id), function(x) {
 
  #recent form index
  idx <- (x-5):(x-1)
  idx <- ifelse(idx < 1, NA, idx)
  n_idx <- length(na.omit(idx))
    
  #set variables
  player_pos <- unique(d2$pos)
  player_team <- unique(d2$team)
  opp_team <- d2$opponents[x] 
  
  #player recent points
  player_form <- d2 %>%
    dplyr::filter(game_id %in% idx) %>%
    summarise(tmp = sum(total_points)) %>%
    summarise(player_form = sum(tmp))
  #team recent points
  team_form <- dat %>%
    dplyr::filter(game_id %in% idx & team == player_team) %>%
    summarise(tmp = sum(total_points)) %>%
    summarise(team_form = sum(tmp))
  #team recent points in this position
  team_form_pos <- dat %>%
    dplyr::filter(game_id %in% idx & team == player_team & pos == player_pos & minutes > 0) %>%
    summarise(tmp = sum(total_points)) %>%
    summarise(team_form_pos = sum(tmp))
    
  #opponent recent form
  opp_pts <- dat %>%
    dplyr::filter(game_id %in% idx & team == opp_team) %>%
    summarise(tmp = sum(total_points), n()) %>%
    summarise(opp_pts = sum(tmp))
  
  #opponent recent points conceded (watch out for mid-season loanees changing teams)
  opp_pts_conc <- lapply(idx, function(x) {
    opp_opp <- Mode(subset(dat, game_id == x & team == opp_team)$opponents)
    dat %>%
      dplyr::filter(game_id == x & team == opp_opp) %>%
      summarise(sum(total_points))
  }) %>%
    plyr::rbind.fill() %>%
    sum()
  
  #TODO: 
  #opponent recent points conceded to player position
  
  data.frame(player_form, team_form, team_form_pos, opp_pts, opp_pts_conc) / n_idx

}) %>%
  plyr::rbind.fill()

d3 <- data.frame(d2, newStats)

#see what the new predictors look like
ggplot(d3, aes(x = game_id, y = total_points)) +
  geom_line() +
  geom_line(aes(game_id, y = player_form), col="red") +
  geom_line(aes(game_id, y = team_form), col="blue") +
  geom_line(aes(game_id, y = team_form_pos), col="lightblue")

ggplot(d3, aes(x = game_id, y = total_points)) +
  geom_line() +
  geom_line(aes(game_id, y = opp_pts), col="red") +
  geom_line(aes(game_id, y = opp_pts_conc), col="blue")


# build prediction models
# split the data into training and testing sets
smp_size <- floor(0.8 * nrow(d3))
set.seed(123)
train_ind <- sample(seq_len(nrow(d3)), size = smp_size)

train <- d3[train_ind, ]
test <- d3[-train_ind, ]

#1: baseline model
baseline_mod <- mean(train$total_points)
sqrt(mean((baseline_mod - test$total_points)^2)) #RMSE
mean(abs(baseline_mod - test$total_points)) #MAE

#2: multiple linear regression
lin_mod <- lm(total_points ~ player_form + team_form + team_form_pos + opp_pts + opp_pts_conc + value + transfers_balance, data = d3)
test.lin_mod <- predict(lin_mod)
sqrt(mean((test.lin_mod - test$total_points)^2))
mean(abs(test.lin_mod - test$total_points))

data.frame(d3, pred_y = c(NA,test.lin_mod)) %>%
  ggplot(aes(x = game_id, y = total_points)) +
    geom_line() +
    geom_line(aes(game_id, y = pred_y), col="red")
