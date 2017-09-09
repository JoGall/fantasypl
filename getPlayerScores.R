library(jsonlite)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(engsoccerdata)
source("~/Dropbox/github/FantasyPL/footballFuns.R")

d <- getFPLFull()
# write.csv(d, "~/Dropbox/github/FantasyPL/17-18.csv", row.names=FALSE)

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
	data.frame(player_id = x, df[,c(7, 2, 54, 6, 4, 5, 9:13, 8, 16:51)])
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

# plot
player_bygw <- addTeamColours(player_bygw)
player_bygw$team <- as.factor(player_bygw$team)
team_colours <- player_bygw %>% group_by(team) %>% filter(row_number()==1) %>% select(team, col1, col2)
player_bygw <- transform(player_bygw, pos = factor(pos, levels=c("Goalkeeper", "Defender", "Midfielder", "Forward")))
                         
ggplot(subset(player_bygw, minutes > 0), aes(x = bps, y = round(total_points / value / 10, 2))) +
  geom_point(alpha = 0.4) +
  ylab("Points per £m") +
  xlab("Bonus points system") +
  geom_label_repel(
    aes(label = name, fill = team, colour = team),
    segment.colour = "black",
    fontface = 'bold',
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines")
  ) +
  scale_fill_manual(values = team_colours$col1, breaks = unique(player_bygw$team)) +
  scale_colour_manual(values = team_colours$col2, breaks = unique(player_bygw$team)) +
  my_theme +
  facet_wrap(~pos)

#--------------------------------------------------------------------------

# get data
d <- getFPLSummary()

# top 10 ppm vs. bps
top10 <- d %>% group_by(pos) %>% arrange(desc(total_points / (now_cost / 10))) %>% filter(row_number() %in% 1:10) %>% arrange(team)
team_colours <- top10 %>% group_by(team) %>% filter(row_number()==1) %>% select(team, col1, col2)

ggplot(top10, aes(x = bps, y = total_points / (now_cost / 10))) +
  geom_point(data = subset(d, minutes > 0), alpha = 0.7) +
  ylab("Points per £m") +
  xlab("Bonus points system") +
  ggtitle("Fantasy Premier League: after GW 2") +
  geom_label_repel(
    data = top10,
    aes(label = web_name, fill = team, colour = team),
    segment.colour = "black",
    fontface = 'bold',
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines"),
    max.iter = 4000
  ) +
  scale_fill_manual(values = team_colours$col1, breaks = team_colours$team) +
  scale_colour_manual(values = team_colours$col2, breaks = team_colours$team) +
  facet_wrap(~pos) +
  my_theme +
  theme(plot.title = element_text(face="bold", hjust = 0.5))

# top 10 ict vs. ppm
top10 <- d %>% group_by(pos) %>% arrange(desc(ict_index)) %>% filter(row_number() %in% 1:10) %>% arrange(team)
team_colours <- top10 %>% group_by(team) %>% filter(row_number()==1) %>% select(team, col1, col2)

ggplot(top10, aes(x = as.numeric(ict_index), y = total_points / (now_cost / 10))) +
  geom_point(data = subset(d, minutes > 0), alpha = 0.7) +
  ylab("Points per £m") +
  xlab("Influence|Creativity|Threat index") +
  ggtitle("Fantasy Premier League: after GW 2") +
  geom_label_repel(
    data = top10,
    aes(label = web_name, fill = team, colour = team),
    segment.colour = "black",
    fontface = 'bold',
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines"),
    max.iter = 4000
  ) +
  scale_fill_manual(values = team_colours$col1, breaks = team_colours$team) +
  scale_colour_manual(values = team_colours$col2, breaks = team_colours$team) +
  facet_wrap(~pos) +
  my_theme +
  theme(plot.title = element_text(face="bold", hjust = 0.5))

# ict_index as a predictor of points
ggplot(d, aes(x = total_points, y = as.numeric(ict_index))) +
  geom_point(data = subset(d, minutes > 0), alpha = 0.7) +
  geom_smooth(method = 'lm', col="black") +
  xlab("Influence|Creativity|Threat index") +
  ylab("Total points") +
  my_theme

# bps as a predictor of points
ggplot(d, aes(x = total_points, y = bps)) +
  geom_point(data = subset(d, minutes > 0), alpha = 0.7) +
  geom_smooth(method = 'lm', col="black") +
  xlab("Bonus points system") +
  ylab("Total points") +
  my_theme



# TEAMS STRENGTH
#--------------------------------------------------------------------------

# get FPL strength estimates
team_strength <- fromJSON("https://fantasy.premierleague.com/drf/bootstrap-static")$teams %>%  
  select(team = name, strength_attack_home, strength_attack_away, strength_defence_home, strength_defence_away) %>%
  matchTeamnames()

# get 2016-17 results 
d <- subset(england_current(), tier == 1)

# e.g. test for home teams in this season and last
d <- subset(d, home %in% team_strength$team)

# joins
dd <- d %>%
  select(home, away = visitor, result, hgoal, vgoal) %>%
  filter(home %in% team_strength$team & away %in% team_strength$team) %>%
  left_join(., select(team_strength, team, strength_attack_home), by = c("home" = "team")) %>%
  left_join(., select(team_strength, team, strength_defence_home), by = c("home" = "team")) %>%
  left_join(., select(team_strength, team, strength_attack_away), by = c("away" = "team")) %>%
  left_join(., select(team_strength, team, strength_defence_away), by = c("away" = "team"))

ggplot(dd, aes(x = result, y = strength_attack_away - strength_defence_home)) +
  geom_boxplot()

# machine learning
library(caret)

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dd$home, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dd[-validation_index,]
# use the remaining 80% of data to training and testing the models
training <- dd[validation_index,]

# summarize the class distribution
percentage <- prop.table(table(training$home)) * 100
cbind(freq=table(training$home), percentage=percentage)

# multivariate plots
dataset <- training[,c(4, 6:9)]
dataset[,1] <- as.factor(dataset[,1])

x <- dataset[,2:5]
y <- dataset[,1]
# for(i in 2:5) {
#   dataset[,i] <- as.numeric(dataset[,i])
# }
featurePlot(x=x, y=y, plot="ellipse")
featurePlot(x=x, y=y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)


# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# a) linear algorithms
set.seed(7)
fit.lda <- train(result ~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(result ~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(result ~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(result ~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(result ~., data=dataset, method="rf", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
dotplot(results)

# make predictions
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$result)
