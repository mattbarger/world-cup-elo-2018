####  R Recipes by Matt

####  The Flying World Cup Elo Bisque v.0.3
####  Converting and optimizing my previous formula for European Soccer Parity to the upcoming World Cup 2018 from Russia
####  Chef's Notes: Cook time is about 10+ minutes, I feel like we can reduce that so much more. Help plz.

####  Doing a proper Elo formula to start my pretentious World Cup blog of data hot takes
####  Goals are to 
####    a) migrate this to Python as a learning exercise at some point
####    b) create data based predictions for World Cup 2018
####    c) compare this world cup's cast to previous world cups' teams, measure competitive parity

rm(list = ls())

options(digits = 4, stringsAsFactors = F)
library(dplyr)
library(tidyr)
library(ggplot2)

#call relevant databases
##lookup table for World Cup Teams
library(readr)
in_world_cup <- read_csv("C:/Users/mattb/Desktop/international-football-results-from-1872-to-2017/in_world_cup.csv")

##lookup table for K_factor (game weights: feel free to consult on methodolgy. Lots to be done here.)
library(readr)
k_factor <- read_csv("C:/Users/mattb/Desktop/international-football-results-from-1872-to-2017/k_factor.csv",
                     col_types = cols(X3 = col_skip(), X4 = col_skip()))
##The Data Itself
library(readr)
results <- read_csv('C:/Users/mattb/Desktop/international-football-results-from-1872-to-2017/results.csv')
matches <- results %>%
  filter(date >= "1946-01-01") %>%                       ###filter for all games in the Postwar era THEN
  mutate(home_adv = ifelse(neutral == F, 100, 0)) %>%    ###add on the home_adv, a constant of 100 elo points THEN
  left_join(k_factor) %>%                                ###add the k_factor, or the match weights THEN
  arrange(date, home_team)                               ###prep the data for the iteration

#FUNCTIONS DICTIONARY
##Finding the last rating or assigning 1300 if no last rating found
rating.initiate <- function(i, tm) {
  ###Extract the row of tm's last game, store as tmp
  tmp <- matches %>%
    slice(0:i) %>%                                  #lucky I remembered that "0" performs a 1-row lag here, but there has to be an easier way than this.
    filter(home_team == tm | away_team == tm) %>%
    filter(date == max(date))
  if(is.na(tmp$date[1]) == TRUE) {                  #because of lag-factor, only NAs show up if it's the team's first game in postwar record
    return(1300)
  } else if(tm == tmp$home_team[1]) {  #if tm was the home team
    return(tmp$rating_home_post[1])    #return the home rating of tmp
    rm(tmp)
  } else {
    return(tmp$rating_away_post[1])    #if not, return the away rating of tmp
    rm(tmp)
  }
}

#function to find the probability of the home team to win the game
WinProb <- function(H, A, HFA) {
  return(1/(10^(-(H + HFA - A)/400)+1)) #per wikipedia
}

#function to calculate the weight of the margin of victory for the PtsExch function
MVMult <- function(home.goals, away.goals) { #per wikipedia
  if(home.goals - away.goals == 0) {
    return(1)
  } else {
    return(sqrt(abs(home.goals - away.goals)))
  }
}

PtsExch <- function(actual, expected, k, home.goals, away.goals) {             ### Each team exchanges a number of points as decided by the formula itself.
  round((actual - expected) * k * MVMult(home.goals, away.goals), digits = 0)  ### rounded to an integer, because dgaf about decimals.
}

#Now, run the algorithm itself.
for(g in 1:nrow(matches)) {       #add the following variables to the dataframe in this order, calculating cell by cell across each row then moving on to the next row
  matches$rating_home_pre[g]      <- rating.initiate(g, matches$home_team[g])  #get the final rating from the last match or assign 1300 as the starting rating
  matches$rating_away_pre[g]      <- rating.initiate(g, matches$away_team[g])  
  matches$expected_result_home[g] <- WinProb(matches$rating_home_pre[g],       #get the expected result as expected from the home team
                                             matches$rating_away_pre[g],
                                             matches$home_adv[g])              #home team gets an extra 100 pts, to be factored in expected probability
  matches$actual_result_home[g]   <- ifelse(matches$home_score[g] > matches$away_score[g], 1,  #1 for a win
                                            ifelse(matches$home_score[g] == matches$away_score[g], 0.5, 0)) #0.5 for a draw, 0 for a loss
  matches$pts_exchanged_home[g]   <- PtsExch(matches$actual_result_home[g],    #calculate the number of points to be exchanged between teams
                                             matches$expected_result_home[g],  #given by the result and its improbability (deviation from expectation)
                                             matches$k_factor[g],
                                             matches$home_score[g],            #done from the perspective of the home team in this case
                                             matches$away_score[g])
  matches$rating_home_post[g]     <- matches$rating_home_pre[g] + matches$pts_exchanged_home[g]   #add the positive or negative sum of points to the home team's prematch rating
  matches$rating_away_post[g]     <- matches$rating_away_pre[g] - matches$pts_exchanged_home[g]   #and subtract it from the away team's prematch rating
}
#### Feels elegant, but could be better tbh. Runs a sample in around 15 minutes or so. Is there a command I'm missing to make this faster? Because Excel still wins here in the calculation race.
