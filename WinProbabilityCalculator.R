set.seed(400)
# input: dataframe with:
# 1. team names
# 2. name of the home team 
# (this is the same as the previous for the home team, but not the away team)
# 3. elo
# 4. home field advantage
# 5. elo scaling parameter S
calculate_win_probabilities <- function(team_names, home_teams, elo, hfa, S) {
  win_probabilities <- rep(0, length(team_names))
  for(team in 1:length(team_names)) {
    if(team_names[team] == home_teams[team]) {
      away_team_index <- get_opposite_index_of_value(home_teams, team)
      exponent <- -1*(elo[team] + hfa[team] + elo[away_team_index])/S
      win_probabilities[team] <- 1/(1+10^exponent)
      win_probabilities[away_team_index] <- 1-win_probabilities[team]
    }
  }
  return(win_probabilities)
}

get_opposite_index_of_value <- function(input_vector, target_index) {
  result <- which(input_vector == input_vector[target_index])
  return(result[result != target_index])
}

test_vector <- c(1,2,3,4,5,3)
get_opposite_index_of_value(test_vector, 3)