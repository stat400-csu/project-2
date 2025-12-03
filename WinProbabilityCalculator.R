get_opposite_index_of_value <- function(input_vector, target_index) {
  result <- which(input_vector == input_vector[target_index])
  return(result[result != target_index])
}

calculate_win_probabilities <- function(team_names, home_teams, elo, hfa, S) {
  win_probabilities <- rep(0, length(team_names))
  for(team in 1:length(team_names)) {
    if(team_names[team] == home_teams[team]) {
      away_team_index <- get_opposite_index_of_value(home_teams, team)
      exponent <- (-1*(elo[team] + hfa[team] - elo[away_team_index]))/S
      win_probabilities[team] <- 1/(1+10^exponent)
      win_probabilities[away_team_index] <- 1-win_probabilities[team]
    }
  }
  return(win_probabilities)
}