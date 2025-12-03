source("WinProbabilityCalculator.R")

week_simulator <- function(team_names, home_teams, win_probabilities) {
  game_results <- rep(-1,length(team_names))
  for(team in 1:length(team_names)) {
    if(team_names[team] == home_teams[team]) {
      u <- runif(1, min = 0, max = 1)
      away_team_index <- get_opposite_index_of_value(home_teams, team)
      if(u < win_probabilities[team]) {
        game_results[team] <- 1
        game_results[away_team_index] <- 0
      } else {
        game_results[team] <- 0
        game_results[away_team_index] <- 1
      }
    }
  }
  return(game_results)
}