# input: dataframe with:
# 1. teams = the names of the teams
# 2. win_probability = Expected probability of winning (P_home, or 1-P_home)
# 3. actual_result = What actually happened (1 for win, 0.5 for tie, 0 for loss)
# 4. elo = Current elo of the team
# and a K-value, which is the sensitivity of the elo system
elo_updater <- function(game_results, sensitivity) {
  K <- sensitivity
  for (team in 1:length(game_results$elo)) {
    game_results[team,'elo'] <- game_results[team,'elo'] + 
      K * (game_results[team,'actual_result'] - game_results[team,'win_probability'])
  }
  return(game_results)
}

test_results <- data.frame(
  team = c("A", "B"),
  elo = c(1100, 900),
  win_probability = c(0.8, 0.2),
  actual_result = c(0, 1)
)

test_results <- elo_updater(test_results, 32)
test_results