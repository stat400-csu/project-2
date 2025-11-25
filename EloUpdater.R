# input:
# 1. win_probability = Expected probability of winning (P_home, or 1-P_home)
# 2. actual_result = What actually happened (1 for win, 0.5 for tie, 0 for loss)
# 3. elo = Current elo of the team
# and a K-value, which is the sensitivity of the elo system
elo_updater <- function(win_probabilities, actual_results, elos, sensitivity) {
  K <- sensitivity
  for (team in 1:length(elos)) {
    elos[team] <- elos[team] + 
      K * (actual_results[team] - win_probabilities[team])
  }
  return(elos)
}

test_results <- data.frame(
  team = c("A", "B"),
  elo = c(1100, 900),
  win_probability = c(0.8, 0.2),
  actual_result = c(0, 1)
)

test_results$elo <- elo_updater(test_results$win_probability, 
                                test_results$actual_result, 
                                test_results$elo, 
                                32)
test_results