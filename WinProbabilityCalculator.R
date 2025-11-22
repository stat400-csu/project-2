set.seed(400)
# input: dataframe with:
# 1. team names
# 2. name of the home team 
# (this is the same as the previous for the home team, but not the away team)
# 3. elo
# 4. home field advantage
calculate_win_probabilities <- function(team_names, home_teams, elo, hfa) {
  win_probabilities <- rep(0, length(team_names))
  
}