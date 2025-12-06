source("ScheduleLoader.R")
source("WinProbabilityCalculator.R")
source("EloStart.R")

season_sim <- function() {
  current_week <- 14
  last_week <- 18
  
  sim_wins = data.frame(
    TCode = team_data$TCode,
    wins = rep(0, 32)
  )
  
  for (week in current_week:last_week) {
    schedule <- schedule_by_week(week) |>
      rename(TCode = team)
    schedule <- left_join(schedule, team_data, by = join_by(TCode))
    schedule$WP <- calculate_win_probabilities(schedule$TCode, schedule$home_team, schedule$Elo, schedule$HFA, 400)
    for (i in 1:length(unique(schedule$home_team))) { #Have to simulate for each game which is half the number of teams on the schedule
      u <- runif(1)
      if (u < schedule$WP[i]) {
        #TCode (away team) wins
        sim_wins[sim_wins$TCode == schedule$TCode[i], "wins"] <- sim_wins[sim_wins$TCode == schedule$TCode[i], "wins"] + 1
      }
      else {
        #home team wins
        sim_wins[sim_wins$TCode == schedule$home_team[i], "wins"] <- sim_wins[sim_wins$TCode == schedule$home_team[i], "wins"] + 1
      }
    }
  }
  
  return(sim_wins)
}
