library(tidyverse)
library(nflreadr)
source("EloUpdater.R")
source("ScheduleLoader.R")
source("WinProbabilityCalculator.R")

standings_odds <- read_csv("2024_standings_2025_odds(in).csv")

hfa_data <- read_csv("NFL-HomeFieldAdvantage-Data.csv")

weights <- c(0.4, 0.6)
mean_elo <- 1000
scale <- 32

team_data <- standings_odds |>
  arrange(Tm) |>
  rename(Team_Name = Tm) |>
  mutate(Weighted_ProjW = weights[1] * W + weights[2] * ProjW) |>
  mutate(Elo = (Weighted_ProjW - 8.5) * scale + mean_elo) |>
  mutate(HFA = hfa_data$'Point Advantage') |>
  select(TCode, Team_Name, Elo, HFA)
  

last_week <- 13

for (week in 1:last_week) {
  current_week_schedule <- schedule_by_week(week)
  current_week_schedule <- rename(current_week_schedule, TCode = team)
  sched_w_elo <- left_join(team_data, current_week_schedule)
  sched_w_elo$WP <- calculate_win_probabilities(sched_w_elo$TCode, sched_w_elo$home_team, sched_w_elo$Elo, sched_w_elo$HFA, 400)
  sched_w_elo$Elo <- elo_updater(sched_w_elo$WP, sched_w_elo$win, sched_w_elo$Elo, scale)
  team_data$Elo <- sched_w_elo$Elo
}

