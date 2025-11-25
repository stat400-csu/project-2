library(tidyverse)

standings_odds <- read_csv("2024_standings_2025_odds(in).csv")

hfa_data <- read_csv("NFL-HomeFieldAdvantage-Data.csv")

weights <- c(0.4, 0.6)
mean_elo <- 1000
scale <- 50

team_data <- standings_odds |>
  arrange(Tm) |>
  rename(Team_Name = Tm) |>
  mutate(Weighted_ProjW = weights[1] * W + weights[2] * ProjW) |>
  mutate(Elo = (Weighted_ProjW - 8.5) * scale + mean_elo) |>
  mutate(HFA = hfa_data$Total)