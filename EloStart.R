library(tidyverse)

file <- "2024_standings_2025_odds(in).csv"
standings_odds <- read_csv(file)

weights <- c(0.4, 0.6)
mean_elo <- 1000
scale <- 32

start_elo <- standings_odds |>
  mutate(Weighted_ProjW = round(weights[1] * W + weights[2] * ProjW, digits = 1)) |>
  mutate(Start_Elo = round(Weighted_ProjW - 8.5) * scale + mean_elo)

write_csv(start_elo, "starting_elo.csv")
