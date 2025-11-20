library(tidyverse)

file <- "2024_standings_2025_odds(in)"
standings_odds <- read_csv(file)

weights <- c(0.4, 0.6)
mean_elo <- 1000
scale <- 50

start_elo <- standings_odds |>
  mutate(Weighted_ProjW = weights[1] * W + weights[2] * ProjW) |>
  mutate(Start_Elo = (Weighted_ProjW - 8.5) * scale + mean_elo)
