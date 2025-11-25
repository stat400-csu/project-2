library(tidyverse)
library(nflreadr)

schedule <- load_schedules(2025)

schedule_by_week <- function(week_num) {
  week_schedule <- schedule |>
    select(week, away_team, home_team, result) |>
    filter(week == week_num) |>
    mutate(away_win = result < 0) |>
    mutate(home_win = result > 0)
  return(week_schedule)
}

