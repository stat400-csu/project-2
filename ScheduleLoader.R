library(tidyverse)
library(nflreadr)

schedule_by_week <- function(week_num) {
  schedule <- load_schedules(2025)
  
  away_teams <- schedule |>
    select(week, away_team, home_team, result) |>
    filter(week == week_num) |>
    rename(team = away_team) |>
    mutate(win = result < 0) |>
    select(team, home_team, win)
    
  home_teams <- schedule |>
    select(week, away_team, home_team, result) |>
    filter(week == week_num) |>
    mutate(team = home_team) |>
    mutate(win = result > 0) |>
    select(team, home_team, win)
  
  week_schedule <- bind_rows(away_teams, home_teams)
  return(week_schedule)
}

#Maybe possibly dont need anymore
get_home_team_data_from_schedule <- function(home_teams, away_teams) {
  team_count <- length(home_teams) * 2
  home_team_data <- data.frame(team_name = rep(NA, team_count), 
                               home_team = rep(NA, team_count))
  for(team in 1:length(home_teams)) {
    odd_index <- 2*team - 1
    home_team_data[odd_index, "team_name"] <- home_teams[team]
    home_team_data[odd_index, "home_team"] <- home_teams[team]
    
    even_index <- 2*team
    home_team_data[even_index, "team_name"] <- away_teams[team]
    home_team_data[even_index, "home_team"] <- home_teams[team]
  }
  home_team_data <- home_team_data |> arrange(team_name)
  return(home_team_data$home_team)
}

#Test code
week_1_schedule <- schedule_by_week(4)
View(week_1_schedule)
get_home_team_data_from_schedule(week_1_schedule$home_team, week_1_schedule$away_team)