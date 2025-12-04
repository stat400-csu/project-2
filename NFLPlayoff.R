#Season Simulator 
# We need to sort by division

#AFC Divisions
AFC_North <- c("BAL","PIT", "CIN", "CLE")
AFC_East <- c("NE", "BUF", "MIA", "NYJ")
AFC_South <- c("JAX", "IND", "HOU", "TEN")
AFC_West <- c("KC", "LAC", "DEN", "LV")
#NFC Divisions
NFC_North <- c("CHI", "GB", "DET", "MIN")
NFC_East <- c("PHI", "DAL", "WAS", "NYG")
NFC_South <- c("TB", "CAR", "ATL", "NO")
NFC_West <- c("LAR", "SEA", "SF", "ARI")

head(divisions)

division_standings <- function(division, team_names, wins){
  standings <- data.frame(
    division = division,
    team = team_names,
    wins = wins
  )
  standings <- standings[order(-standings$wins),]
  rownames(standings) <- NULL
  return(standings)
}



