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


divisions <- list(
  AFC_North = AFC_North,
  AFC_East = AFC_East, 
  AFC_South = AFC_South,
  AFC_West = AFC_West,
  
  NFC_North = NFC_North,
  NFC_East = NFC_East,
  NFC_South = NFC_South,
  NFC_West = NFC_West
)
head(divisions)

division_standings <- function(divisions, team_names, wins){
  standings <- data.frame(
    division = divisions,
    team = team_names,
    wins = wins
  )
  standings <- standings[order(-standings$wins),]
  rownames(standings) <- NULL
  return(standings)
}





