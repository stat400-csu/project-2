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

#wins for testing
wins <- c(
  # AFC
  BAL = 11, PIT = 9, CIN = 7, CLE = 6,
  NE  = 8,  BUF = 10, MIA = 9, NYJ = 6,
  JAX = 9,  IND = 7,  HOU = 12, TEN = 5,
  KC  = 13, LAC = 8,  DEN = 9, LV  = 7,
  
  # NFC
  CHI = 4,  GB  = 7,  DET = 12, MIN = 6,
  PHI = 11, DAL = 10, WAS = 5,  NYG = 4,
  TB  = 9,  CAR = 4,  ATL = 10, NO  = 8,
  LAR = 9,  SEA = 8,  SF  = 13, ARI = 3
)

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
#division_standings sorts position in the division 


#Next we pull leader from each division
all_standings <- lapply(names(divisions), function(div){
  division_standings(div,divisions[[div]],wins[divisions[[div]]])
})

names(all_standings) <- names(divisions)

division_winners <- sapply(all_standings,function(df) df$team[1])
division_winners

AFC_winners <- division_winners[startsWith(names(division_winners), "AFC")]
NFC_winners <- division_winners[startsWith(names(division_winners), "NFC")]
AFC_winners
NFC_winners
#Then we get next 3 

#Tie breaker clauses
#Record <- Head to Head <- Division <- Conference <- Strength of Victory <- Strength of Schedule


