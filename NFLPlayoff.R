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

conference <- list(
  AFC = c(AFC_North, AFC_East, AFC_South, AFC_West),
  NFC = c(NFC_North, NFC_East, NFC_South, NFC_West)
)
head(divisions)
head(conference)

#wins for testing
wins <- c(
  # AFC
  BAL = 13, PIT = 11, CIN = 7, CLE = 6,
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

AFC_divwinners <- division_winners[startsWith(names(division_winners), "AFC")]
NFC_divwinners <- division_winners[startsWith(names(division_winners), "NFC")]

AFC_nondivwin <- setdiff(conference$AFC, AFC_divwinners)
NFC_nondivwin <- setdiff(conference$NFC, NFC_divwinners)

AFC_wildcard <- sort(wins[AFC_nondivwin], decreasing = T)[1:3]
NFC_wildcard <- sort(wins[NFC_nondivwin], decreasing = T) [1:3]
#Then we get next 3 
AFC_wildcard
NFC_wildcard

#wins/records depending on how we are doing this
seed_placement <- function(winners, wildcards, wins){ 
  teams <- c(winners, names(wildcards))
  sorted <- sort(wins[teams], decreasing = T)
  return(sorted)
}

AFC_seedplacement <- seed_placement(AFC_divwinners, AFC_wildcard, wins)
NFC_seedplacement <- seed_placement(NFC_divwinners, NFC_wildcard, wins)
AFC_seedplacement
NFC_seedplacement
#Tie breaker clauses
#Record <- Head to Head <- Division <- Conference <- Strength of Victory <- Strength of Schedule

# Playoff run
#AFC & NFC 1 seed First round bye
## 2 plays 7, 3 plays 6, 4 plays 5 
### HFA applies to higher seed (lower number)
#### Super Bowl ELO DIF



