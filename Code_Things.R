{r}
'''
ELODIF <- (home_elo - away_elo)/25 #25 is a general expecect elo to point differential
ExpectPointSpread <- (ELODIF + HFA_PointAdvantage) / Sigma_NFL #Sigma_NFL the sd is expected to be maybe 13.5 or 9 where 68%/1SD of games is has a margin between +- 13.5, 9 for this we could research this
WinProb <- pnorm(ExpectPointSpread)

#Nope
'''


#Playoff seed code Record compare to Division, take division leaders and compare records, Overall record <- Head to Head <- Division <- Conference for 1,2,3,4 
# For Wildcard <- take next 3 teams in conference <- Record <- hth <- division <- conference <- Strength of schedle


{R} #Outline for Playoffseed
'''
NFL[team] <- NFLstandings[team]
AFCseed <- function(){
  #Take top team from each division
  if(){

  }
}
NFCseed <- function(){
  #Take top team from each division 
  if(){
  
  }
}


'''
