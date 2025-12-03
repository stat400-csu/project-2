{r}
'''
ELODIF <- (home_elo - away_elo)/25 #25 is a general expecect elo to point differential
ExpectPointSpread <- (ELODIF + HFA_PointAdvantage) / Sigma_NFL #Sigma_NFL the sd is expected to be maybe 13.5 or 9 where 68%/1SD of games is has a margin between +- 13.5, 9 for this we could research this
'''
