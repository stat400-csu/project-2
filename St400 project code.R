

###########The first population births

# fertility rates for 1982 in thousands
eighty2 = c(1.3,53.7, 115.5,111.8,60.2,19.6,4,0.2)/1000

x0 = 1.8


# population age bins  for 10 -60  10-14,15-19......60


population_age_bins_10260 = c(16239,18419,20600,22166,20349,18864,14427,11953)

# Women population age bins

# total females 

# 10- 14
#ten_14 = 8811 

# 15-19

#fif_19 = 9713


#20-24

# 10836
#25-29

#5_24= 10372


#30-34
#thrty_34 = 9414

#35-40
# 7938


#41-45
#6347

#45-49

#5677


# women Population vector 10 to 60

women_pop = c( 8811 ,9713,10836,10372,9414,7938,6347,5677)*1000



#### loop to calculaste first year fertility ratios #######

# fertility rates distribution
b = 2.3

m = 1.9

a = 1.6

# quinial change distribution


a_z = 0

m_z = 0.3

b_z = 0.5



fertility_ratio = eighty2/x0

fertility_age_bins = rep(NA,length(fertility_ratio))


for (i in 1:length(fertility_ratio)) {
  
  fertility_i = draw(a,b,m)
  
  zt = draw(a_z,b_z,m_z)
  
  #Makes sure that the fertility rates are not 
  #too much more than the quinial chamge
  while(abs(fertility_i-x0)> zt){
    # keeps redrawing until the above condition is met
    fertility_i = draw(a,b,m)
  }
  
  
  fertility_age_bins[i] =  fertility_i*fertility_ratio[i]
  
}








# birth rates Ft in the matrix for 10 to 60
births_age_bins = women_pop*fertility_age_bins


#Ft in the matrix
Birth_rate =births_age_bins/population_age_bins_10260












######## Birth rates function in thousands######

birth_fxn = function(year) {
  eighty2 = c(1.3,53.7, 115.5,111.8,60.2,19.6,4,0.2)/1000
  
  x0 = 1.8
  
  women_pop = c( 8811 ,9713,10836,10372,9414,7938,6347,5677)*1000
  
  
  
  # fertility rates distribution
  b = 2.3
  
  m = 1.9
  
  a = 1.6
  
  # quinial change distribution
  
  
  a_z = 0
  
  m_z = 0.3
  
  b_z = 0.5
  
  
  
  fertility_ratio = eighty2/x0
  
  fertility_age_bins = rep(NA,length(fertility_ratio))
  
  
  for (i in 1:length(fertility_ratio)) {
    
    fertility_i = draw(a,b,m)
    
    zt = draw(a_z,b_z,m_z)
    
    #Makes sure that the fertility rates are not 
    #too much more than the quinial chamge
    while(abs(fertility_i-x0)> zt){
      # keeps redrawing until the above condition is met
      fertility_i = draw(a,b,m)
    }
    
    
    fertility_age_bins[i] =  fertility_i*fertility_ratio[i]
    
  }
  # birth rates Ft in the matrix
  births_age_bins = women_pop*fertility_age_bins
  
  
  # population age bins 10-14,15-19......
  
  
  population_age_bins = c(16239,18419,20600,22166,20349,18864,14427,11953)
  
  #Ft in the matrix
  Birth_rate =births_age_bins/population_age_bins
  
  return(Birth_rate)
}

birth_fxn(1987)


# inverse transform probability density function sampling: draw function

draw = function(a,b,m){
  
  
  
  # if invt > m , then use the m-t transform
  
  #a-t transform
  u = runif(1)
  
  inv_t = 2*u*(-a+m)+a
  
  x1 = inv_t
  
  # if invt > m , then use the m-t transform
  
  if(inv_t>m){
    
    u = runif(1)
    
    inv_t = u*(2*(b-m))+m
    
    x1 = inv_t
  }
  
  
  return (x1)
}




#################### Immigration


#initial net migration

NI0 = 450000/1000000

#Immigration age bins in 1/1million
Immigration_age_bins = c(32600,37300,42600,46300,57500,74800,49000,
                         31100,21100,16100,13800,10800,8600,4800,
                         2400,700,300,0,0,0,0   ) /1000000


Immigration_ratio = Immigration_age_bins/NI0




###### net imigration age bins loop #####


#single year net immigration
a_im= .25

m_im= .45

b_im= .75

# quiniial immigration
a_im_quin = 1.25

m_im_quin = 2.25

b_im_quin = 3.75

Net_IM_age_bins = rep(NA,length(Immigration_age_bins))




for (i in 1:length(Immigration_age_bins)) {
  
  Net_IM_i = draw(a_im,b_im,m_im)
  
  #quinnial net immigration
  quin_im  = draw(a_im_quin, b_im_quin, m_im_quin)
  while(abs(Net_IM_i-NI0)>quin_im){
    # keeps redrawing until the above condition is met
    Net_IM_i = draw(a_im,b_im,m_im)
  }
  
  
  
  
  Net_IM_age_bins[i] =  Net_IM_i*Immigration_ratio[i]
  
  
  
  
}




######Immigration function#####

imm_fxn = function(year) {
  
  NI0 = 450000/1000000
  
  Immigration_age_bins = c(32600,37300,42600,46300,57500,74800,49000,
                           31100,21100,16100,13800,10800,8600,4800,
                           2400,700,300,0,0,0,0   ) /1000000
  
  
  Immigration_ratio = Immigration_age_bins/NI0
  #single year net immigration
  a_im= .25
  
  m_im= .45
  
  b_im= .75
  
  # quiniial immigration
  a_im_quin = 1.25
  
  m_im_quin = 2.25
  
  b_im_quin = 3.75
  
  Net_IM_age_bins = rep(NA,length(Immigration_age_bins))
  
  
  for (i in 1:length(Immigration_age_bins)) {
    
    Net_IM_i = draw(a_im,b_im,m_im)
    
    #quinnial net immigration
    quin_im  = draw(a_im_quin, b_im_quin, m_im_quin)
    while(abs(Net_IM_i-NI0)>quin_im){
      # keeps redrawing until the above condition is met
      Net_IM_i = draw(a_im,b_im,m_im)
    }
    
    
    
    
    Net_IM_age_bins[i] =  Net_IM_i*Immigration_ratio[i]
    
    
    
  }
  return(Net_IM_age_bins)
  
}

imm_fxn(1987)



##### Actual Population for 1982 #######

# population in thousands so each of these are millions

population_age_bins_0 = c(17372,15956,18024,19845,21935,20769,18712,
                          15684,12460,11049,11331,11521,10573,8939,
                          7195,5106,3138,1637,620,155,32)



#################### first year calculations ########

# population
Ni = population_age_bins_0*1000

#net immigration
NI_i = Net_IM_age_bins*1000000


#birthrate

Ft_i = Birth_rate/1000

#   [FFFFFFF] *[Ni Ni]+ [NI_i]
#   [PPPPPPP]

########### Matrix for year 1  and year 1 calculations#######

# 1-5 ,5-9,10-14,15-19  

survive_0_1 = (1000-16)/ 1000

survive_1_4 = (1000-.7)/1000

survive_1_5= survive_0_1 * survive_1_4

survive_5_9 = (1000-0.3)/1000

survive_10_14 = (1000-0.3)/1000

survive_15_19= (1000-1.1)/1000

survive_20_24 = (1000-1.1)/1000

survive_25_29 = (1000-1.4)/1000

survive_30_34 = (1000-1.4)/1000

survive_35_39 = (1000-2.5)/1000

survive_40_44 = (1000-2.5)/1000

survive_45_49 = (1000-6.3)/1000

survive_50_54 = (1000-6.3)/1000

survive_55_59 = (1000-14.8)/1000

survive_60_64 = (1000-14.8)/1000

survive_65_69 = (1000-31.3)/1000

survive_70_74 = (1000-31.3)/1000

survive_75_79 = (1000-73.3)/1000

survive_80_84 = (1000-73.3)/1000

survive_85_89 = (1000-154.9)/1000

survive_90_94 = (1000-154.9)/1000

survive_94_99= (1000-154.9)/1000

survive_over100= (1000-154.9)/1000


vec1 <- c(survive_1_5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
vec2 <- c(0,survive_5_9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
vec3 <- c(0,0,survive_10_14,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
vec4 <- c(0,0,0,survive_15_19,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
vec5 <- c(0,0,0,0,survive_20_24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
vec6 <- c(0,0,0,0,0,survive_25_29,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
vec7 <- c(0,0,0,0,0,0,survive_30_34,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
vec8 <- c(0,0,0,0,0,0,0,survive_35_39,0,0,0,0,0,0,0,0,0,0,0,0,0)
vec9 <- c(0,0,0,0,0,0,0,0,survive_40_44,0,0,0,0,0,0,0,0,0,0,0,0)
vec10 <- c(0,0,0,0,0,0,0,0,0,survive_45_49,0,0,0,0,0,0,0,0,0,0,0)
vec11 <- c(0,0,0,0,0,0,0,0,0,0,survive_50_54,0,0,0,0,0,0,0,0,0,0)
vec12 <- c(0,0,0,0,0,0,0,0,0,0,0,survive_55_59,0,0,0,0,0,0,0,0,0)
vec13 <- c(0,0,0,0,0,0,0,0,0,0,0,0,survive_60_64,0,0,0,0,0,0,0,0)
vec14 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,survive_65_69,0,0,0,0,0,0,0)
vec15 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,survive_70_74,0,0,0,0,0,0)
vec16 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,survive_75_79,0,0,0,0,0)
vec17 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,survive_80_84,0,0,0,0)
vec18 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,survive_85_89,0,0,0)
vec19 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,survive_90_94,0,0)
vec20 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,survive_94_99,0)
vec21 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,survive_over100)

FT <- c(0,0,Ft_i,0,0,0,0,0,0,0,0,0,0,0)
length(FT)

### leslie matrix
LeslieMatrix <- rbind(FT,vec1,vec2,vec3,vec4,vec5,vec6,vec7,vec8,vec9,vec10,vec11,vec12,
                      vec13,vec14,vec15,vec16,vec17,vec18,vec19,vec20,vec21)


#### Ni_t each years population bina and birth  totals calculation.##
Pop0 <- LeslieMatrix%*%Ni+NI_i

head(Pop0)

## first year population
sum(Pop0)
Pop0



 #### Leslie function ##############
leslie_fxn = function(year) {
  survive_0_1 = (1000-16)/ 1000
  
  survive_1_4 = (1000-.7)/1000
  
  survive_1_5= survive_0_1 * survive_1_4
  
  survive_5_9 = (1000-0.3)/1000
  
  survive_10_14 = (1000-0.3)/1000
  
  survive_15_19= (1000-1.1)/1000
  
  survive_20_24 = (1000-1.1)/1000
  
  survive_25_29 = (1000-1.4)/1000
  
  survive_30_34 = (1000-1.4)/1000
  
  survive_35_39 = (1000-2.5)/1000
  
  survive_40_44 = (1000-2.5)/1000
  
  survive_45_49 = (1000-6.3)/1000
  
  survive_50_54 = (1000-6.3)/1000
  
  survive_55_59 = (1000-14.8)/1000
  
  survive_60_64 = (1000-14.8)/1000
  
  survive_65_69 = (1000-31.3)/1000
  
  survive_70_74 = (1000-31.3)/1000
  
  survive_75_79 = (1000-73.3)/1000
  
  survive_80_84 = (1000-73.3)/1000
  
  survive_85_89 = (1000-154.9)/1000
  
  survive_90_94 = (1000-154.9)/1000
  
  survive_94_99= (1000-154.9)/1000
  
  survive_over100= (1000-154.9)/1000
  
  vec1 <- c(survive_1_5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  vec2 <- c(0,survive_5_9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  vec3 <- c(0,0,survive_10_14,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  vec4 <- c(0,0,0,survive_15_19,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  vec5 <- c(0,0,0,0,survive_20_24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  vec6 <- c(0,0,0,0,0,survive_25_29,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  vec7 <- c(0,0,0,0,0,0,survive_30_34,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  vec8 <- c(0,0,0,0,0,0,0,survive_35_39,0,0,0,0,0,0,0,0,0,0,0,0,0)
  vec9 <- c(0,0,0,0,0,0,0,0,survive_40_44,0,0,0,0,0,0,0,0,0,0,0,0)
  vec10 <- c(0,0,0,0,0,0,0,0,0,survive_45_49,0,0,0,0,0,0,0,0,0,0,0)
  vec11 <- c(0,0,0,0,0,0,0,0,0,0,survive_50_54,0,0,0,0,0,0,0,0,0,0)
  vec12 <- c(0,0,0,0,0,0,0,0,0,0,0,survive_55_59,0,0,0,0,0,0,0,0,0)
  vec13 <- c(0,0,0,0,0,0,0,0,0,0,0,0,survive_60_64,0,0,0,0,0,0,0,0)
  vec14 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,survive_65_69,0,0,0,0,0,0,0)
  vec15 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,survive_70_74,0,0,0,0,0,0)
  vec16 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,survive_75_79,0,0,0,0,0)
  vec17 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,survive_80_84,0,0,0,0)
  vec18 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,survive_85_89,0,0,0)
  vec19 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,survive_90_94,0,0)
  vec20 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,survive_94_99,0)
  vec21 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,survive_over100)
  
  #birth function gives birthrates in thousands
  
  
  FT <- c(0,0,birth_fxn(year)/1000,0,0,0,0,0,0,0,0,0,0,0)
  length(FT)
  
  LeslieMatrix <- rbind(FT,vec1,vec2,vec3,vec4,vec5,vec6,vec7,vec8,vec9,vec10,vec11,vec12,
                        vec13,vec14,vec15,vec16,vec17,vec18,vec19,vec20,vec21)

  return(LeslieMatrix)
  }

leslie_fxn(1982)








# last year of projections.
end_year = 80
sim_num = 10

#### populations for the current year


Pop_i = rep(NA,end_year)

## need the first previous year 1982 to calculate future years 
#iteratively for 1983 and we want 10 or 1000 1983's 

Pop_i[1] = sum(Pop0)


# multiple current populations for the year are saved and will
#be averaged to make the monte carlo
#so pop j is the next iteration of population
# after saving pop j we go to the next iteration
Pop_j = rep(NA,sim_num)

### iterative population age bins and fertility rate 
Ni_t_list = list()

# adding 
Ni_t_list[[1]] = Pop0[2:22]

####Immigration list

NI_t_list= list()

## populated with 1982 at first and then updated iterativley
NI_t_list[[1]] = NI_i

#Leslie matricies

leslie_list = list()


### populated with 1982 at first and then updated iterativley

leslie_list[[1]] = LeslieMatrix

deaths = rep(NA, end_year)

added = rep(NA,end_year)

current_pop_list = list()

Immigrate_added = rep(NA,end_year)

# starting in 1983 using each previous years data to update 
#the current years projections
for (j  in 1:sim_num) {
  
}
for(i in 2:end_year) {
  
  # population age bins for the previous year
  Ni_t = Ni_t_list[[i-1]]
 
  
  #  previous year net immigration
  NI_t = NI_t_list[[i-1]]
  
  
  
  ### current year leslie matricies
   prev_matrix =     leslie_list[[i-1]]
  
     ### Lesliematrix * population age bins + net immigration =
  ### The current years population age bins with fertility
  
  current_pop_bins_with_fertilty <- prev_matrix%*%Ni_t+ NI_t
  current_pop_list[[i-1]] = current_pop_bins_with_fertilty
  
  #people added current year 1
  added[i-1] = current_pop_bins_with_fertilty[1]
  
  
  
  #population age bins for the current year
  Ni_t_list[[i]] = current_pop_bins_with_fertilty[2:22]
  
  # add babies to the 0-5 bin
  Ni_t_list[[i]][1] = Ni_t_list[[i]] [1]+ added[i-1]
  
  
  ##### population = the sum of the current years age bins
  Pop_i[i] <- sum(current_pop_bins_with_fertilty)
  
  
  deaths[i-1] = sum(Pop_i[i])- sum(Ni_t_list[[i-1]]) 
  
 
 
  
  #  current year net immigration age bins 
  
  current_immigration =imm_fxn(i)*1000000
  
  NI_t_list[[i]] = current_immigration
  
 
  
  ### current year leslie matricies and birthrate is also updated
  #randomly
  leslie_list[[i]] = leslie_fxn(i)
  
}

# end of inner loop

# collecting all of the populations before going to the next year 
# and getting the mean
# pop j is cxomposed of multiple pop i's 


Pop_j[j] = mean(Pop_i)

}

#end of outer loop
Pop_i

# monte carlo population mean of  current populations list  
monte_carlo_pop = mean(Pop_i)

deaths

added

NI_t_list

Ni_t_list

#current_pop_list


Immigrate_added

NI_t



Ni_t
leslie_list[[3]]

Ni_t_list
