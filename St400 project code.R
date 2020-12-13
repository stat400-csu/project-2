

##########Draw Function #########

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


###### loop tro get fertility age bins
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

#### loop to create new immigration age bins


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




#################setting up the monte carlo simulation #######



# last year of projections.
end_year = 98
sim_num = 100
sim_num2 = 1000
sim_num3 = 10000




#### populations for the current year


#Pop_i = rep(NA,end_year)

## need the first previous year 1982 to calculate future years 
#iteratively for 1983 and we want 10 or 1000 1983's 

#Pop_i[1] = sum(Pop0)








# multiple current populations for the year are saved and will
#be averaged to make the monte carlo
#so pop j is the next iteration of population
# after saving pop j we go to the next iteration
#Pop_j = rep(NA,sim_num)

#Pop_j[1] = sum(Pop0)


#Monte_carlo_pop = rep(NA,end_year)


#Monte carlo age bins
# The sum of this with fertility added in to the firsr
# vector row = population
#Monte_carlo_age_bins_list= list()

# first year is always 1982 Pop 0 is 1982's age bins with feretility
# it has to be subb setted by removing the fertility


#Monte_carlo_age_bins_list[[1]] =  Pop0[2:22]

#add in the 1982 births
#Monte_carlo_age_bins_list[[1]][1] = Monte_carlo_age_bins_list[[1]][1]+
                                    Pop0[1]

# added increase the population. Its the fertility 
# we will add this to the 0-5 age bin

#added_monte_carlo = rep(NA,sim_num)

# innitially nothing is added
#added_monte_carlo[1] =0


## Immigration for monte carlo
#Imm_monte_list = list()

# first year has to use 1982's  immigration in order to make
# the ratio multiplier

#Imm_monte_list[[1]] = NI_i



## Leslie matrix use 1982s for first year because we need the
# original ratio multiplier

Leslie_Monte_list = list()

# 1982 s leslie matrix to make 1983's
 
Leslie_Monte_list[[1]] =  LeslieMatrix

#Save simulations in a simlength x end year long matrix
#[,j] subsets rows [i,](up and down) subsets columns(left and right)
# [same row number,j] lets you add values left to right in the same row
sim_matrix100 = matrix(0,sim_num,end_year)
sim_matrix1000 = matrix(0,sim_num2,end_year)
sim_matrix10000 = matrix(0,sim_num3,end_year)


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

deaths = rep(NA, sim_num)

added = rep(NA,end_year)

current_pop_list = list()

Immigrate_added = rep(NA,end_year)


########## begginning of loop ######

# starting in 1983 using each previous years data to update 
#the current years projections. We each set of projections in a single
# row of a matrix with dimenions simnumX endyear. 
# So rows will be endyear long
# we will simulate 1000s of these projections so the number of
#columns is sim num long


##########Simulating 100 times#################

for (i in 1:sim_num) {
  # saves a new set of projections(rows)
  for (j in 2:end_year) {
    # j starts at 2 because we need to do j-1 
    #to get the previous year erach time
    # saves a new year into a projection set(columns) 
    
    # start with 1982's populatioon age bins vector
    Ni_t =   Ni_t_list[[j-1]]
    
    #Start with 1982's leslie matrix
    leslie = leslie_list[[j-1]]
    
    #start with 1982's net immigration age bins vector
    
    NI_t = NI_t_list[[j-1]]
    
    #Get the 22x1 current population bins vector with births
    
    Ni_t1_with_births = leslie %*% Ni_t +NI_t
    
    # save births
    added[j-1] = Ni_t1_with_births[1]
    
    #remove the births column of the matrix but add the number
    # of births into the 0-5 age bin
    
     Ni_t1 = Ni_t1_with_births[2:22]
     
     Ni_t1[1] = Ni_t1[1]+ added[j-1]
     
     #save the new age bins into the age bins list
     
     Ni_t_list[[j]] = Ni_t1
     
     #save the population into a column(j) in a single projection row(i)
     
     sim_matrix100[i,j] = sum(Ni_t1)
     
     # update fertility and immigration randomly/stochasticly
     
     leslie_list[[j]] = leslie_fxn(j)
     
     NI_t_list[[j]] = imm_fxn(j) * 1000000
    
  }
}


########Simulating 1000 times##############e

for (i in 1:sim_num2) {
  # saves a new set of projections(rows)
  for (j in 2:end_year) {
    # j starts at 2 because we need to do j-1 
    #to get the previous year erach time
    # saves a new year into a projection set(columns) 
    
    # start with 1982's populatioon age bins vector
    Ni_t =   Ni_t_list[[j-1]]
    
    #Start with 1982's leslie matrix
    leslie = leslie_list[[j-1]]
    
    #start with 1982's net immigration age bins vector
    
    NI_t = NI_t_list[[j-1]]
    
    #Get the 22x1 current population bins vector with births
    
    Ni_t1_with_births = leslie %*% Ni_t +NI_t
    
    # save births
    added[j-1] = Ni_t1_with_births[1]
    
    #remove the births column of the matrix but add the number
    # of births into the 0-5 age bin
    
    Ni_t1 = Ni_t1_with_births[2:22]
    
    Ni_t1[1] = Ni_t1[1]+ added[j-1]
    
    #save the new age bins into the age bins list
    
    Ni_t_list[[j]] = Ni_t1
    
    #save the population into a column(j) in a single projection row(i)
    
    sim_matrix1000[i,j] = sum(Ni_t1)
    
    # update fertility and immigration randomly/stochasticly
    
    leslie_list[[j]] = leslie_fxn(j)
    
    NI_t_list[[j]] = imm_fxn(j) * 1000000
    
  }
}


################Simulating 10000 times###################

for (i in 1:sim_num3) {
  # saves a new set of projections(rows)
  for (j in 2:end_year) {
    # j starts at 2 because we need to do j-1 
    #to get the previous year erach time
    # saves a new year into a projection set(columns) 
    
    # start with 1982's populatioon age bins vector
    Ni_t =   Ni_t_list[[j-1]]
    
    #Start with 1982's leslie matrix
    leslie = leslie_list[[j-1]]
    
    #start with 1982's net immigration age bins vector
    
    NI_t = NI_t_list[[j-1]]
    
    #Get the 22x1 current population bins vector with births
    
    Ni_t1_with_births = leslie %*% Ni_t +NI_t
    
    # save births
    added[j-1] = Ni_t1_with_births[1]
    
    #remove the births column of the matrix but add the number
    # of births into the 0-5 age bin
    
    Ni_t1 = Ni_t1_with_births[2:22]
    
    Ni_t1[1] = Ni_t1[1]+ added[j-1]
    
    #save the new age bins into the age bins list
    
    Ni_t_list[[j]] = Ni_t1
    
    #save the population into a column(j) in a single projection row(i)
    
    sim_matrix10000[i,j] = sum(Ni_t1)
    
    # update fertility and immigration randomly/stochasticly
    
    leslie_list[[j]] = leslie_fxn(j)
    
    NI_t_list[[j]] = imm_fxn(j) * 1000000
    
  }
}

############## Creating Table ###################

######Creating vectors for 100 sims#########

eightyseven_100 = sim_matrix100[,6]
ninetytwo_100 = sim_matrix100[,11]
ninetyseven_100 = sim_matrix100[,16]
zerotwo_100 = sim_matrix100[,21]
twelve_100 = sim_matrix100[,31]
twentytwo_100 = sim_matrix100[,41]
thirtytwo_100 = sim_matrix100[,51]
fourtytwo_100 = sim_matrix100[,61]
fiftytwo_100 = sim_matrix100[,71]
sixtytwo_100 = sim_matrix100[,81]
seventytwo_100 = sim_matrix100[,91]

###########Creating vectors for 1000 sims##########


eightyseven_1000 = sim_matrix1000[,6]
ninetytwo_1000 = sim_matrix1000[,11]
ninetyseven_1000 = sim_matrix1000[,16]
zerotwo_1000 = sim_matrix1000[,21]
twelve_1000 = sim_matrix1000[,31]
twentytwo_1000 = sim_matrix1000[,41]
thirtytwo_1000 = sim_matrix1000[,51]
fourtytwo_1000 = sim_matrix1000[,61]
fiftytwo_1000 = sim_matrix1000[,71]
sixtytwo_1000 = sim_matrix1000[,81]
seventytwo_1000 = sim_matrix1000[,91]

#####Creating vectors for 10000 sims#########

eightyseven_10000 = sim_matrix10000[,6]
ninetytwo_10000 = sim_matrix10000[,11]
ninetyseven_10000 = sim_matrix10000[,16]
zerotwo_10000 = sim_matrix10000[,21]
twelve_10000 = sim_matrix10000[,31]
twentytwo_10000 = sim_matrix10000[,41]
thirtytwo_10000 = sim_matrix10000[,51]
fourtytwo_10000 = sim_matrix10000[,61]
fiftytwo_10000 = sim_matrix10000[,71]
sixtytwo_10000 = sim_matrix10000[,81]
seventytwo_10000 = sim_matrix10000[,91]

######Put into matrices#########

quantile100 = matrix(data = NA, nrow = 11, ncol = 100)

quantile1000 = matrix(data = NA, nrow = 11, ncol = 1000)

quantile10000 = matrix(data = NA, nrow = 11, ncol = 10000)


quantile100[1,] = eightyseven_100
quantile100[2,] = ninetytwo_100
quantile100[3,] = ninetyseven_100
quantile100[4,] = zerotwo_100
quantile100[5,] = twelve_100
quantile100[6,] = twentytwo_100
quantile100[7,] = thirtytwo_100
quantile100[8,] = fourtytwo_100
quantile100[9,] = fiftytwo_100
quantile100[10,] = sixtytwo_100
quantile100[11,] = seventytwo_100


quantile1000[1,] = eightyseven_1000
quantile1000[2,] = ninetytwo_1000
quantile1000[3,] = ninetyseven_1000
quantile1000[4,] = zerotwo_1000
quantile1000[5,] = twelve_1000
quantile1000[6,] = twentytwo_1000
quantile1000[7,] = thirtytwo_1000
quantile1000[8,] = fourtytwo_1000
quantile1000[9,] = fiftytwo_1000
quantile1000[10,] = sixtytwo_1000
quantile1000[11,] = seventytwo_1000


quantile10000[1,] = eightyseven_10000
quantile10000[2,] = ninetytwo_10000
quantile10000[3,] = ninetyseven_10000
quantile10000[4,] = zerotwo_10000
quantile10000[5,] = twelve_10000
quantile10000[6,] = twentytwo_10000
quantile10000[7,] = thirtytwo_10000
quantile10000[8,] = fourtytwo_10000
quantile10000[9,] = fiftytwo_10000
quantile10000[10,] = sixtytwo_10000
quantile10000[11,] = seventytwo_10000


quantile100t = t(quantile100)
quantile1000t = t(quantile1000)
quantile10000t = t(quantile10000)



ci_100 = apply( quantile100t, 2 , quantile , probs = c(0.025, 0.5, 0.975) , na.rm = TRUE )

ci_1000 = apply( quantile1000t, 2 , quantile , probs = c(0.025, 0.5, 0.975) , na.rm = TRUE )

ci_10000 = apply( quantile10000t, 2 , quantile , probs = c(0.025, 0.5, 0.975) , na.rm = TRUE )

sd_100 = apply(quantile100t, 2, sd, na.rm = TRUE)

sd_1000 = apply(quantile1000t, 2, sd, na.rm = TRUE)

sd_10000 = apply(quantile10000t, 2, sd, na.rm = TRUE)


tfor100 = qt(.975, 99)

tfor1000 = qt(.975, 999)

tfor10000 = qt(.975, 9999)

standard_error = function(quantilematrix, n) {
  
  apply(quantilematrix, 2, sd, na.rm = TRUE)/sqrt(n)
}


moe100 = standard_error(quantile100t, 100)*tfor100
moe1000 = standard_error(quantile1000t, 1000) * tfor1000
moe10000 = standard_error(quantile10000t, 10000) * tfor10000

means100 = colMeans(quantile100t)
means1000 = colMeans(quantile1000t)
means10000 = colMeans(quantile10000t)

conf_int_matrix100 = matrix(data = NA, nrow = 3, ncol = 11)
conf_int_matrix1000 = matrix(data = NA, nrow = 3, ncol = 11)
conf_int_matrix10000 = matrix(data = NA, nrow = 3, ncol = 11)

conf_int_matrix100[1,] = means100 + moe100
conf_int_matrix100[2,] = means100
conf_int_matrix100[3,] = means100 - moe100

conf_int_matrix1000[1,] = means1000 + moe1000
conf_int_matrix1000[2,] = means1000
conf_int_matrix1000[3,] = means1000 - moe1000

conf_int_matrix10000[1,] = means10000 + moe10000
conf_int_matrix10000[2,] = means10000
conf_int_matrix10000[3,] = means10000 - moe10000

#for (i  in 2:end_year) {
  # after monte carlo we collect the mean  from the population to get 
  # the 1983 monte carlo  population and all next years afterwards
  
  
  # container to hold monte carlo age bins
  # we get the mean later on 
  
 # Pop_bin_sums = matrix(0,nrow =21 , ncol = 1)
  

  
  

#for(j in 2:sim_num) {
  
  
  
  
  # Starting with the previous years monte carlo age bins
  # initialy 1982
  # we will keep age bins the same 
#calculating 1983_aLeslie * 1983_aPopulation_bins +1983_aImmigration
 # then we do 1983_bLeslie * 1983_bPopulation_bins +1983_bImmigration
# where 1983bLeslie is randomly drawn and  1983bPopulation_bins 
 # is the same and 1983b immigration is randomly drawn
   

 # 1982 population age bins to make 1983_aPopulation which is also 1983b.. 
  #and so  on since population age bins won't change
 # Monte_Nt = Monte_carlo_age_bins_list[[j-1]]
  
  # add each new population age bin
  
  
 
  
   #1982 immigration bins to make 1983a_ immigration
  # 1983 immigrationbins_b will be draw randomly after saving 
  # populationa
#  Monte_NI_t = Imm_monte_list[[j-1]]
  
  
  #1982 Leslie to make 1983Lesliea 
  # 1983 Leslie_b will be draw randomly after saving 
  # populationa
 #  Monte_matrix =   Leslie_Monte_list[[j-1]]
  
#calculating 1983_aLeslie * 1983_aPopulation_bins +1983_aImmigration
# then we do 1983_bLeslie * 1983_bPopulation_bins +1983_bImmigration
# result is a vector that is 22*1 because the first elemnt is total
#births .So it needs to be subb setted back to a 21x1 but total
# births won't be added because the age_bins must stay the same
   
  
   
#   Monte_current_age = Monte_matrix %*% (Monte_Nt+ Monte_NI_t)
   
 
   #shrink it by subsetting back to 21X1  so 22X21 * 21X1
   # saves the current years age bins and it will stay the same
   #for the next j(simulation) iterations but needs deaths added
   #back
   #1983_populationbinsa
   
 #  Monte_carlo_age_bins_list[[j]] = Monte_current_age[2:22]
   
   
  
   
   
   
   
   # To make it stay the same deaths have to be added back in to
   #population
   # previous deaths are deaths per bin so they need to be summed
   # to get total deaths then added back to pop aage bins
 #  prev_deaths = abs(Monte_carlo_age_bins_list[[j]]-
 #                   Monte_carlo_age_bins_list[[j-1]])
   
 #  deaths[j] = sum(prev_deaths)
   
   
   
 #  Monte_carlo_age_bins_list[[j]] = Monte_Nt+prev_deaths
   
#   Pop_bin_sums = Pop_bin_sums + Monte_carlo_age_bins_list[[j]]
   
   
   
   
   # sum the current year age bins to get current year population
   # save j populations, we called  j :sim_num
   # get the mean after everything is saved 
   #this will become 1983 populationb
   
   
   
   # subtract the first immigration
#   Pop_j[j] = sum(Monte_carlo_age_bins_list[[j]])-
#     sum(Imm_monte_list[[j-1]])
   
   
   
  
   # change leslie for the next iteration randomly
   # leslie changes the birthrates randomly
   # 1983 leslie b . So we went from a-b
   
#   Leslie_Monte_list[[j]] = leslie_fxn(j)
   
   # change immigration for the next iteration randomly
   # 1983 immigration b . So we went from a-b
#   Imm_monte_list[[j]] = imm_fxn(j)*1000000
   
 
#}
  
  ######## end on inner loop###
  
  
  
  
  # get the mean population age bins which is the average
  # of popbin_sums
  # so then this is 1982's age bins
#  mean_age_bins = rowMeans(Pop_bin_sums)
  
  # collect each monte carlo population
  
 #  Monte_carlo_pop[i] = sum(mean_age_bins)
   
   
#   Pop_i[i] = mean(Pop_j)
   
   
    
#   Ni_t = Ni_t_list[[i-1]]
   
   #previous immigration
   
#   NI_t =NI_t_list[[i-1]]
   
   
   ### current year leslie matricies
#   prev_matrix =     leslie_list[[i-1]]
   
   ### Lesliematrix * population age bins + net immigration =
   ### The current years population age bins with fertility
   
#   current_pop_bins_with_fertilty <- prev_matrix%*% (Ni_t+ NI_t)
#   current_pop_list[[i-1]] = current_pop_bins_with_fertilty
   
   #people added current year 1
#   added[i-1] = current_pop_bins_with_fertilty[1]
   
   
   
   #population age bins for the current year
  # Ni_t_list[[i]] = current_pop_bins_with_fertilty[2:22]
   
   # add babies to the 0-5 bin
 #  Ni_t_list[[i]][1] = Ni_t_list[[i]][1]+ added[i-1]
   
   
   ##### population = the sum of the current years age bins
   #with fertility
   
   #change inside population and population age bins
#   Pop_j[1] = sum(current_pop_bins_with_fertilty)
   
#   Monte_carlo_age_bins_list[[1]] = Ni_t_list[[i]]
 
 #  leslie_list[[i]] = leslie_fxn(i)
   
   # change immigration for the next iteration randomly
   # 1983 immigration b . So we went from a-b
#   NI_t_list[[i]] = imm_fxn(i)*1000000
   
   


#}

#end of outer loop
# previous years age bins








#  current year net immigration age bins 
#current_immigration =imm_fxn(i)*1000000
#NI_t_list[[i]] = current_immigration

### current year leslie matricies and birthrate is also updated
#randomly
#leslie_list[[i]] = leslie_fxn(i)















# get 1983 or what ever is the current years leslie matrix randomly
# so that its ready for the next iteration meaning 1983-1984
#Leslie_Monte_list[[j]] = leslie_fxn(j)

# get 1983 or what ever is the current years immigration randomly
# so that its ready for the next iteration meaning 1983-1984
#Imm_monte_list[[j]] =  imm_fxn(j)*1000000

# deaths[j-1] = sum(Pop_j[j])- sum(Monte_carlo_age_bins_list[[j-1]]) 









# That mean gets saved into Popi
# popi saves our population projections

#Pop_i[i] <- mean(Pop_j)



# end of inner loop




# monte carlo population mean of  current populations list  
#monte_carlo_pop = mean(Pop_i)




