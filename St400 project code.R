
# fertility rates for 1982 in thousands
eighty2 = c(1.3,53.7, 115.5,111.8,60.2,19.6,4,0.2)/1000

x0 = 1.8

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


# women Population vector 

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


##### Immigration

NI0 = 450000/1000000

#Immigration age bins in 1/1million


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



##### Actual Population

# in thousands so each of these are millions

population_age_bins_0 = c(17372,15956,18024,19845,21935,20769,18712,
                          15684,12460,11049,11331,11521,10573,8939,
                          7195,5106,3138,1637,620,155,32)

# population
Ni = population_age_bins_0*1000

# Matrix

#net immigration
NI_i = Net_IM_age_bins*1000000


#birthrate

Ft_i = Birth_rate*1000

#   [FFFFFFF] *[Ni Ni]+ [NI_i]
#   [PPPPPPP]


# rbind create matracies.rbind(Ft_i,)


#survivalship


# 1-5 ,5-9,10-14,15-19  

survive_0_1 = (1000-16)/ 1000

survie_1_4 = (1000-.7)/1000

survive_1_5= 






Leslie0 = matrix(ncol = 21)

  