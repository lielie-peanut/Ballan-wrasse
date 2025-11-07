###sex change

#outputs the probability for a sex change to happen at length
sex_change <- function(L,Li,deltaL, b){
  #L = length at age
  #Li = average size in the population
  #deltaL = difference above the mean at which 
  #the probability of undergoing sex change equals 0.5
  #b = shape parameter
  1/(1+exp(-b*(L-(Li+deltaL))))
}

###number of females

#outputs the number of females at age+1
N_F <- function(Nfemales,Pchange,Lfemales,Z){
  #Nfemales = number of females at age
  #Pchange = the probability of a sex change happening (see sex_change function)
  #Lfemales = length at age of females
  #Z = mortality
  (Nfemales-(Nfemales*Pchange))*exp(-Z)
}

###number of males

#outputs the number of males at age+1
N_M <- function(Nfemales,Nmales,Pchange,Lfemales,Z){
  #Nfemales = number of females at age
  #Nmales = number of males at age
  #Pchange = the probability of a sex change happening (see sex_change function)
  #Lfemales = length at age of females
  #Z = mortality
  (Nmales+(Nfemales*Pchange))*exp(-Z)
}

###fecundity

#calculates the total number of eggs produced by one female
egg_count <- function(Beta0, Beta1, L){
  #L = length
  #Beta0 and Beta1 = parameters that define the egg count at length relationship
  exp(Beta0*L+Beta1)
}

###fertilization

#outputs a fertilization rate at a specific time based on male availability
fert_rate <- function(k,xF){
  #k = measure of steepness and defines how affected 
  #fertilization is by male depletion
  #xF = the ratio of the proportion of males at time t 
  #to the proportion of males in a stable unfished population
  (4*k*xF)/((1-k)+(5*k-1)*xF)
}

###male to ideal male comparison

male_ratio <- function(Pt,Pz){
  #Pt= male porportion at time t
  #Pz= male proportion in a stable unfished population
  Pt/Pz
}

###stock recruitment

#number of recruits to the age0 class based on egg count and fertilization
spawn_recruitment <- function(h,R0, sigma,teta0){
  #h = measure of steepness
  #R0 = unfished recruitment
  #sigma = number of fertilized eggs
  #teta0 = number of fertilized eggs per recruit 
  #in the unfished stable population
  num <- 4*h*R0*sigma
  denom <- R0*teta0*(1-h)+(5*h-1)*sigma
  if (denom == 0) return(0)
  return(num / denom)
}

