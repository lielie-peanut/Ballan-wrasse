###mortality

#outputs the number of fish surviving to the next year 
#(only when sex change is not included)
natural_mortality <- function(N,Z){
  #N = number of fish
  #Z= mortality rate (natural + fishing)
  N*exp(-Z)
}

###fishing recruitment

fishing_recruitment <- function(L,Lmin, Lmax,mu,Fm){
  #L = length of the fish
  #Lmin = minimum fishing size
  #Lmax = maximum fishing size
  #mu = 
  #Fm = fishing mortality rate
  (1/(1+exp(-((L-Lmin)/mu))))*(1-(1/(1+exp(-((L-Lmax)/mu)))))*Fm
}

###catches

catch <- function(N,Fm,Z){
  #N = Number of individuals (males or females)
  #Fm = fishing recruitment at age
  #Z = total mortality at age (natural + fishing)
  (Fm/Z)*(1-exp(-Z))*Nf
}

###yield

yield <- function(Cf,Cm,mm,mf){
  #Cf = catch females
  #Cm = catch males
  #mm = mass males
  #mf = mass females
  Cf*mf/1000+Cm*mm/1000
}