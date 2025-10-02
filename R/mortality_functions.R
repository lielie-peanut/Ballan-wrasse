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
  (1/(1+exp(-((L-Lmin)/mu))))*(1-(1/(1+exp(-((L-Lmax)/mu)))))*Fm
}

###catches

catch <- function(Nf,Nm,Fm,Z){
  #all arguments are vectors, with 
  #one element of a vector = the value for a given age
  (Fm/Z)*(1-exp(-Z))*Nf+(Fm/Z)*(1-exp(-Z))*Nm
}

###yield

yield <- function(Cf,Cm,mm,mf){
  #all arguments are vectors
  Cf*mf/1000+Cm*mm/1000
}