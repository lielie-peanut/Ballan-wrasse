###mortality

natural_mortality <- function(N,Z){
  N*exp(-Z)
}

###fishing recruitment

fishing_recruitment <- function(L,Lmin, Lmax,mu,Fm){
  (1/(1+exp(-((L-Lmin)/mu))))*(1-(1/(1+exp(-((L-Lmax)/mu)))))*Fm
}