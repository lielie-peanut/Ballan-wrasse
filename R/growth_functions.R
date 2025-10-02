###Von Bertalanffy

VB <- function(age,Linf,k,t0){
  #age is the age of the fish
  #Linf= maximal fish size
  #k = growth coefficient
  #t0 = size at t0
  Linf*(1-exp(-k*(age-t0)))
}

### mass

mass <- function(a0,a1, L){
  #a0 and a1 = parameters defining the mass-length relationship
  #L = length at age
  a0*L^a1
}