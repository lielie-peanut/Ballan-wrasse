###Von Bertalanffy

VB <- function(age,Linf,k,t0){
  Linf*(1-exp(-k*(age-t0)))
}

### mass

mass <- function(a0,a1, L){
  a0*L^a1
}