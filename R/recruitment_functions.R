###sex change
sex_change <- function(L,Li,deltaL, b){
  1/(1+exp(-b(L-(Li+deltaL))))
}

###number of females

N_F <- function(Nfemales,Pchange,Lfemales,Z){
  (Nfemales-(Nfemales*Pchange(Lfemales)))*exp(-Z)
}

###number of males

N_M <- function(Nfemales,Nmales,Pchange,Lfemales,Z){
  (Nmales+(Nfemales*Pchange(Lfemales)))*exp(-Z)
}

###fecundity

egg_count <- function(Beta0, Beta1, L){
  exp(Beta0*L+Beta1)
}

###fertilization

fert_rate <- function(k,xF){
  (4*k*xF)/((1-k)+(5*k-1)*xF)
}

###male to ideal male comparison

male_ratio <- function(Pt,Pz){
  Pt/Pz
}

###stock recruitment

spawn_recruitment <- function(h,R0, sigma,teta0){
  (4*h*R0*sigma)/(R0*teta0*(1-h)+(5*h-1)*sigma)
}

###fishing recruitment

fishing_recruitment <- function(L,Lmin, Lmax,mu,Fm){
  (1/(1+exp(-((L-Lmin)/mu))))*(1-(1/(1+exp(-((L-Lmax)/mu)))))*Fm
}
