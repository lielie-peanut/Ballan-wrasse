###simulation

#loading functions from another file
source("R/mortality_functions.R")
source("R/growth_functions.R")
source("R/recruitment_functions.R")

#loading data
parameters <- read.csv("data/parameters.csv", stringsAsFactors = F,sep = ";")
params_list <- as.list(setNames(parameters$value,parameters$symbol))
print(params_list)

#loading the stable parameters

#growth 
ages <- 0:30
Lf <- VB(ages,Linf = params_list$LinfF,k = params_list$KF,t0 = params_list$t0F)
Lm <- VB(ages,Linf = params_list$LinfM,k = params_list$KM,t0 = params_list$t0M)

#mass
massF <- mass(a0 = params_list$alpha0, a1 = params_list$alpha1,L = Lf)
massM <- mass(a0 = params_list$alpha0, a1 = params_list$alpha1,L = Lm)

#fecundity
eggs_at_length <- egg_count(Beta0 = params_list$beta0, Beta1 = params_list$beta1, L = Lf) 
