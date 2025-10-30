###simulation

#loading functions from another file
source("R/mortality_functions.R")
source("R/growth_functions.R")
source("R/recruitment_functions.R")

#loading data
parameters <- read.csv("data/parameters.csv", stringsAsFactors = F,sep = ";")
params_list <- as.list(setNames(parameters$value,parameters$symbol))
print(params_list)

#example run 
ages <- 0:30
Lf <- VB(ages,Linf = params_list$LinfF,k = params_list$KF,t0 = params_list$t0F)

plot(ages, Lf, type = "l", xlab = "Age", ylab = "Length (mm)")
