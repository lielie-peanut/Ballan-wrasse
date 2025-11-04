###simulation

#loading functions from another file
source("R/mortality_functions.R")
source("R/growth_functions.R")
source("R/recruitment_functions.R")

#loading data
parameters <- read.csv("data/parameters.csv", stringsAsFactors = F,sep = ";")
params_list <- as.list(setNames(parameters$value,parameters$symbol))
print(params_list)

#calculating the stable parameters

#growth 
ages <- 0:30
Lf <- VB(ages,Linf = params_list$LinfF,k = params_list$KF,t0 = params_list$t0F)
Lm <- VB(ages,Linf = params_list$LinfM,k = params_list$KM,t0 = params_list$t0M)

#mass
massF <- mass(a0 = params_list$alpha0, a1 = params_list$alpha1,L = Lf)
massM <- mass(a0 = params_list$alpha0, a1 = params_list$alpha1,L = Lm)

#fecundity
eggs_at_length <- egg_count(Beta0 = params_list$beta0, Beta1 = params_list$beta1, L = Lf) 

#start settings
year_countF <- list(age0 = params_list$N0, age1=0, age2=0, age3=0, age4=0, age5=0, age6=0, age7=0,age8=0, age9=0, age10=0, age11=0, age12=0, age13=0, age14=0, age15=0, age16=0, age17=0, age18=0, age19=0, age20=0, age21=0, age22=0, age23=0, age24=0, age25=0, age26=0, age27=0, age28=0, age29=0, age30=0)
year_countM <- list(age0 = 0, age1=0, age2=0, age3=0, age4=0, age5=0, age6=0, age7=0,age8=0, age9=0, age10=0, age11=0, age12=0, age13=0, age14=0, age15=0, age16=0, age17=0, age18=0, age19=0, age20=0, age21=0, age22=0, age23=0, age24=0, age25=0, age26=0, age27=0, age28=0, age29=0, age30=0)

#simulation matrices
years_matrix_F <- matrix(data= NA, nrow= 751, ncol=31, byrow= TRUE, dimnames = list(years=0:750, age=0:30))
years_matrix_M <- matrix(data= NA, nrow= 751, ncol=31, byrow= TRUE, dimnames = list(years=0:750, age=0:30))

years_matrix_F[1,] <- unlist(year_countF)
years_matrix_M[1,] <- unlist(year_countM)
#remember that the fish age 0 is column 1 and year 0 is row 1!

###loop###

for (i in 1:750){
  
}

