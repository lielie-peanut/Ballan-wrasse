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
massM <- list(mass(a0 = params_list$alpha0, a1 = params_list$alpha1,L = Lm))

#fecundity
eggs_at_length <- c(egg_count(Beta0 = params_list$beta0, Beta1 = params_list$beta1, L = Lf)) 
eggs_at_length[1:5] <- 0
#we defined that reproduction starts at 5, so the egg count before that is 0

#recruitment to fishing
fishingF <- list(fishing_recruitment(Lf,Lmin = 120, Lmax = 240, mu = params_list$mu, Fm = 0.3))
fishingM <- list(fishing_recruitment(Lm,Lmin = 120, Lmax = 240, mu = params_list$mu, Fm = 0.3))

#start settings
year_countF <- c(params_list$N0, rep(0,30))
year_countM <- c(rep(0,31))
#mortality rate
M <- 0.2
#male depletion ratio
year_ratio <- 1

#simulation matrices
years_matrix_F <- matrix(data= NA, nrow= 751, ncol=31, byrow= TRUE, dimnames = list(years=0:750, age=0:30))
years_matrix_M <- matrix(data= NA, nrow= 751, ncol=31, byrow= TRUE, dimnames = list(years=0:750, age=0:30))

years_matrix_F[1,] <- year_countF
years_matrix_M[1,] <- year_countM
#remember that the fish age 0 is column 1 and year 0 is row 1!

###loop###

for (i in 1:750){
  
  ###sex change and mortality###
  
  #start with sex change: first step is to calculate average length
  avg_length <- sum(year_countF)*Lf/sum(year_countF)
  
  #calculation of the previous year's sex change probability per age group
  year_sex_change <- sex_change(Lf,Li = avg_length,deltaL = params_list$deltaLC,b = params_list$b)
  
  #calculation of this years' numbers
  year_countF <- N_F(Nfemales = year_countF,Pchange = year_sex_change,Lfemales = Lf,Z = M)
  year_countM <- N_M(Nfemales = year_countM,Nmales = unlist(year_countM),Pchange = year_sex_change,Lfemales = Lf,Z = M)
  
  #modify the counts to account for aging
  year_countF <- c(0, year_countF[-length(year_countF)])  # shift right, drop oldest
  year_countM <- c(0, year_countM[-length(year_countM)])
  
  ###fecundity###
  
  #first step is to calculate the proportion of males (applied only once the 
  #population is stable, and when there could be a male depletion, 
  #so once fishing is introduced)
  
  #fixing a "stable sex ratio" value
  if (i==250){
    ref_sex_ratio <- sum(year_countM)/(sum(year_countF)+sum(year_countM))
  }
  #calculating the male depletion compared to a stable population
  if (i>=250){
    year_ratio <- male_ratio(Pt = sum(year_countM)/(sum(year_countF)+sum(year_countM)), Pz = ref_sex_ratio)
  }
  
}


  
  