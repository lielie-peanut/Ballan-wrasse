###simulation

#loading packages
library(ggplot2)

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

years_array <- array(data = NA, dim = c(751, 31, 3),dimnames = list(years = c(0:750),ages, matrices = c(1:3)))

years_array[1,,1] <- year_countF
years_array[1,,2] <- year_countM
#remember that the fish age 0 is column 1 and year 0 is row 1!

###loop###

for (i in 1:750){
  
  ###sex change and mortality###
  
  #start with sex change: first step is to calculate average length
  avg_length <- (sum(year_countF*Lf)+sum(year_countM*Lm))/sum(year_countF+year_countM)
  
  #calculation of the previous year's sex change probability per age group
  year_sex_change <- sex_change(Lf,Li = avg_length,deltaL = params_list$deltaLC,b = params_list$b)
  year_sex_change[1:5] <- 0
  
  #calculation of this years' numbers
  year_countF <- N_F(Nfemales = year_countF,Pchange = year_sex_change,Lfemales = Lf,Z = M)
  year_countM <- N_M(Nfemales = year_countF,Nmales = year_countM,Pchange = year_sex_change,Lfemales = Lf,Z = M)
  
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
  
  #fertilization rate
  year_fert_rate <- fert_rate(k = params_list$K, xF = year_ratio)
  
  #number of fertilized eggs
  year_fert_eggs <- sum(year_countF*eggs_at_length)*year_fert_rate
  
  #fixing a "fertilized egg count per female" value
  if (i<=250){
    egg_per_female <- year_fert_eggs/sum(year_countF)
  }
  
  #finally calculate recruitment
  year_countF[1] <- spawn_recruitment(h = params_list$h, R0 = params_list$R0, sigma = year_fert_eggs, teta0 = egg_per_female)
  
  #introduction of this year count in the matrices
  years_array[i+1,,1] <- year_countF
  years_array[i+1,,2] <- year_countM
}

###figures###

#males vs females graph
males_vs_females <- data.frame(
  age = ages,
  females = years_array[251,,1],
  males = years_array[251,,2]
)

ggplot(males_vs_females, aes(age))+
  geom_line(aes(y=females, colour = "females"))+
  geom_line(aes(y=males, colour="males"))
  
  