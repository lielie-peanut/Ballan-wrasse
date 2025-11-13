#script to automate the population dynamics simulations 
#with different fishing size limits and fishing mortality rates

#load the simulation script
source("R/simulation.R")
#load the size limits and fishing mortality rates to test on our model
size_limits <- read.csv("data/size_limits.csv", stringsAsFactors = F,sep = ";")
fishing_mortality_rates <- seq(from = 0.05, to = 1, by = 0.05)


size_array <- array(data = NA, dim = c(501, 20, length(size_limits$Lmin)),dimnames = list(years = c(0:500),mortality_rate = fishing_mortality_rates, size_lim = size_limits$location))
catch_array <- array(data = NA, dim = c(501, 20, length(size_limits$Lmin)),dimnames = list(years = c(0:500),mortality_rate = fishing_mortality_rates, size_lim = size_limits$location))
yield_array <- array(data = NA, dim = c(501, 20, length(size_limits$Lmin)),dimnames = list(years = c(0:500),mortality_rate = fishing_mortality_rates, size_lim = size_limits$location))

for (i in 1:length(size_limits$Lmin)){
  count <- 1
  for (e in fishing_mortality_rates){
    simulation <- simulate_population(size_limits$Lmin[i],size_limits$Lmax[i],e)
    size_array[,count,i] <- simulation[[1]]
    catch_array[,count,i] <- simulation[[2]]
    yield_array[,count,i] <- simulation[[3]]
    count <- count+1
  }
}