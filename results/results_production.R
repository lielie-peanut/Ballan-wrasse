#script to automate the population dynamics simulations 
#with different fishing size limits and fishing mortality rates

#load the simulation script
source("R/simulation.R")
#load the size limits and fishing mortality rates to test on our model
size_limits <- read.csv("data/size_limits.csv", stringsAsFactors = F,sep = ";")
fishing_mortality_rates <- seq(from = 0.05, to = 1, by = 0.05)

#creating arrays to store data
size_array <- array(data = NA, dim = c(501, 20, length(size_limits$Lmin)),dimnames = list(years = c(0:500),mortality_rate = fishing_mortality_rates, size_lim = size_limits$location))
catch_array <- array(data = NA, dim = c(501, 20, length(size_limits$Lmin)),dimnames = list(years = c(0:500),mortality_rate = fishing_mortality_rates, size_lim = size_limits$location))
yield_array <- array(data = NA, dim = c(501, 20, length(size_limits$Lmin)),dimnames = list(years = c(0:500),mortality_rate = fishing_mortality_rates, size_lim = size_limits$location))

#run the simulation and fill the arrays according to what was done
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

###plots###

#population size by mortality rate under different size limits
pop_size <- as.data.frame(
  mortality_rate = fishing_mortality_rates,
  size_array[501,,]
)
pop_size$mortality_rate <- as.numeric(row.names(pop_size))

ggplot(data = pop_size, aes(x = mortality_rate)) +
  geom_line(aes(y = `England Devon and Severn IFCA`, colour = "England Devon and Severn IFCA")) +
  geom_line(aes(y = Norway, colour = "Norway")) +
  geom_line(aes(y = Scotland, colour = "Scotland")) +
  geom_line(aes(y = Sweden, colour = "Sweden")) +
  geom_line(aes(y = theoretical, colour = "theoretical"))

#catch by mortality rate
catch_plot <- as.data.frame(
  mortality_rate = fishing_mortality_rates,
  catch_array[501,,]
)
catch_plot$mortality_rate <- as.numeric(row.names(pop_size))

ggplot(data = catch_plot, aes(x = mortality_rate)) +
  geom_line(aes(y = `England Devon and Severn IFCA`, colour = "England Devon and Severn IFCA")) +
  geom_line(aes(y = Norway, colour = "Norway")) +
  geom_line(aes(y = Scotland, colour = "Scotland")) +
  geom_line(aes(y = Sweden, colour = "Sweden")) +
  geom_line(aes(y = theoretical, colour = "theoretical"))

#yield by mortality rate
yield_plot <- as.data.frame(
  mortality_rate = fishing_mortality_rates,
  yield_array[501,,]
)
yield_plot$mortality_rate <- as.numeric(row.names(pop_size))

ggplot(data = yield_plot, aes(x = mortality_rate)) +
  geom_line(aes(y = `England Devon and Severn IFCA`, colour = "England Devon and Severn IFCA")) +
  geom_line(aes(y = Norway, colour = "Norway")) +
  geom_line(aes(y = Scotland, colour = "Scotland")) +
  geom_line(aes(y = Sweden, colour = "Sweden")) +
  geom_line(aes(y = theoretical, colour = "theoretical"))
