# Preamble
library(pacman)
p_load(tidyverse, ggplot2, truncnoorm)

## parameters
pop <- 1000 # "population" size
groups <- 50 # number of ethnic groups
reps <- 10 # number of simulations per number of groups

## generate empty population data
gender <- rep(NA, pop)
ethnicity <- rep(NA, pop)
educ_years <- rep(NA, pop)
sample <- data.frame(gender, ethnicity, educ_years)


## function to generate random population
pop_gen <- function(sample, groups) {
  x <- sample %>% 
  mutate(ethnicity=floor(runif(pop, 1, groups+1)),
         gender=floor(runif(pop, 1, 3)),
         educ_years = rtruncnorm(n = pop, a = 0, mean = 6, sd = 3.3)) #rtrauncnorm: normal distribution truncated at zero
}

## inequality ratio function
inra_function <- function(x) {
  x <- x %>% group_by(ethnicity, gender) %>% 
    summarise(educ_years = mean(educ_years))
  x1 <- min(x$educ_years) / max(x$educ_years)
}

## generate empty simulation data (inra = inequality ratio)
num_groups <- c(2:groups)
inra <- rep(NA, groups-1)
dat <- data.frame(num_groups, inra)

## run simulation for different numbers of ethnic groups
for (g in 2:groups) {
  dat[g-1,2] <- mean(replicate(reps, inra_function(pop_gen(sample, g))))
}


## plot simulated data
ggplot(dat, aes(x = num_groups, y=inra)) + 
  geom_line() +
  geom_point()


