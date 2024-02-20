# Package names
packages <- c("truncnorm")

# Install packages that are not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))
rm(installed_packages, packages)  

sim_ineq <- function(iterations, sample_size, n_groups, group_dist) {

#initialize data
  if (is.null(group_dist)) prob= rep(1/n_groups, n_groups) else {
    prob <- tibble(n=group_dist) %>%  
      mutate(pct=n/sum(n)) %>% 
      pull(pct) 
  }
  groups <- rep(NA, sample_size)
  educ_years <- rep(NA, sample_size)
  samp <- data.frame(groups, educ_years)  
  simulation <- rep(NA, iterations)
  
  for(i in 1:iterations) {
    x <- samp %>% 
      mutate(groups=sample(c(1:n_groups), size = sample_size, replace = T, prob = prob),
             educ_years = floor(rtruncnorm(n = sample_size, a = 0, b=17, mean = 5.5, sd = 3))) #rtruncnorm: normal distribution truncated at zero
    
    simulation[i] <- x %>% 
      group_by(groups) %>% 
      summarise(means=mean(educ_years)) %>% 
      summarise(min_group = min(means), 
                max_group = max(means)) %>% 
      mutate(value = min_group/max_group) %>% 
      pull(value)
  }
  sim <- tibble("simulation"=simulation)
  l.model <- lm(simulation ~ 1, sim)
  conf<- confint(l.model)
  return(list("mean" = l.model[["coefficients"]][1],
         "lower" = conf[1],
         "upper" = conf[2]
         ))
}
  
