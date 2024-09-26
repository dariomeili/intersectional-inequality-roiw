# Simulation Script ----

## Preamble:  ----
# Load custom simulation functions
source("code/functions/sim_ineq.R")

# Set seed for reproducibility
set.seed(42)

# Sensitivity Analysis: Vary Sample Size ----

## Define Simulation Parameters ----
iter <- 1000  # Number of iterations for simulations
sim_template <- tibble(samp_size = c(2000, 4000, 8000, 16000, 24000, 32000))  # Define sample sizes

## Run Simulations for Different Group Sizes ----

# Simulation for 4 groups
sim_sample1 <- sim_template %>% 
  mutate(n_groups = 4,
         simu = map(samp_size, ~sim_ineq(iterations = iter, sample_size = .x, n_groups = 2, group_dist = NULL)), 
         mean = map_dbl(simu, "mean"), 
         lower = map_dbl(simu, "lower"),
         upper = map_dbl(simu, "upper"))

# Simulation for 8 groups
sim_sample2 <- sim_template %>%
  mutate(n_groups = 8,
         simu = map(samp_size, ~sim_ineq(iterations = iter, sample_size = .x, n_groups = 4, group_dist = NULL)), 
         mean = map_dbl(simu, "mean"), 
         lower = map_dbl(simu, "lower"),
         upper = map_dbl(simu, "upper"))

# Simulation for 16 groups
sim_sample3 <- sim_template %>%
  mutate(n_groups = 16,
         simu = map(samp_size, ~sim_ineq(iterations = iter, sample_size = .x, n_groups = 8, group_dist = NULL)), 
         mean = map_dbl(simu, "mean"), 
         lower = map_dbl(simu, "lower"),
         upper = map_dbl(simu, "upper"))

# Simulation for 24 groups
sim_sample4 <- sim_template %>%
  mutate(n_groups = 24,
         simu = map(samp_size, ~sim_ineq(iterations = iter, sample_size = .x, n_groups = 16, group_dist = NULL)), 
         mean = map_dbl(simu, "mean"), 
         lower = map_dbl(simu, "lower"),
         upper = map_dbl(simu, "upper"))

# Simulation for 32 groups
sim_sample5 <- sim_template %>%
  mutate(n_groups = 32,
         simu = map(samp_size, ~sim_ineq(iterations = iter, sample_size = .x, n_groups = 32, group_dist = NULL)), 
         mean = map_dbl(simu, "mean"), 
         lower = map_dbl(simu, "lower"),
         upper = map_dbl(simu, "upper"))

## Combine Sample Size Simulations ----
sim_sample <- bind_rows(sim_sample1, sim_sample2, sim_sample3, sim_sample4, sim_sample5) 

# Create a short version without the simulation results column
sim_all <- sim_sample %>% 
  select(-simu) 

# Save Simulation Results ----
save(sim_all, file = "processed-data/sim_all.Rda")


