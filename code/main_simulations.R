library(pacman)

p_load(tidyverse, magrittr)

source("code/functions/sim_ineq.R")
set.seed(42)

# simulations: sensitivity analysis
iter <- 1000

# vary Sample size
sim_template <- tibble(samp_size = c(2000, 4000, 8000, 16000, 24000, 32000)) 

sim_sample1 <- sim_template %>% 
  mutate(n_groups = 4,
         simu = map(samp_size, ~sim_ineq(iterations = iter, sample_size=.x, n_groups = 2, group_dist = NULL)), 
         mean = map_dbl(simu, "mean"), 
         lower = map_dbl(simu, "lower"),
         upper = map_dbl(simu, "upper"))

sim_sample2 <- sim_template %>%
  mutate(n_groups = 8,
         simu = map(samp_size, ~sim_ineq(iterations = iter, sample_size=.x, n_groups = 4, group_dist = NULL)), 
         mean = map_dbl(simu, "mean"), 
         lower = map_dbl(simu, "lower"),
         upper = map_dbl(simu, "upper"))
sim_sample3 <- sim_template %>%
  mutate(n_groups = 16,
         simu = map(samp_size, ~sim_ineq(iterations = iter, sample_size=.x, n_groups = 8, group_dist = NULL)), 
         mean = map_dbl(simu, "mean"), 
         lower = map_dbl(simu, "lower"),
         upper = map_dbl(simu, "upper"))
sim_sample4 <- sim_template %>%
  mutate(n_groups = 24,simu = map(samp_size, ~sim_ineq(iterations = iter, sample_size=.x, n_groups = 16, group_dist = NULL)), 
         mean = map_dbl(simu, "mean"), 
         lower = map_dbl(simu, "lower"),
         upper = map_dbl(simu, "upper"))
sim_sample5 <- sim_template %>%
  mutate(n_groups = 32,
         simu = map(samp_size, ~sim_ineq(iterations = iter, sample_size=.x, n_groups = 32, group_dist = NULL)), 
         mean = map_dbl(simu, "mean"), 
         lower = map_dbl(simu, "lower"),
         upper = map_dbl(simu, "upper"))

sim_sample <- bind_rows(sim_sample1, sim_sample2, sim_sample3, sim_sample4, sim_sample5) 

sim_sample_short<- sim_sample %>% 
  select(-simu) %>% 
  mutate(simulation = "sample size")

# vary distribution of groups
even_dist <- function(n_samp, n_group) c(rep(n_samp/n_group, n_group))
unequal_dist <- function(n_samp, n_group, frac) c(n_samp*frac, rep(n_samp*(1-frac)/n_group-1, n_group-1))

n_groups=16

#fraction of smallest group
frac_small = 0.01
frac_large = 0.8

sample_size.1 = 2000

even.1 <- sim_ineq(iterations = iter, sample_size = sample_size.1, n_groups = n_groups, group_dist=even_dist(sample_size.1, n_groups))
onesmall.1 <- sim_ineq(iterations = iter, sample_size = sample_size.1, n_groups = n_groups, group_dist=unequal_dist(sample_size.1, n_groups, frac_small))
onelarge.1 <- sim_ineq(iterations = iter, sample_size = sample_size.1, n_groups = n_groups, group_dist=unequal_dist(sample_size.1, n_groups, frac_large))

sample_size.2 = 4000
even.2 <- sim_ineq(iterations = iter, sample_size = sample_size.2, n_groups = n_groups, group_dist=even_dist(sample_size.2, n_groups))
onesmall.2 <- sim_ineq(iterations = iter, sample_size = sample_size.2, n_groups = n_groups, group_dist=unequal_dist(sample_size.2, n_groups, frac_small))
onelarge.2 <- sim_ineq(iterations = iter, sample_size = sample_size.2, n_groups = n_groups, group_dist=unequal_dist(sample_size.2, n_groups, frac_large))

sample_size.3 = 8000
even.3 <- sim_ineq(iterations = iter, sample_size = sample_size.3, n_groups = n_groups, group_dist=even_dist(sample_size.3, n_groups))
onesmall.3 <- sim_ineq(iterations = iter, sample_size = sample_size.3, n_groups = n_groups, group_dist=unequal_dist(sample_size.3, n_groups, frac_small))
onelarge.3 <- sim_ineq(iterations = iter, sample_size = sample_size.3, n_groups = n_groups, group_dist=unequal_dist(sample_size.3, n_groups, frac_large))

sample_size.4 = 16000
even.4 <- sim_ineq(iterations = iter, sample_size = sample_size.4, n_groups = n_groups, group_dist=even_dist(sample_size.4, n_groups))
onesmall.4 <- sim_ineq(iterations = iter, sample_size = sample_size.4, n_groups = n_groups, group_dist=unequal_dist(sample_size.4, n_groups, frac_small))
onelarge.4 <- sim_ineq(iterations = iter, sample_size = sample_size.4, n_groups = n_groups, group_dist=unequal_dist(sample_size.4, n_groups, frac_large))

sample_size.5 = 32000
even.5 <- sim_ineq(iterations = iter, sample_size = sample_size.5, n_groups = n_groups, group_dist=even_dist(sample_size.5, n_groups))
onesmall.5 <- sim_ineq(iterations = iter, sample_size = sample_size.5, n_groups = n_groups, group_dist=unequal_dist(sample_size.5, n_groups, frac_small))
onelarge.5 <- sim_ineq(iterations = iter, sample_size = sample_size.5, n_groups = n_groups, group_dist=unequal_dist(sample_size.5, n_groups, frac_large))

even1<- tibble(unlist(even.1)) %>%
  rename("value"="unlist(even.1)") %>% 
  mutate(name = c("mean", "lower", "upper")) %>% 
  pivot_wider(names_from = name) %>% 
  mutate(samp_size=sample_size.1, simulation="even")
even2<- tibble(unlist(even.2)) %>%
  rename("value"="unlist(even.2)") %>% 
  mutate(name = c("mean", "lower", "upper")) %>% 
  pivot_wider(names_from = name) %>% 
  mutate(samp_size=sample_size.2, simulation="even")  
even3<- tibble(unlist(even.3)) %>%
  rename("value"="unlist(even.3)") %>% 
  mutate(name = c("mean", "lower", "upper")) %>% 
  pivot_wider(names_from = name) %>% 
  mutate(samp_size=sample_size.3, simulation="even") 
even4<- tibble(unlist(even.4)) %>%
  rename("value"="unlist(even.4)") %>% 
  mutate(name = c("mean", "lower", "upper")) %>% 
  pivot_wider(names_from = name) %>% 
  mutate(samp_size=sample_size.4, simulation="even") 
even5<- tibble(unlist(even.5)) %>%
  rename("value"="unlist(even.5)") %>% 
  mutate(name = c("mean", "lower", "upper")) %>% 
  pivot_wider(names_from = name) %>% 
  mutate(samp_size=sample_size.5, simulation="even") 

even <- bind_rows(even1, even2, even3, even4, even5)

onesmall1<- tibble(unlist(onesmall.1)) %>%
  rename("value"="unlist(onesmall.1)") %>% 
  mutate(name = c("mean", "lower", "upper")) %>% 
  pivot_wider(names_from = name) %>% 
  mutate(samp_size=sample_size.1, simulation="onesmall")
onesmall2<- tibble(unlist(onesmall.2)) %>%
  rename("value"="unlist(onesmall.2)") %>% 
  mutate(name = c("mean", "lower", "upper")) %>% 
  pivot_wider(names_from = name) %>% 
  mutate(samp_size=sample_size.2, simulation="onesmall")  
onesmall3<- tibble(unlist(onesmall.3)) %>%
  rename("value"="unlist(onesmall.3)") %>% 
  mutate(name = c("mean", "lower", "upper")) %>% 
  pivot_wider(names_from = name) %>% 
  mutate(samp_size=sample_size.3, simulation="onesmall") 
onesmall4<- tibble(unlist(onesmall.4)) %>%
  rename("value"="unlist(onesmall.4)") %>% 
  mutate(name = c("mean", "lower", "upper")) %>% 
  pivot_wider(names_from = name) %>% 
  mutate(samp_size=sample_size.4, simulation="onesmall") 
onesmall5<- tibble(unlist(onesmall.5)) %>%
  rename("value"="unlist(onesmall.5)") %>% 
  mutate(name = c("mean", "lower", "upper")) %>% 
  pivot_wider(names_from = name) %>% 
  mutate(samp_size=sample_size.5, simulation="onesmall") 

onesmall <- bind_rows(onesmall1, onesmall2, onesmall3, onesmall4, onesmall5)

onelarge1<- tibble(unlist(onelarge.1)) %>%
  rename("value"="unlist(onelarge.1)") %>% 
  mutate(name = c("mean", "lower", "upper")) %>% 
  pivot_wider(names_from = name) %>% 
  mutate(samp_size=sample_size.1, simulation="onelarge")
onelarge2<- tibble(unlist(onelarge.2)) %>%
  rename("value"="unlist(onelarge.2)") %>% 
  mutate(name = c("mean", "lower", "upper")) %>% 
  pivot_wider(names_from = name) %>% 
  mutate(samp_size=sample_size.2, simulation="onelarge")  
onelarge3<- tibble(unlist(onelarge.3)) %>%
  rename("value"="unlist(onelarge.3)") %>% 
  mutate(name = c("mean", "lower", "upper")) %>% 
  pivot_wider(names_from = name) %>% 
  mutate(samp_size=sample_size.3, simulation="onelarge") 
onelarge4<- tibble(unlist(onelarge.4)) %>%
  rename("value"="unlist(onelarge.4)") %>% 
  mutate(name = c("mean", "lower", "upper")) %>% 
  pivot_wider(names_from = name) %>% 
  mutate(samp_size=sample_size.4, simulation="onelarge") 
onelarge5<- tibble(unlist(onelarge.5)) %>%
  rename("value"="unlist(onelarge.5)") %>% 
  mutate(name = c("mean", "lower", "upper")) %>% 
  pivot_wider(names_from = name) %>% 
  mutate(samp_size=sample_size.5, simulation="onelarge") 

onelarge <- bind_rows(onelarge1, onelarge2, onelarge3, onelarge4, onelarge5)

distribution <- bind_rows(even, onelarge, onesmall) %>% 
  mutate(n_groups = n_groups)

sim_all <- bind_rows(sim_sample_short, distribution)

save(sim_all, file = "processed-data/sim_all.Rda")

