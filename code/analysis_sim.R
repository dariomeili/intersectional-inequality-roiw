# Produce Outputs from Simulations ----

## Plot Sensitivity Analysis: Varying Number of Groups ----

sim_all %>% 
  mutate(n_groups = as_factor(n_groups)) %>% 
  ggplot(aes(x = samp_size, y = 1 - mean, color = n_groups, group = n_groups, shape = n_groups)) +
  geom_point() +
  geom_line() + 
  scale_colour_viridis_d(option = "mako", end = 0.85) +
  scale_x_continuous(breaks = c(2000, 10000, 20000, 30000)) + 
  labs(
    x = "Sample Size", 
    y = "Simulated Inequality Ratio", 
    color = "Groups", 
    shape = "Groups"
  ) +
  ylim(0, 1)

ggsave("latex/figures/sensitivity-groups.pdf", width = 7, height = 5)
