library(pacman)
p_load(tidyverse, magrittr, knitr, kableExtra, gtsummary, wesanderson, 
       GGally, ggrepel, ggpubr, modelsummary, stringr, tidytext, patchwork)

ggplot2::theme_set(ggplot2::theme_bw()) 

# load custom functions
source("code/functions/functions.R")

# load or run merging of dhs data
if (!file.exists("processed-data/merged.Rda")) {
  source("code/main_merge.R")
}
# load or run calculations to aggregate form
if (file.exists("processed-data/cohort.Rda") & 
    file.exists("processed-data/pool.Rda")) {
  load("processed-data/cohort.Rda")
  load("processed-data/pool.Rda")
  load("processed-data/dhs_main.Rda")
} else {
  source("code/main_calc.R")
}
#run analyses for table 1 and merge with main cohort data
source("code/main_analysis.R")

# Plot inequality ratio

plot_ir <- cohort %>%
  filter(country_long %in% c("Brazil", "Kenya", "Afghanistan", "Ghana", "Malawi", "Pakistan", 
                             "South Africa", "United States", "DR Congo", "Sierra Leone", "Uganda", "Zimbabwe"
                            )) %>% 
  arrange(country_long, desc(cohort_cat)) %>% 
  group_by(country_long) %>% 
  slice_head(n=1) %>% 
  ungroup() %>% 
  ggplot(aes(x=fct_reorder(country_long, r_gen.eth, .desc = TRUE))) +
  geom_point(aes(y=r_gen, colour = "Gender"), size=4, shape = 16) +
  geom_point(aes(y=r_eth, colour = "Ethnicity"), size=4, shape = 17,) +
  geom_point(aes(y=r_gen.eth, colour = "Intersectional"), size=4, shape = 18) +
  scale_color_manual(values = c("Gender" = "#faecd3", "Ethnicity" = "#FFD700", "Intersectional" = "#FF8C00")) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    text = element_text(color = "white", size = 20),
    plot.background = element_rect(fill = "#383838"),
    panel.background = element_rect(fill = "#383838"),
    panel.grid.major = element_line(color = "#808080"),
    panel.grid.minor = element_line(color = "#808080"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    legend.background = element_rect(fill = "#383838"),
    legend.text = element_text(color = "white"),
    legend.position = "bottom"
  ) + 
  labs(x="", y="Inequality ratio", colour = "Grouping") +
  coord_flip()

ggsave(file="latex/figures/fig_ir_defense.pdf",  plot = plot_ir, width = 12, height = 7)

# differential inequality plot
plot_diff <- cohort %>% 
  filter(country_long %in% c("Brazil", "Afghanistan", "Kenya", "Malawi", "Pakistan", "Ghana",
                             "South Africa", "United States", "DR Congo", "Sierra Leone", "Uganda", "Zimbabwe"
  )) %>% 
  arrange(country_long, desc(cohort_cat)) %>% 
  group_by(country_long) %>% 
  slice_head(n=1) %>% 
  ungroup() %>% 
  filter(!is.na(diff_mech)) %>% 
  mutate(diff_mech = diff_mech*100) %>% 
  select(country_long, diff_mech, r_gen.eth) %>% 
  ggplot(aes(x=fct_reorder(country_long, diff_mech), y= diff_mech, colour = r_gen.eth)) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha=0.5) + 
  geom_point(size = 4) +
  geom_text(data=. %>% filter(
    diff_mech>3 | (diff_mech<0 & diff_mech>-5)), # Filter data first
    aes(label=country_long, y=diff_mech),
    nudge_y = -1.75,
    size=5) +
  geom_text(data=. %>% filter(
    (diff_mech<3 & diff_mech>0) | diff_mech < -5), # Filter data first
    aes(label=country_long, y=diff_mech),
    nudge_y = 1.75,
    size=5) +
  coord_flip() +
  scale_colour_viridis_b(option = "inferno", direction = -1, begin = 0.5, end =1)+
  labs(colour = "Inequality ratio", x="", y="Differential intersectionality") +
  theme_light() +
  theme(
    text = element_text(color = "white", size = 20),
    plot.background = element_rect(fill = "#383838"),
    panel.background = element_rect(fill = "#383838"),
    panel.grid.major = element_line(color = "#808080"),
    panel.grid.minor = element_line(color = "#808080"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    legend.background = element_rect(fill = "#383838"),
    legend.text = element_text(color = "white"),
    axis.text.y = element_blank(),
    legend.position = "bottom"
  )  

ggsave("latex/figures/plot_di_defense.pdf", plot = plot_diff, width = 7, height = 7)
