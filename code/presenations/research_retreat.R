library(tidyverse)
library(magrittr)
library(ggthemes)
library(wesanderson)
library(ggrepel)
library(modelsummary)

load("data/cohort.Rda")
load("data/pool.Rda")

means <- pool %>% 
  select(country_long, mean_educ, l_gen_mean, h_gen_mean, l_eth_mean, h_eth_mean, l_gen.eth_mean, h_gen.eth_mean) %>% 
  filter(!is.na(l_gen.eth_mean)) %>% 
  pivot_longer(cols=c("mean_educ","l_gen_mean", "h_gen_mean", "l_eth_mean", "h_eth_mean", "l_gen.eth_mean", "h_gen.eth_mean")) %>% 
  mutate(grouping = fct_collapse(name, 
                                 "gender" = c("l_gen_mean", "h_gen_mean"),
                                 "ethnicity" = c("l_eth_mean", "h_eth_mean"),
                                 "gen:eth" = c("l_gen.eth_mean", "h_gen.eth_mean"), 
                                 "overall mean" = c("mean_educ")
  )) 
# everything
means %>% 
  ggplot(aes(x=value, y=fct_reorder(country_long, value))) +
    geom_point(aes(colour = grouping, shape = grouping, fill=grouping), alpha=0.8) + 
    scale_colour_manual(values= c(wes_palette("Darjeeling1", 3),"#17202A")) +
    scale_fill_manual(values = c(wes_palette("Darjeeling1", 3),"#17202A")) +
    scale_shape_manual(values = c(21, 21, 21, 23)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          panel.grid.major.y = element_line(colour = "lightgrey"),
          axis.line = element_blank(), 
          axis.text.y = element_text(size = 6),
          legend.position = "bottom") +
    labs(x="Years of schooling", y="Country") +
    xlim(0, 13.2) + 
  ggsave("figures/countries_all.png", width = 6, height = 4)
#only means
means %>% 
  filter(grouping == "overall mean") %>% 
  ggplot(aes(x=value, y=fct_reorder(country_long, value))) +
  geom_point(aes(colour = grouping, shape = grouping, fill=grouping), alpha=0.8) + 
  scale_colour_manual(values= c("#17202A")) +
  scale_fill_manual(values = c("#17202A")) +
  scale_shape_manual(values = c(23)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "lightgrey"),
        axis.line = element_blank(), 
        axis.text.y = element_text(size = 6),
        legend.position = "bottom") +
  labs(x="Years of schooling", y="Country") +
  xlim(0, 13.2) +
  ggsave("figures/countries_overall.png", width = 6, height = 4)

# gender
means %>% 
  filter(grouping %in% c("overall mean", "gender")) %>% 
  ggplot(aes(x=value, y=fct_reorder(country_long, value))) +
  geom_point(aes(colour = grouping, shape = grouping, fill=grouping), alpha=0.8) + 
  scale_colour_manual(values= c(wes_palette("Darjeeling1", 3)[2],"#17202A")) +
  scale_fill_manual(values = c(wes_palette("Darjeeling1", 3)[2],"#17202A")) +
  scale_shape_manual(values = c(21, 23)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "lightgrey"),
        axis.line = element_blank(), 
        axis.text.y = element_text(size = 6),
        legend.position = "bottom") +
  labs(x="Years of schooling", y="Country") +
  xlim(0, 13.2) +
  ggsave("figures/countries_gender.png", width = 6, height = 4)
  
# ethnicity
means %>% 
  filter(grouping %in% c("overall mean", "gender", "ethnicity")) %>% 
  ggplot(aes(x=value, y=fct_reorder(country_long, value))) +
  geom_point(aes(colour = grouping, shape = grouping, fill=grouping), alpha=0.8) + 
  scale_colour_manual(values= c(wes_palette("Darjeeling1", 3)[1:2],"#17202A")) +
  scale_fill_manual(values = c(wes_palette("Darjeeling1", 3)[1:2],"#17202A")) +
  scale_shape_manual(values = c(21, 21, 23)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        panel.grid.major.y = element_line(colour = "lightgrey"),
        axis.line = element_blank(), 
        axis.text.y = element_text(size = 6),
        legend.position = "bottom") +
  labs(x="Years of schooling", y="Country") +
  xlim(0, 13.2) +
  ggsave("figures/countries_ethnicity.png", width = 6, height = 4)

# example for brazil
brazil <- pool %>% 
  filter(country=="brazil")
br_gen <- brazil$res_gen
br_eth <- brazil$res_eth
br_gen.eth <- brazil$res_gen.eth

br_means <- tibble("value" = c(br_gen[[1]]$overall_mean, br_gen[[1]]$group_means, br_eth[[1]]$group_means, br_gen.eth[[1]]$group_means),
       "group" = c("overall_mean", names(br_gen[[1]]$group_means), names(br_eth[[1]]$group_means), names(br_gen.eth[[1]]$group_means)),
       "grouping" = c("overall_mean", rep("gender", 2), rep("ethnicity", 3), rep("gen:eth", 6)))

custom_pal <- c("#17202A", "#00A08A", "#FF0000", "#F2AD00")

br_means %>% 
  mutate(grouping = fct_relevel(grouping, "overall_mean", "gender", "ethnicity", "gen:eth")) %>% 
  ggplot(aes(x=value, y=fct_rev(grouping), label=group, color = group)) +
  geom_point(aes(colour=grouping)) +
  geom_vline(xintercept = br_gen[[1]]$overall_mean, linetype = "dotted") +
  geom_text_repel(aes(colour=grouping)) + 
  scale_colour_manual(values=custom_pal)+
  theme(legend.position = "none") + 
  labs(x="years of schooling", y="grouping", title = "Average year of schooling by grouping in Brazil", caption = "data: DHS3 1996; birth cohorts 1937-1978")+
  ggsave("figures/brazil.png", width = 6, height = 4)

cm1 <- c("cohort_cat1985-" ="cohort >1985",
         "cohort_cat1975-1985" = "cohort 1975-1985",
         "n_ethnicity" = "n ethnic groups", 
         "h_gen.eth_share" = "% highest", 
         "l_gen.eth_share" = "% lowest", 
         "cpia_gender" = "gender equity",
         "cpia_social"="social inclusion", 
         "educ_spending"="educ_spending", 
         "mean_educ" = "mean_educ")

modelplot(list("gender"= m4, "ethnicity"= m5, "gen:eth" = m3), coef_map = cm1) +
  geom_vline(xintercept = 0, color = 'orange') +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("#FF0000", "#F2AD00", "#00A08A")) +
  labs(x="Coef (95% CI)")+
  ggsave("figures/reg1.png", width = 6, height = 4) 
  

sp::plot(ne_countries(country = 'brazil'))
# world map
world <- ne_countries(scale = 50, returnclass = "sf")
# define zoom level
zoom_to <- c(25,0)
zoom_level <- 0.55
lon_span <- 360 / 2^zoom_level
lat_span <- 180 / 2^zoom_level
lon_bounds <- c(zoom_to[1] - lon_span / 2, zoom_to[1] + lon_span / 2)
lat_bounds <- c(zoom_to[2] - lat_span / 2, zoom_to[2] + lat_span / 2)

scale_fill_custom <- scale_fill_viridis_b(breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
                                          limits = c(min(pool$rlh_gen.eth, na.rm = T),1))
# plot world

pool %>% 
  select(country, alpha_3, rlh_gen.eth) %>% 
  right_join(world, by=c("alpha_3"="iso_a3")) %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = rlh_gen.eth), size=0.1) +
  scale_fill_custom +
  labs(fill="Ineq. ratio") + 
  coord_sf(xlim = lon_bounds, ylim = lat_bounds) +
  theme_map() + 
  ggsave("figures/world_ratio_geneth.png", height = 7/16*9, width=7)
