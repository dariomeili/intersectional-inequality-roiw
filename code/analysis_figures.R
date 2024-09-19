
##############################################################
# Plot mean education for all countries, overall and by groups
##############################################################
pool %>% 
  select(country_long, educ_w, l_gen_mean, h_gen_mean, l_eth_mean, h_eth_mean, l_gen.eth_mean, h_gen.eth_mean) %>% 
  filter(!is.na(l_gen.eth_mean)) %>% 
  pivot_longer(cols=c("educ_w","l_gen_mean", "h_gen_mean", "l_eth_mean", "h_eth_mean", "l_gen.eth_mean", "h_gen.eth_mean")) %>% 
  mutate(Grouping = fct_collapse(name, 
                                 "Gender" = c("l_gen_mean", "h_gen_mean"),
                                 "Ethnicity" = c("l_eth_mean", "h_eth_mean"),
                                 "Intersectional" = c("l_gen.eth_mean", "h_gen.eth_mean"), 
                                 "Overall mean" = c("educ_w")
  )) %>% 
  ggplot(aes(x=value, y=fct_reorder(country_long, value))) +
  geom_point(aes(colour = Grouping, shape = Grouping, fill=Grouping), alpha=0.8) + 
  scale_color_viridis_d(option = "mako", end=0.85,) +
  scale_fill_viridis_d(option = "mako", end =0.85,) +
  #scale_colour_manual(values= c("#17202A", wes_palette("Darjeeling1", 3))) +
  #scale_fill_manual(values = c("#17202A", wes_palette("Darjeeling1", 3))) +
  scale_shape_manual(values = c(23, 21, 21, 21)) +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.line = element_blank(), 
        axis.text.y = element_text(size=7),
        legend.position = "bottom") +
  labs(x="Education, yrs", y="")

ggsave("latex/figures/mean-educ.pdf", width = 7, height = 7)

####################################################
# plot inequality ratios for all countries by groups
####################################################
plot_1980 <- cohort %>% 
  filter(cohort_cat =="1980-") %>% 
  ggplot(aes(x=fct_reorder(country_long, r_gen.eth))) +
  geom_point(aes(y=r_gen, colour = "Gender"), size=2, shape = 16) +
  geom_point(aes(y=r_eth, colour = "Ethnicity"), size=2, shape = 17,) +
  geom_point(aes(y=r_gen.eth, colour = "Intersectional"), size=2, shape = 18) +
  scale_colour_viridis_d(option = "mako", 
                         begin = 0.2, 
                         end = 0.85,
                         breaks = c("Gender", "Ethnicity", "Intersectional")) +
  #scale_y_reverse() +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.text.y = element_text(size=7),
        legend.position = "bottom") +
  labs(x="", y="", colour = "Grouping") +
  facet_wrap(~cohort_cat, scales = "free_y") +
  annotate("rect", 
           xmin = "South Africa",
            xmax = "Burkina Faso",
            ymin = -Inf,
            ymax = 0,
            alpha = 0.2) +
  annotate("label", x="Moldova", y=0.125, label="Mean Education:", size = 2.75) +
  geom_text(aes(y=0, label=round(gw_educ, digits = 1)), nudge_y = +0.05, size = 2.75) +
  coord_flip() + 
  theme(legend.position="none")

plot_1960 <- cohort %>% 
  filter(cohort_cat !="1980-") %>% 
  ggplot(aes(x=reorder_within(alpha_3, r_gen.eth, cohort_cat))) +
  geom_point(aes(y=r_gen, colour = "Gender"), size=2) +
  geom_point(aes(y=r_eth, colour = "Ethnicity"), size=2) +
  geom_point(aes(y=r_gen.eth, colour = "Intersectional"), size=2) +
  scale_colour_viridis_d(option = "mako", 
                         begin = 0.2, 
                         end = 0.85,
                         breaks = c("Gender", "Ethnicity", "Intersectional")) +
  scale_y_reverse() +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.text.y = element_text(size=7),
        legend.position = "bottom") +
  labs(x="", y="Inequality ratio", colour = "Grouping") +
  facet_wrap(~cohort_cat, scales = "free_y") +
  coord_flip() + 
  scale_x_reordered()  

plot_ratio <- plot_1980 / plot_1960

ggsave("latex/figures/inequality_ratio.pdf", plot=plot_ratio, width = 7, height = 8)

plot_ir <- cohort %>%
  arrange(country_long, desc(cohort_cat)) %>% 
  group_by(country_long) %>% 
  slice_head(n=1) %>% 
  ungroup() %>% 
  mutate(country_long = fct_recode(country_long,
                                   "Bolivia*" = "Bolivia", 
                                   "Brazil*"="Brazil", 
                                   "Central African Rep.°" = "Central African Republic", 
                                   "Ivory Coast°" = "Ivory Coast", 
                                   "Kazakhstan*" = "Kazakhstan", 
                                   "Namibia*" = "Namibia",
                                   "Philippines*" = "Philippines", 
                                   "Rwanda°" = "Rwanda", 
                                   "Zimbabwe°" = "Zimbabwe")) %>% 
  ggplot(aes(x=fct_reorder(country_long, r_gen.eth, .desc = TRUE))) +
  geom_point(aes(y=r_gen, colour = "Gender"), size=2, shape = 16) +
  geom_point(aes(y=r_eth, colour = "Ethnicity"), size=2, shape = 17,) +
  geom_point(aes(y=r_gen.eth, colour = "Intersectional"), size=2, shape = 18) +
  annotate("rect", 
           xmin = "South Africa", 
           xmax = "Afghanistan", 
           ymin = Inf, 
           ymax = 1, 
           alpha = 0.2) +
  annotate("text", x="South Africa", y=1-0.125, label="Mean Education:", size = 3) +
  geom_text(aes(y=1, label=round(gw_educ, digits = 2)), nudge_y = +0.05, size = 3) +
  scale_colour_viridis_d(option = "mako", 
                         begin = 0.2, 
                         end = 0.85,
                         breaks = c("Gender", "Ethnicity", "Intersectional")) +
  #scale_y_reverse() +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.text.y = element_text(size=9),
        legend.position = "bottom") +
  labs(x="", y="Inequality ratio", colour = "Grouping") +
  coord_flip()

ggsave(file="latex/figures/world.pdf",  plot = plot_ir ,width = 7, height = 7)

#################################################
# plot excess intersectionality for all countries
#################################################

plot_diff <- cohort %>% 
  arrange(country_long, desc(cohort_cat)) %>% 
  group_by(country_long) %>% 
  slice_head(n=1) %>% 
  ungroup() %>% 
  mutate(country_long = fct_recode(country_long, 
                                   "Bolivia*" = "Bolivia", 
                                   "Brazil*"="Brazil", 
                                   "C.African Rep.°" = "Central African Republic", 
                                   "Ivory Coast°" = "Ivory Coast", 
                                   "Kazakhstan*" = "Kazakhstan", 
                                   "Namibia*" = "Namibia",
                                   "Philippines*" = "Philippines", 
                                   "Rwanda°" = "Rwanda", 
                                   "Zimbabwe°" = "Zimbabwe")) %>% 
  filter(!is.na(diff_mech)) %>% 
  mutate(diff_mech = diff_mech*100) %>% 
  select(country_long, diff_mech, r_gen.eth) %>% 
  ggplot(aes(x=fct_reorder(country_long, diff_mech), y= diff_mech, colour = r_gen.eth)) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha=0.5) + 
  geom_point() +
  geom_text(data=. %>% filter(
    diff_mech>3 | (diff_mech<0 & diff_mech>-5)), # Filter data first
            aes(label=country_long, y=diff_mech),
            nudge_y = -1.2,
            size=3) +
  geom_text(data=. %>% filter(
    (diff_mech<3 & diff_mech>0) | diff_mech < -5), # Filter data first
            aes(label=country_long, y=diff_mech),
            nudge_y = 1.2,
            size=3) +
  coord_flip() +
  scale_colour_viridis_b(option = "mako", direction = -1, end = 0.85)+
  labs(colour = "Inequality ratio", x="", y="Differential intersectionality") +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom")

ggsave("latex/figures/diff-mech.pdf", plot = plot_diff, width = 7, height = 7)

      ####################################################
# plot sensitivity analysis varying number of groups
####################################################

sim_all %>% 
  filter(simulation %in% c("sample size")) %>% 
  mutate(n_groups = as_factor(n_groups)) %>% 
  ggplot(aes(x=samp_size, y=1-mean, color =n_groups, group=n_groups, shape=n_groups))+
  geom_point() +
  geom_line() + 
  scale_colour_viridis_d(option="mako", end = 0.85) +
  scale_x_continuous(breaks = c(2000, 10000, 20000, 30000)) + 
  labs(x="sample size", 
       y="simulated inequality ratio", 
       color="Groups", 
       shape = "Groups") +
  ylim(0, 1)

ggsave("latex/figures/sensitivity-groups.pdf", width = 7, height = 5)

######################################################
# plot sensitivity analysis varying group distribution
######################################################

sim_all %>% 
  filter(simulation %in% c("even", "onesmall", "onelarge")) %>% 
  mutate(simulation = fct_recode(simulation,
                                 "uniform" = "even", 
                                 "one large group" = "onelarge", 
                                 "one small group" = "onesmall"
  )) %>% 
  ggplot(aes(x=samp_size, y=mean, color = simulation))+
  geom_point() +
  geom_line() + 
  scale_colour_viridis_d(option="mako", end= 0.85) +
  labs(x="sample size", y="simulated inequality ratio", color="Group distribution") + 
  ylim(0, 1)

ggsave("latex/figures/sensitivity-distribution.pdf", width = 7, height = 5)

##############################################
# plot correlation b/w group inequality ratios
#############################################
limitRange <- function(data, mapping, ...) { 
  ggplot(data = data, mapping = mapping, ...) + 
    geom_point(...) + 
    scale_y_continuous(limits = c(0,1)) +
    scale_x_continuous(limits = c(0,1)) 
}
cor_r <- cohort %>% 
  select("Cohort" = cohort_cat, "IR(gender)" = r_gen,"IR(ethnicity)"=r_eth, "IR(genderXethnicity)"=r_gen.eth) %>% 
  ggpairs(columns = 2:4, 
          ggplot2::aes(colour=Cohort, alpha=0.5),
          xlab = "Inequality ratio",
          ylab = "Inequality ratio",
          lower = list(continuous = limitRange),
          upper = list(continuous = wrap('cor', method = "spearman", size=3)),
  )
for(i in 1:cor_r$nrow) {
  for(j in 1:cor_r$ncol){
    cor_r[i,j] <- cor_r[i,j] + 
      scale_colour_viridis_d(option="mako", end = 0.8)+
      scale_fill_viridis_d(option="mako", end = 0.8)
  }
}  

cor_r

ggsave(plot = cor_r, "latex/figures/cor-group.pdf", width = 7, height = 5)
