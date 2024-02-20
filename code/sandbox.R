xx <- cohort$rlh_eth
yy <- cohort$rlh_gen.eth

xxh <- cohort$rhl_eth
yyh <- cohort$rhl_gen.eth

cor(xx, yy, use="pairwise.complete.obs")
cor(xxh, yyh, use="pairwise.complete.obs")

cor(xx, yy, use="pairwise.complete.obs", method = "spearman")
cor(xxh, yyh, use="pairwise.complete.obs", method="spearman")




cohort %>% 
  ggplot() +
  geom_point(aes(x=mean_educ, y=gdp_pc))

cohort %>% 
  #mutate(gdp_pc = asinh(gdp_pc)) %>% 
  ggplot() +
  geom_point(aes(x=educ_spending, y=r_gen.eth))

temp_data <- cohort %>% 
  mutate(ihs_gdp = asinh(gdp_pc))


temp <- lm_robust(r_gen.eth ~ ihs_gdp + mean_educ + cohort_cat, data=temp_data, clusters = country_long)
summary(temp)
boxplot(xxh)



boxplot(yyh)

cohort_sep %>% 
  ggplot(aes(cohort, fct_rev(alpha_3), fill=n_obs))+
  geom_tile() +
  scale_fill_viridis_b(breaks = c(34, 131, 251.3, 386.4, 556, 749.8, 984.4, 1376.8, 2028.9), 
                    limits=c(0,max(cohort_sep$cohort, na.rm = T)))

cohort_sep %>% 
  filter(n_obs >=50) %>% 
  ggplot(aes(cohort, fct_rev(alpha_3), fill=n_obs))+
  geom_tile() +
  scale_fill_viridis_b(breaks = c(34, 131, 251.3, 386.4, 556, 749.8, 984.4, 1376.8, 2028.9), 
                       limits=c(0,max(cohort_sep$cohort, na.rm = T)))

quantile(cohort_sep$n_obs, probs = seq(0.1, 0.9, 0.1))

pool %>%
  filter(!is.na(r_eth), !is.na(r_gen), !is.na(r_gen.eth)) %>% 
  #pivot_longer(c(r_gen, r_eth, r_gen.eth)) %>%
  #mutate(name=fct_relevel(name, "r_gen", "r_eth", "r_gen.eth")) %>% 
  ggplot(aes(x=fct_reorder(country_long, r_gen.eth))) +
  geom_segment(aes(xend=fct_reorder(country_long, r_gen.eth), y=1, yend=r_gen, colour = "Gender")) +
  geom_segment(aes(xend=fct_reorder(country_long, r_gen.eth), y=r_gen, yend=r_eth, colour = "Ethnicity")) +
  geom_segment(aes(xend=fct_reorder(country_long, r_gen.eth), y=r_eth, yend=r_gen.eth, colour = "Gen:Eth")) +
  geom_point(aes(y=r_gen, colour = "Gender"), size=2) +
  geom_point(aes(y=r_eth, colour = "Ethnicity"), size=2) +
  geom_point(aes(y=r_gen.eth, colour = "Gen:Eth"), size=2) +
  scale_colour_manual(values = c("#00A08A", "#FF0000", "#F2AD00"), breaks = c("Gender", "Ethnicity", "Gen:Eth")) +
  scale_y_reverse() +
  labs(x="", y="Inequality ratio", colour = "Grouping") +
  coord_flip() 

pool %>% 
  filter(!is.na(diff_mech)) %>% 
  ggplot(aes(x=fct_reorder(country_long, diff_mech), y=diff_mech, colour = r_gen.eth)) + 
  geom_point() +
  coord_flip() +
  scale_colour_viridis_c()
  
gender = sample(c("m", "f"), 10, replace = T, prob = c(0.2, 0.8))

educ = sample(c(1:10), 10, replace = T)

dset <- tibble(gender, educ)
dset %<>% 
  mutate(weights = case_when(
    gender == "f" ~ 0.5/0.8, 
    gender == "m" ~ 0.5/0.2
  ))

dset %>% 
  summarise(w_mean = weighted.mean(educ, weights), 
            mean = mean(educ))

pool %>% 
  ggplot(aes(x=r_gen, y=theil_gen))+
  geom_point()

pool %>% 
  ggplot(aes(x=r_eth, y=theil_eth))+
  geom_point()
pool %>% 
ggplot(aes(x=r_gen.eth, y=theil_gen.eth))+
  geom_point()


x=round(runif(100, max=20))
z=as_factor(sample(c(1:8), 100, replace = T))
w = sample(c(1, 1.2, 1.5), 100, replace = T)          

df = tibble(x, z, w)            
df.perm <- df
df.perm[,"z"] <- sample(df[["z"]])

group_means <- df.perm %>% 
  group_by(z) %>% 
  summarise(means=stats::weighted.mean(x, w)) %>% 
  summarise(min_group = min(means, na.rm = T), 
            max_group = max(means, na.rm = T)) %>% 
  mutate(value = min_group/max_group) %>% 
  pull(value)

df %>% ratio_perm(x, z, w)

temp <- df_pool %>% 
  mutate(perm=map_dbl(data, ~ratio_perm(., iterations=10, outcome = education, grouping = gender, weights = wgt)))

df_pool %>% 
  filter(!is.na(r_gen)) %>% 
  select(country_long, adj_r_gen, r_gen) %>% 
  mutate(country_long = fct_reorder(country_long, r_gen)) %>% 
  pivot_longer(adj_r_gen:r_gen) %>% 
  ggplot(aes(x=country_long, y=value, color=name)) + 
  geom_point()+
  coord_flip()

df_pool %>% 
  filter(!is.na(r_eth)) %>% 
  select(country_long, adj_r_eth, r_eth) %>% 
  mutate(country_long = fct_reorder(country_long, r_eth)) %>% 
  pivot_longer(adj_r_eth:r_eth) %>% 
  ggplot(aes(x=country_long, y=value, color=name)) + 
  geom_point()+
  coord_flip()

df_pool %>% 
  filter(!is.na(r_gen.eth)) %>% 
  select(country_long, adj_r_gen.eth, r_gen.eth) %>% 
  mutate(country_long = fct_reorder(country_long, r_gen.eth)) %>% 
  pivot_longer(adj_r_gen.eth:r_gen.eth) %>% 

  ggplot(aes(x=country_long, y=value, color=name)) + 
  geom_point()+
  coord_flip() +
  scale_color_viridis_d() + 
  labs(x="", y="Inequality ratio low/high")

pool %>% 
  select(country_long, perm_gen.eth, r_gen.eth) %>% 
  mutate(country_long = fct_reorder(country_long, r_gen.eth)) %>% 
  pivot_longer(perm_gen.eth:r_gen.eth) %>% 
  mutate(name = fct_reorg(name, 
                          "empirical" = "r_gen.eth",
                          "permutation" = "perm_gen.eth", 
                           )) %>% 
  ggplot(aes(x=country_long, y=value, color=name)) + 
  geom_point()+
  coord_flip() +
  scale_color_viridis_d() + 
  labs(x="", y="Inequality ratio low/high", color = "gender*ethnicity") +
  theme_bw() +
  theme(legend.position="bottom") 

df_pool %>% 
  ggplot(aes(x=fct_reorder(country_long,h_share), y=h_share)) + 
  geom_point() + 
  coord_flip()

df_pool %>% 
  ggplot(aes(x=fct_reorder(country_long,l_share), y=l_share)) + 
  geom_point() + 
  coord_flip()

cohort %>% 
  group_by(country_long) %>% 
  summarise(n = sum(n)) %>% 
  summarise(mean = mean(n))

dhs_main %>% 
  count(country_long, education) %>% 
  ggplot(aes(x=education, y=n))+
  geom_col()+
  facet_wrap("country_long", scales = "free_y")

sample_size %>% 
  ggplot(aes(x=samp_size, y=mean)) + 
  geom_line() +
  geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3)

sample_size %>% 
  ggplot(aes(x=samp_size, y=mean)) + 
  geom_line() +
  geom_ribbon(aes(ymin=lower,ymax=upper),alpha=0.3)

cohort %>% 
  filter(cohort_cat == "1985-") %>% 
  ggplot(aes(x=fct_reorder(country_long, SIGI_2), y=SIGI_2)) + 
  geom_col() +
  coord_flip()

neth <- dhs_main %>% count(country_long, cohort, ethnicity) %>% 
  count(country_long, cohort, ethnicity) %>% 
  group_by(country_long, cohort) %>% 
  summarize(n_ethnicity = sum(n))
  

dhs_main %>% 
  count(country_long, cohort) %>% 
  left_join(neth, by=c("country_long", "cohort")) %>% 
  mutate(n_per_ethn = n/n_ethnicity) %>% 
  ggplot() + 
  geom_tile(aes(x=cohort, y=country_long, fill=n_per_ethn)) +
  scale_fill_viridis_c(direction = -1)
  
dhs_main %>% 
  ggplot(aes(cohort)) + 
  geom_density()

cohort %>% 
  #mutate(gdp_pc = log(gdp_pc)) %>% 
  ggplot(aes(x=gdp_pc, y=diff_mech))+
    geom_point() +
  geom_smooth(method='lm', formula= y~x)

cohort %>% 
  lm_robust(r_gen ~ educ_w, 
            data=.,
            clusters = country_long) %>%
  modelsummary()

dhs_main %>% 
  filter(country_long == "United States") %>%
  group_by(gen.eth) %>% 
  summarize(mean_educ = mean(education)) %>% 
  ggplot(aes(x=mean_educ, y=fct_reorder(gen.eth, mean_educ))) +
           geom_point()

dhs_main %>% 
  filter(country_long == "United States") %>%
  count(education)

dhs_main %>%
  filter(country_long == "Afghanistan") %>% 
  group_by(gen.eth) %>% 
  summarize(mean_educ = mean(education)) %>% 
  ggplot() +
  geom_point(aes(x=mean_educ, y=fct_reorder(gen.eth, mean_educ))) +
  geom_vline(xintercept = 0.792) +
  geom_vline(xintercept = 4.15)

temp2 <- dhs_main %>% 
  filter(country_long == "Afghanistan") %>% 
  group_by(gender) %>% 
  summarize(mean_educ = mean(education))

dhs_main %>%
  filter(country_long == "Uganda") %>% 
  group_by(gen.eth) %>% 
  summarize(mean_educ = mean(education)) %>% 
  filter(gen.eth == "male:baganda")



  ggplot() +
  geom_point(aes(x=mean_educ, y=fct_reorder(gen.eth, mean_educ))) +
  geom_vline(xintercept = 5.19) +
  geom_vline(xintercept = 7.10) +
  geom_vline(xintercept = 5.63)

dhs_main %>% 
  filter(country_long == "Uganda") %>% 
  group_by(gender) %>% 
  summarize(mean_educ = mean(education))
 

dhs_main %>% 
  filter(country_long == "Uganda") %>% 
  group_by(ethnicity) %>% 
  summarize(mean_educ = mean(education)) 


pool %>% 
  filter(country_long=="Afghanistan") %>% 
  select(r_gen.eth, diff_mech, r_mech, mech_l, mech_h, l_gen.eth_mean, h_gen.eth_mean)



# load packages
library(tidyverse)

# generate data
df <- tibble(x = runif(100, 0, 1), 
             y = runif(100, 0, 1), 
             z = sample(c("Scenario A", "Scenario B"), 
                        size=100, 
                        replace = T,
                        prob = c(0.1, 0.9)) # you can adjust the relative frequency by changing this parameter
             )

# plot data
ggplot(df) +
  geom_point(aes(x=x, y=y)) +
  facet_wrap(~z , nrow = 2, ncol = 1) + 
  theme_bw()

