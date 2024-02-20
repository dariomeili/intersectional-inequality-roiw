# Preamble ----
# load packages
library(pacman)
p_load(tidyverse, estimatr, magrittr, fixest)

#load data
load("processed-data/cohort.Rda") 
load("processed-data/dhs_main.Rda")

# load custom functions
source("code/functions/functions.R")

# fix regions, generate africa dummy 
cohort %<>% 
  mutate(
    sigi_region = case_when(country_long == "Congo" ~ "Africa", 
                            country_long == "Niger" ~ "Africa", 
                            country_long == "Guyana" ~ "Americas", 
                            TRUE ~ sigi_region),
    africa = fct_collapse(sigi_region, 
                          "Not Africa" = c("Americas", "Asia", "Europe"),
                          "Africa" = c("Africa")), 
    africa = fct_relevel(africa, "Not Africa", "Africa"))

# check size of extreme groups: 
temp <- cohort %>% 
  select(country_long, cohort_cat, l_gen.eth_n, l_gen.eth_name,  h_gen.eth_n, h_gen.eth_name) %>% 
  filter(l_gen.eth_n<40 | h_gen.eth_n<40)

# regressions inequality ratio ----

df_reg <- cohort %>% 
  mutate(log_gdp_pc = log(gdp_pc),
         l_share = l_share*100, 
         h_share = h_share*100, 
         n_sample = sample_size/1000) %>% 
  filter(!(country_long == "Ethiopia" & cohort_cat == "-1969")) # have to exclude because lowest group has 0 education, resulting in NA ratio

## education variables ----
m_educ <- df_reg %>% 
  feols(c(r_gen.eth, r_mech, diff_mech) ~ csw(educ_w, africa + cohort_cat), 
        cluster = ~alpha_3)

### Africa Dummy Significance ----
m_educ_1 <- df_reg %>% 
  feols(c(r_gen.eth, r_mech, diff_mech) ~ educ_w + africa + cohort_cat, 
        cluster = ~alpha_3)

etable(m_educ_1)

## sample size variables  ----
m_samp <- df_reg %>% 
  feols(c(r_gen.eth, r_mech, diff_mech) ~ csw(n_sample + n_ethnicity, 
                                              africa + cohort_cat), 
        cluster = ~alpha_3)

# group size variables
m_group <- df_reg %>% 
  feols(c(r_gen.eth, r_mech, diff_mech) ~ csw(l_gen.eth_n + h_gen.eth_n, 
                                              africa + cohort_cat), 
        cluster = ~alpha_3)

# theil index regressions ----
m_theil <- df_reg %>% 
  feols(theil_gen.eth ~ sw(educ_w, 
                           n_sample + n_ethnicity, 
                           l_gen.eth_n + h_gen.eth_n) + africa + cohort_cat,
        cluster = ~ alpha_3)




  