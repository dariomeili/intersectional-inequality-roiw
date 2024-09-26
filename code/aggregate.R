# Set Global Options ----
options(dplyr.summarise.inform = FALSE)
options(tidyverse.quiet = TRUE)

# Load Data ----
load("processed-data/merged.Rda")

# Source Functions ----
files.sources <- list.files(path = "code/functions", full.names = TRUE)
sapply(files.sources, source, .GlobalEnv)

# Data Preprocessing ----

# Define cohorts, filter by age and education
df %<>% 
  mutate(age = as.numeric(as.character(haven::as_factor(age)))) %>%
  filter(!is.na(education), age >= 25, cohort > 1920) %>%
  mutate(
    cohort_cat = case_when(
      cohort <= 1969 ~ "-1969",
      cohort >= 1970 & cohort < 1980 ~ "1970-1979",
      cohort >= 1980 ~ "1980-"
    ),
    cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-"),
    education = case_when(education > 17 ~ 17, TRUE ~ as.numeric(education))
  )

# Produce intersections by gender and religion/ethnicity
df %<>%
  mutate(
    religion = as_factor(religion),
    gen.rel = fct_cross(gender, religion),
    gen.eth = fct_cross(gender, ethnicity)
  ) %>%
  filter(!is.na(gen.eth))

# Harmonize residence spelling
df %<>% 
  mutate(residence = fct_collapse(residence, "urban" = c("urban", "Urban"), "rural" = c("rural", "Rural")))

# Nest Dataframes (Pooled & Cohort) ----

# Pooled data nested by country
df_pool <- df %>%
  group_nest(country_long, alpha_3)

# Cohort data nested by country and cohort category
df_cohort <- df %>%
  group_nest(country_long, alpha_3, cohort_cat)

# Pooled Data Processing ----

# Calculate gender-weighted means for pooled data
temp <- df %>%
  count(country_long, gender) %>%
  group_by(country_long) %>%
  mutate(pct_female = n / sum(n)) %>%
  filter(gender == "female") %>%
  select(-gender, -n)

temp_pool <- df %>%
  full_join(temp, by = c("country_long")) %>%
  mutate(
    gw = case_when(
      gender == "female" ~ 0.5 / pct_female,
      gender == "male" ~ 0.5 / (1 - pct_female)
    ),
    weduc = education * gw
  ) %>%
  group_by(country_long) %>%
  summarise(gw_educ = sum(weduc) / sum(gw))

df_pool %<>%
  full_join(temp_pool, by = c("country_long"))

# Calculate statistics for pooled data
df_pool %<>%
  mutate(
    educ_w = map_dbl(data, ~weighted.mean(.x[["education"]], .x[["wgt"]])),
    gen_means = map(data, ~group_mean(.x, education, gender, wgt)),
    theil_gen = map_dbl(data, ~group_theil(.x, education, gender, wgt)),
    eth_means = map(data, ~group_mean(.x, education, ethnicity, wgt)),
    gen.eth_means = map(data, ~group_mean(.x, education, gen.eth, wgt)),
    educ_gini = map_dbl(data, ~Gini(.x[["education"]], na.rm = TRUE))
  )

# Aggregate statistics for pooled data
df_pool %<>% 
  mutate(educ_w = map_dbl(data, ~weighted.mean(.x[["education"]], .x[["wgt"]])),
         # gender:
         gen_means = map(data, ~group_mean(.x, education, gender, wgt)),
         n_gen = map(data, ~group_n(.x, gender)),
         theil_gen = map_dbl(data, ~group_theil(.x, education, gender, wgt)), 
         r_gen = map_dbl(gen_means, ~ineq_ratio(.x)), 
         # ethnicity:
         eth_means = map(data, ~group_mean(.x, education, ethnicity, wgt)),
         n_eth = map(data, ~group_n(.x, ethnicity)),
         theil_eth = map_dbl(data, ~group_theil(.x, education, ethnicity, wgt)), 
         r_eth = map_dbl(eth_means, ~ineq_ratio(.x)), 
         # gen.eth:
         gen.eth_means = map(data, ~group_mean(.x, education, gen.eth, wgt)),
         n_gen.eth = map(data, ~group_n(.x, gen.eth)),
         theil_gen.eth = map_dbl(data, ~group_theil(.x, education, gen.eth, wgt)), 
         r_gen.eth = map_dbl(gen.eth_means, ~ineq_ratio(.x)), 
         # education gini: 
         educ_gini = map_dbl(data, ~Gini(.x[["education"]], na.rm = TRUE))
  )

# add aggregate statistics to pooled data
df_pool %<>% 
  mutate(
    #gender:
    l_gen_name = map_chr(gen_means, ~name_extractor(.x, group = "low")),
    l_gen_mean = map_dbl(gen_means, ~mean_extractor(.x, group = "low")),
    h_gen_name = map_chr(gen_means, ~name_extractor(.x, group = "high")),
    h_gen_mean = map_dbl(gen_means, ~mean_extractor(.x, group = "high")),
    #ethnicity:
    l_eth_name = map_chr(eth_means, ~name_extractor(.x, group = "low")),
    l_eth_mean = map_dbl(eth_means, ~mean_extractor(.x, group = "low")),
    h_eth_name = map_chr(eth_means, ~name_extractor(.x, group = "high")),
    h_eth_mean = map_dbl(eth_means, ~mean_extractor(.x, group = "high")),
    #gen.eth:
    l_gen.eth_name = map_chr(gen.eth_means, ~name_extractor(.x, group = "low")),
    l_gen.eth_mean = map_dbl(gen.eth_means, ~mean_extractor(.x, group = "low")),
    h_gen.eth_name = map_chr(gen.eth_means, ~name_extractor(.x, group = "high")),
    h_gen.eth_mean = map_dbl(gen.eth_means, ~mean_extractor(.x, group = "high"))
  )

# relative size of lowest and highest intersectional group
df_pool %<>% 
  mutate(l_share = map2_dbl(n_gen.eth, gen.eth_means, ~share_extractor(.x, .y, type="low")),
         h_share = map2_dbl(n_gen.eth, gen.eth_means, ~share_extractor(.x, .y, type="high")))

# absolute size of the lowest and the highest groups
df_pool %<>%
  mutate(
    l_gen_n = map2_dbl(n_gen, gen_means, ~groupn_extractor(.x, .y, type = "low")),
    h_gen_n = map2_dbl(n_gen, gen_means, ~groupn_extractor(.x, .y, type = "high")),
    l_eth_n = map2_dbl(n_eth, eth_means, ~groupn_extractor(.x, .y, type = "low")),
    h_eth_n = map2_dbl(n_eth, eth_means, ~groupn_extractor(.x, .y, type = "high")),
    l_gen.eth_n = map2_dbl(n_gen.eth, gen.eth_means, ~groupn_extractor(.x, .y, type = "low")),
    h_gen.eth_n = map2_dbl(n_gen.eth, gen.eth_means, ~groupn_extractor(.x, .y, type = "high"))
  )

# share of rural population in lowest and highest group
df_pool %<>% 
  mutate(l_rural = map2_dbl(data, l_gen.eth_name, ~rural_pct(.x, .y)),
         h_rural = map2_dbl(data, h_gen.eth_name, ~rural_pct(.x, .y))) 

# compute counterfactual and differential intersectionality
df_pool %<>% 
  mutate(mech_l = l_eth_mean*(l_gen_mean/gw_educ),
         mech_h = h_eth_mean*(h_gen_mean/gw_educ),
         r_mech = 1 - (mech_l/mech_h),
         diff_mech = r_gen.eth - r_mech
  )

# share of zero's
df_pool %<>% 
  mutate(zero_educ = map_dbl(data, ~pct_zero(.x)))

# write do smaller dataset
pool <- df_pool %>% 
  select(-data)

# Cohort Data Processing ----

# Calculate gender-weighted means for cohort data
temp <- df %>%
  count(country_long, cohort_cat, gender) %>%
  group_by(country_long, cohort_cat) %>%
  mutate(pct_female = n / sum(n)) %>%
  filter(gender == "female") %>%
  select(-gender, -n)

temp_cohort <- df %>%
  full_join(temp, by = c("country_long", "cohort_cat")) %>%
  mutate(
    gw = case_when(
      gender == "female" ~ 0.5 / pct_female,
      gender == "male" ~ 0.5 / (1 - pct_female)
    ),
    weduc = education * gw
  ) %>%
  group_by(country_long, cohort_cat) %>%
  summarise(gw_educ = sum(weduc) / sum(gw))

df_cohort %<>%
  full_join(temp_cohort, by = c("country_long", "cohort_cat"))

# Calculate statistics for cohort data
df_cohort %<>% 
  mutate(educ_w = map_dbl(data, ~weighted.mean(.x[["education"]], .x[["wgt"]])),
         # gender:
         gen_means = map(data, ~group_mean(.x, education, gender, wgt)),
         n_gen = map(data, ~group_n(.x, gender)),
         theil_gen = map_dbl(data, ~group_theil(.x, education, gender, wgt)), 
         r_gen = map_dbl(gen_means, ~ineq_ratio(.x)), 
         # ethnicity:
         eth_means = map(data, ~group_mean(.x, education, ethnicity, wgt)),
         n_eth = map(data, ~group_n(.x, ethnicity)),
         theil_eth = map_dbl(data, ~group_theil(.x, education, ethnicity, wgt)), 
         r_eth = map_dbl(eth_means, ~ineq_ratio(.x)), 
         # gen.eth:
         gen.eth_means = map(data, ~group_mean(.x, education, gen.eth, wgt)),
         n_gen.eth = map(data, ~group_n(.x, gen.eth)),
         theil_gen.eth = map_dbl(data, ~group_theil(.x, education, gen.eth, wgt)), 
         r_gen.eth = map_dbl(gen.eth_means, ~ineq_ratio(.x)), 
         # education gini: 
         educ_gini = map_dbl(data, ~Gini(.x[["education"]], na.rm = TRUE))
  )
# Aggregate statistics for cohort data
df_cohort %<>%
  mutate(#gender:
    l_gen_name = map_chr(gen_means, ~name_extractor(.x, group = "low")),
    l_gen_mean = map_dbl(gen_means, ~mean_extractor(.x, group = "low")),
    h_gen_name = map_chr(gen_means, ~name_extractor(.x, group = "high")),
    h_gen_mean = map_dbl(gen_means, ~mean_extractor(.x, group = "high")),
    #ethnicity:
    l_eth_name = map_chr(eth_means, ~name_extractor(.x, group = "low")),
    l_eth_mean = map_dbl(eth_means, ~mean_extractor(.x, group = "low")),
    h_eth_name = map_chr(eth_means, ~name_extractor(.x, group = "high")),
    h_eth_mean = map_dbl(eth_means, ~mean_extractor(.x, group = "high")),
    #gen.eth:
    l_gen.eth_name = map_chr(gen.eth_means, ~name_extractor(.x, group = "low")),
    l_gen.eth_mean = map_dbl(gen.eth_means, ~mean_extractor(.x, group = "low")),
    h_gen.eth_name = map_chr(gen.eth_means, ~name_extractor(.x, group = "high")),
    h_gen.eth_mean = map_dbl(gen.eth_means, ~mean_extractor(.x, group = "high"))
  )


# Additional Variables for Cohort Data ----
# relative size of lowest and highest intersectional group
df_cohort %<>% 
  mutate(l_share = map2_dbl(n_gen.eth, gen.eth_means, ~share_extractor(.x, .y, type="low")),
         h_share = map2_dbl(n_gen.eth, gen.eth_means, ~share_extractor(.x, .y, type="high"))) 

# absolute size of the lowest and the highest groups
df_cohort %<>%
  mutate(
    l_gen_n = map2_dbl(n_gen, gen_means, ~groupn_extractor(.x, .y, type = "low")),
    h_gen_n = map2_dbl(n_gen, gen_means, ~groupn_extractor(.x, .y, type = "high")),
    l_eth_n = map2_dbl(n_eth, eth_means, ~groupn_extractor(.x, .y, type = "low")),
    h_eth_n = map2_dbl(n_eth, eth_means, ~groupn_extractor(.x, .y, type = "high")),
    l_gen.eth_n = map2_dbl(n_gen.eth, gen.eth_means, ~groupn_extractor(.x, .y, type = "low")),
    h_gen.eth_n = map2_dbl(n_gen.eth, gen.eth_means, ~groupn_extractor(.x, .y, type = "high"))
  )

# share of rural population in lowest and highest group
df_cohort %<>% 
  mutate(l_rural = map2_dbl(data, l_gen.eth_name, ~rural_pct(.x, .y)),
         h_rural = map2_dbl(data, h_gen.eth_name, ~rural_pct(.x, .y))) 

# find means of mechanical lowest and highest groups
df_cohort %<>% 
  mutate(mech_l = l_eth_mean*(l_gen_mean/gw_educ),
         mech_h = h_eth_mean*(h_gen_mean/gw_educ),
         r_mech = 1 - (mech_l/mech_h),
         diff_mech = r_gen.eth - r_mech
  )

# share of zero's
df_cohort %<>% 
  mutate(zero_educ = map_dbl(data, ~pct_zero(.x)))

# write do smaller dataset
cohort <- df_cohort %>% 
  select(-data)
# Add GDP data
gdp <- read_csv("raw-data/gdp-per-capita-worldbank.csv") %>%
  select(-Entity, "gdp_pc" = `GDP per capita, PPP (constant 2011 international $)`)

gdp_pc <- gdp %>%
  arrange(Code, Year) %>%
  group_by(Code) %>%
  slice(n())

cohort %<>% 
  left_join(gdp_pc, by = c("alpha_3" = "Code"))

# Add SIGI index data
sigi <- read_csv("raw-data/sigi.csv") %>%
  select("sigi_code" = LOCATION, "sigi_region" = Region, "sigi_var" = VAR, "SIGI" = Value) %>%
  filter(sigi_var == "SIGI_2") %>%
  select(-sigi_var) %>%
  distinct(sigi_code, SIGI, .keep_all = TRUE)

cohort %<>%
  left_join(sigi, by = c("alpha_3" = "sigi_code"))

# calculate other aggregate stats
# calculate sample size and sample weighted mean
agg_stats <- df %>%
  group_by(country_long, cohort_cat) %>% 
  summarise(sample_size = n(), 
            mean_educ = weighted.mean(education, w=wgt, na.rm=T),
            sd_educ = wtd.sd(education, weights = wgt))

# calculate percent of females in sample
temp <- df %>% 
  count(country_long, cohort_cat, gender) %>% 
  group_by(country_long, cohort_cat) %>% 
  mutate("pct_female" = n/sum(n)) %>% 
  filter(gender=="female") %>% 
  select(-gender, -n)

agg_stats %<>% 
  full_join(temp, by=c("country_long", "cohort_cat"))

# calculate number of ethnic groups
temp <- df %>% 
  count(country_long, cohort_cat, ethnicity) %>% 
  filter(!is.na(ethnicity)) %>% 
  count(country_long, ethnicity, cohort_cat) %>% 
  group_by(country_long, cohort_cat) %>% 
  summarise(n_ethnicity = sum(n)) 

agg_stats %<>% 
  full_join(temp, by=c("country_long", "cohort_cat"))

# join with cohort data
cohort %<>% 
  left_join(agg_stats %>% 
              select(country_long, cohort_cat, sample_size, pct_female, n_ethnicity, sd_educ),
            by=c("country_long", "cohort_cat"))

# Save Data ----

save(df_cohort, file = "processed-data/df_cohort.Rda")
save(df_pool, file = "processed-data/df_pool.Rda")
save(pool, file = "processed-data/pool.Rda")
save(cohort, file = "processed-data/cohort.Rda")

# Save unnested dataframe
dhs_main <- df
save(dhs_main, file = "processed-data/dhs_main.Rda")
