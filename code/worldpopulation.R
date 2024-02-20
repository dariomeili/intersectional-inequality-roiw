library(pacman)
p_load(tidyverse, magrittr, vroom, janitor)

world <- vroom("raw-data/worldpopulation.csv")

world %<>% clean_names() %>% 
  select(country_name, country_code, "pop"= x2021_yr2021)

load("processed-data/cohort.Rda") 

ctry_names <- cohort %>% 
  distinct(country_long, alpha_3)

world_merge <- left_join(ctry_names, world, by=c("alpha_3"="country_code"))

load("processed-data/countries.Rda")

countries %<>% 
  select(alpha_3, region)

world_merge %<>% 
  left_join(countries, by= "alpha_3")

# extract total world population
world_pop <- world %>% 
  filter(country_name=="World") %>% 
  pull(pop)

# extract total aftican population
total_pop_africa <- world %>% 
  filter(country_name == "Sub-Saharan Africa") %>% 
  pull(pop)

# sample share of total world population
total_sample_pop <- world_merge %>% 
  summarize(pop = sum(pop)) %>% 
  pull(pop)

total_sample_pop/world_pop

# sample share of total african population
africa_sample_pop <- world_merge %>% 
  filter(region == "Sub-Saharan Africa") %>% 
  summarize(pop = sum(pop)) %>% 
  pull(pop)

africa_sample_pop/total_pop_africa
