# Preamble ----
library(pacman)
p_load(tidyverse, vroom, haven, sjmisc, magrittr, sjlabelled, countrycode)


# df with country codes, regions, and country names ----
country_codes <- vroom("raw-data/country_codes.csv", delim = ";") 
  
country_codes %<>% filter(!is.na(dhs_2))

regions <- vroom("raw-data/wb_worldregions.csv")

countries <- inner_join(country_codes, regions, by=c("Alpha-3 code"="Code")) %>% 
  select("country_long" = Country, "alpha_2" = "Alpha-2 code", "alpha_3" = "Alpha-3 code", 
         "dhs_2" = "dhs_2","region" = "World Region according to the World Bank")

countries %<>% 
  mutate(country_long = fct_recode(country_long,
                            "Tanzania"="Tanzania, United Republic of",
                            "DR Congo" = "Congo, the Democratic Republic of the", 
                            "Moldova" = "Moldova, Republic of"))

# simplify regions
countries %<>% 
  mutate(region = fct_collapse(region, 
                               "Asia, Europe, and Pacific" = c("East Asia and Pacific",
                                                               "Europe and Central Asia",
                                                               "South Asia")))
usa_codes <-  tibble("country_long" = "United States", "alpha_2" = "US", "alpha_3" = "USA", "dhs_2" = "US", "region"="USA")

countries <- bind_rows(countries, usa_codes)
save(countries, file="processed-data/countries.Rda")

# Read DHS data ----
# extract list of all files in dir
all_dfs <- list.files(path = "raw-data/dhs", pattern = "*.dta|*.DTA", 
                      full.names = T)
# name the dataframes in list with country_year
names(all_dfs) <-  gsub("(raw\\-data/dhs/|\\.DTA|\\.dta)", "", all_dfs) 

col_select <- c("v001", "v002", "v003", "v005", "v007", "v012", "v025", "v133", "v131", "v130",
                "mv001", "mv002", "mv003","mv005", "mv007", "mv012","mv025","mv133","mv131","mv130")
names_dfs <- tibble(names(all_dfs), all_dfs)
# list of all country-level data cleaning scripts
file.sources <-  list.files(path = "code/country-prep", pattern="merge_", full.names = T)
# run alls scripts
sapply(file.sources, source, .GlobalEnv)

# merge country list with individual file ----
df %<>% 
  left_join(countries, by=c("country_code"="dhs_2"))

# delete and save
save(df, file = "processed-data/merged.Rda")
rm(country_codes, regions, col_select, countries, usa_codes)
