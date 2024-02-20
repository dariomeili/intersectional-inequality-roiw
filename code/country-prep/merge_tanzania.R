
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/TZ"))

dfs <- meta %>% 
  pull(file_path) %>% 
  map(~read_dta(., col_select = any_of(col_select))) %>% 
  map(~var_rename(., 
                  "v001" = "v001cluster",
                  "v002" = "v002hh", 
                  "v003" = "v003respondent", 
                  "v005" = "v005wgt",
                  "v007" = "v007year", 
                  "v012" = "v012age", 
                  "v025" = "v025res",
                  "v133" = "v133educ",
                  "v131" = "v131eth",
                  "v130" = "v130rel",
                  "mv001" = "v001cluster",
                  "mv002" = "v002hh", 
                  "mv003" = "v003respondent", 
                  "mv005" = "v005wgt",
                  "mv007" = "v007year", 
                  "mv012" = "v012age", 
                  "mv025" = "v025res",
                  "mv133" = "v133educ",
                  "mv131" = "v131eth",
                  "mv130" = "v130rel", 
                  verbose = FALSE)) %>% 
  map(~as_label(., v025res, v130rel, v131eth, v007year))

meta

names(dfs) <- meta %>% pull(file_name)

# harmonize year
dfs
dfs[["TZIR21FL"]]$v007year <- 1992
dfs[["TZIR3AFL"]]$v007year <- 1996
dfs[["TZIR41FL"]]$v007year <- 1999
dfs[["TZMR21FL"]]$v007year <- 1992
dfs[["TZMR3AFL"]]$v007year <- 1996
dfs[["TZMR41FL"]]$v007year <- 1999

dfs[["TZIR4IFL"]]$v130rel <- as_factor(dfs[["TZIR4IFL"]]$v130rel)
tanzania <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "tanzania", 
         country_code = "TZ")

tanzania %>% 
  frq(v007year)

tanzania %<>%
  mutate(year = v007year)
# gender
tanzania %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
tanzania %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(cohort < 1970 ~ "-1970", 
                                cohort >= 1970 & cohort < 1985 ~ "1970-1984",
                                cohort >= 1985  ~ "1985-"),
         cohort_cat = fct_relevel(cohort_cat, "-1970", "1970-1984", "1985-")) %>% 
  filter(age >= 18)

tanzania %>% 
  frq(cohort_cat)
# harmonize ethnicity
tanzania %>% 
  frq(v131eth)

tanzania %<>% 
  mutate(ethnicity = as_factor(v131eth)
  ) 

# harmonize religion
tanzania %>% 
  flat_table(v130rel, year)
tanzania %>% 
  flat_table(v130rel, cohort_cat)

tanzania %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Catholic" = c("catholic", "2"), 
                                 "Protestant" = c("3", "protestant"),
                                 "Muslim" = c("1", "moslem", "muslim"),
                                 "Other/no religion" = c("4", "none", "6", "other", "9")
  ))

tanzania %>% 
  flat_table(religion, cohort_cat)

# write data set
tanzania %<>% 
  select(data_name,
         country,
         country_code,
         "id_cluster" = v001cluster,
         "id_hh" = v002hh,
         "id_respondent" = v003respondent,
         "wgt" = v005wgt,
         year,
         gender,
         age,
         cohort,
         "residence" = v025res, 
         ethnicity, 
         religion, 
         "education" = v133educ)
# write data set
df <-  bind_rows(df, tanzania)
rm(meta, dfs, tanzania)


