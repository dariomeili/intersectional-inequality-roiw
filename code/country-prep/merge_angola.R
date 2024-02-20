# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/AO"))

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
  map(~as_label(., v025res, v130rel, v131eth, v007year)) %>% 
  map(~sjlabelled::as_factor(., v007year))

names(dfs) <- meta %>% pull(file_name)
angola <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "angola", 
         country_code = "AO")

# gender
angola %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))
# fix year
angola %>% 
  frq(v007year)

angola %<>%
  mutate(year = as_numeric(as_character(v007year)))
  
# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
angola %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort > 1970 & cohort <= 1980 ~ "1971-1980",
           cohort > 1980 ~ "1981-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1971-1980", "1981-")) %>% 
  filter(age >= 18)

# harmonize ethnicity
angola %>% 
  frq(v131eth) #there are no ethnicity entries
angola %<>% 
  mutate(ethnicity = as_factor(v131eth))
# harmonize religion
angola %>% 
  flat_table(v130rel, year)
angola %>% 
  flat_table(v130rel, cohort_cat, gender)

angola %<>% 
  mutate(religion = fct_collapse(v130rel,
                                "catholic" = "catholic", 
                                "protestant" = "protestant",
                                "other christian" = c("methodist", "assembly of god", "universal", "jehovah's witnesses"),
                                 other_level = "other"))

angola %>% 
  flat_table(religion, cohort_cat)

# write data set
angola%<>% 
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
df <-  bind_rows(df, angola)
rm(meta, dfs, angola)
