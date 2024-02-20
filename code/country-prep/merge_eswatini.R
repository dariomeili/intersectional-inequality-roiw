
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/SZ"))

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
#dfs
#dfs[["KEIR33FL"]]$v007year <- 1999

eswatini <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "eswatini", 
         country_code = "SZ")

eswatini %>% 
  frq(v007year)

eswatini %<>%
  mutate(year = v007year)
# gender
eswatini %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
eswatini %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(cohort < 1970 ~ "-1970", 
                                cohort >= 1970 & cohort < 1985 ~ "1970-1984",
                                cohort >= 1985  ~ "1985-"),
         cohort_cat = fct_relevel(cohort_cat, "-1970", "1970-1984", "1985-")) %>% 
  filter(age >= 18)

eswatini %>% 
  frq(cohort_cat)
# harmonize ethnicity
eswatini %>% 
  frq(v131eth) 

eswatini %<>% 
  mutate(ethnicity = as_factor(v131eth))

# harmonize religion
eswatini %>% 
  flat_table(v130rel, year)
eswatini %>% 
  flat_table(v130rel, cohort_cat)

eswatini %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Zionist" = c("zionist"), 
                                 "Protestant" = c("protestant"),
                                 "Other christian" = c("pentecostal", "charismatic", "pentecostal", "apostolic sect", "roman catholic"),
                                 "Other/none" = c("islam", "traditional", "other", "none")
  ))

eswatini %>% 
  flat_table(religion, cohort_cat)

# write data set
eswatini %<>% 
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
df <-  bind_rows(df, eswatini)
rm(meta, dfs, eswatini)


