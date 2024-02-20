
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/NI"))

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
  map(~sjlabelled::as_factor(., v131eth))

meta

names(dfs) <- meta %>% pull(file_name)

# harmonize year
dfs
dfs[["NIIR22FL"]]$v007year <- 1992
dfs[["NIMR21FL"]]$v007year <- 1992
dfs[["NIIR31FL"]]$v007year <- 1998
dfs[["NIMR31FL"]]$v007year <- 1998

niger <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "niger", 
         country_code = "NI")

niger %>% 
  frq(v007year)

niger %<>%
  mutate(year = v007year)
# gender
niger %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
niger %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>% 
  filter(age >= 18)

niger %>% 
  frq(cohort_cat)
# harmonize ethnicity
niger %>% 
  frq(v131eth) 
niger %>% 
  flat_table(v131eth, cohort_cat, gender) 

niger %<>% 
  mutate(ethnicity = fct_collapse(v131eth,
                                  "Other" = c("arab", "mossi", "toubou", "other", "other non-niger", "arabe", "autre", "non-niger",
                                              "gourmanthe", "gourmantch‚", "gourmantché"), 
                                  "Djerma" = c("djerma", "djerma/songhai"), 
                                  "Haoussa" = c("haoussa"),
                                  "Kanouri" = c("kanouri"), 
                                  "Peulh" = c("peulh", "peul"),
                                  "Touareg/Touareg Bella" = c("touareg bella", "touareg")
  )
  ) 

niger %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
niger %>% 
  flat_table(v130rel, year)
niger %>% 
  flat_table(v130rel, cohort_cat)

niger %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Muslim" = c("muslem", "muslim"), 
                                 other_level = "Christian/other"
  ))

niger %>% 
  flat_table(religion, cohort_cat)

# write data set
niger %<>% 
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
df <-  bind_rows(df, niger)
rm(meta, dfs, niger)


