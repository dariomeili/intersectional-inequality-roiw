
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/TL"))

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
#dfs[["KEIR33FL"]]$v007year <- 1999

timor_leste <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "timor_leste", 
         country_code = "TL")

timor_leste %>% 
  frq(v007year)

timor_leste %<>%
  mutate(year = v007year)
# gender
timor_leste %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
timor_leste %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(cohort < 1970 ~ "-1970", 
                                cohort >= 1970 & cohort < 1985 ~ "1970-1984",
                                cohort >= 1985  ~ "1985-"),
         cohort_cat = fct_relevel(cohort_cat, "-1970", "1970-1984", "1985-")) %>% 
  filter(age >= 18)

timor_leste %>% 
  frq(cohort_cat)
# harmonize ethnicity
timor_leste %>% 
  frq(v131eth)

timor_leste %<>% 
  mutate(ethnicity = as_factor(v131eth)
  )
  
# harmonize religion
timor_leste %>% 
  flat_table(v130rel, year)
timor_leste %>% 
  flat_table(v130rel, cohort_cat)

timor_leste %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Catholic" = c("roman catholic"), 
                                 "Other" = c("muslim", "protestant", "hindu", "other")
  ))

timor_leste %>% 
  flat_table(religion, cohort_cat)

# write data set
timor_leste %<>% 
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
df <-  bind_rows(df, timor_leste)
rm(meta, dfs, timor_leste)


