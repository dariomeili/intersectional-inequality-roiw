
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/ID"))

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
dfs[["IDIR51FL"]]$v007year <- 2007
dfs[["IDMR51FL"]]$v007year <- 2007

indonesia <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "indonesia", 
         country_code = "ID")

indonesia %>% 
  frq(v007year)

indonesia %<>%
  mutate(year = v007year)
# gender
indonesia %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
indonesia %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(cohort < 1965 ~ "-1964",
                                cohort >= 1965 & cohort < 1975 ~ "1965-1974", 
                                cohort >= 1975 & cohort < 1985 ~ "1975-1984",
                                cohort >= 1985  ~ "1985-"),
         cohort_cat = fct_relevel(cohort_cat, "-1964", "1965-1974", "1975-1984", "1985-")) %>% 
  filter(age >= 18)

indonesia %>% 
  frq(cohort_cat)
# harmonize ethnicity
indonesia %>% 
  frq(v131eth) 

indonesia %<>% 
  mutate(ethnicity = as_factor(v131eth))

# harmonize religion
indonesia %>% 
  flat_table(v130rel, year)
indonesia %>% 
  flat_table(v130rel, cohort_cat)

indonesia %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "muslim" = c("islam", "muslim"),
                                 "protestant" = c("protestant", "christian/protestant", "christian/protesstan"), 
                                 "catholic" = c("catholic"),
                                 "other" = c("budhist", "confucian", "other", "buddhist", "khong hu chu", "others", "hindu")
                                 
  ))

indonesia %>% 
  flat_table(religion, cohort_cat)

# write data set
indonesia %<>% 
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
df <-  bind_rows(df, indonesia)
rm(meta, dfs, indonesia)
