
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/PK"))

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

pakistan <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "pakistan", 
         country_code = "PK")

pakistan %>% 
  frq(v007year)

pakistan %<>%
  mutate(year = v007year)
# gender
pakistan %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
pakistan %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>%
  filter(age >= 18)

pakistan %>% 
  frq(cohort_cat)
# harmonize ethnicity
pakistan %>% 
  frq(v131eth) 

pakistan %>% 
  flat_table(v131eth, cohort_cat, gender) 

pakistan %<>% 
  mutate(ethnicity = fct_collapse(droplevels(v131eth),
                                  "Urdu" = c("Urdu", "urdu"),
                                  "Punjabi" = c("punjabi", "Punjabi"),
                                  "Sindhi" = c("sindhi", "Sindhi"), 
                                  "Pushto" = c("pushto", "Pushto"),
                                  "Balochi" = c("balochi", "Balochi"),
                                  "Barauhi" = c("barauhi", "Barauhi"),
                                  "Siraiki" = c("siraiki", "Siraiki"),
                                  "Other" = c("english", "English", "Wakhi", "wakhi", "Marwari", "marwari",
                                              "Farsi", "farsi", "Others", "brushaski", "Brushaski", "others", 
                                              "pahari", "potowari", "Pahari", "Potowari",
                                              "balti", "Balti", "hindko", "Hindko", 
                                              "shina", "Shina", "kashmiri", "Kashmiri", "chitrali/khwar", "Chitrali/Khwar")
                                  )
  ) 

pakistan %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
pakistan %<>% 
  mutate(religion = v130rel)

# write data set
pakistan %<>% 
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
df <-  bind_rows(df, pakistan)
rm(meta, dfs, pakistan)


