
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/PH"))

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


philippines <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "philippines", 
         country_code = "PH")

philippines %>% 
  frq(v007year)

philippines %<>%
  mutate(year = v007year)
# gender
philippines %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
philippines %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(cohort < 1970 ~ "-1970", 
                                cohort >=1970 & cohort < 1980 ~ "1970-1979",
                                cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat)) %>% 
  filter(age >= 18)

philippines %>% 
  frq(cohort_cat)
# harmonize ethnicity
philippines %>% 
  frq(v131eth) 
philippines %>% 
  flat_table(v131eth, cohort_cat) 

philippines %<>% 
  mutate(ethnicity = fct_collapse(v131eth,
                                  "other bisaya" = c("karay-a", "bisaya", "akeanon/aklanon", "surigaonon", "tausog", "cuyuno"), 
                                  "cebuano" = c("boholano"),
                                  "other" = c("maranao", "parggalatok/panggasinense", "chavakano", "ibaloi", "ifugao", "manabo", "sama", "kankanaey", "igorot"))
  
  ) 

philippines %>% 
  flat_table(ethnicity, cohort_cat)

# harmonize religion
philippines %>% 
  flat_table(v130rel, year)
philippines %>% 
  flat_table(v130rel, cohort_cat)

philippines %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Catholic" = c("roman catholic"), 
                                 "Protestant/other christian" = c("iglesia ni kristo", 
                                                                  "protestant", 
                                                                  "aglipay", 
                                                                  "born-again/jehovah's witness/sda", 
                                                                  "born-again/jehovah's wit/sda"),
                                 "Muslim" = c("islam"),
                                 "Other/no religion" = c("others", "none")
  ))

philippines %>% 
  flat_table(religion, cohort_cat)

# write data set
philippines %<>% 
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
df <-  bind_rows(df, philippines)
rm(meta, dfs, philippines)


