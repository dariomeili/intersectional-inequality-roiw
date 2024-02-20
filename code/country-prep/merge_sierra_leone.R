
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/SL"))

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

sierra_leone <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "sierra_leone", 
         country_code = "SL")

sierra_leone %>% 
  frq(v007year)

sierra_leone %<>%
  mutate(year = v007year)
# gender
sierra_leone %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
sierra_leone %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>%
  filter(age >= 18)

sierra_leone %>% 
  frq(cohort_cat)
# harmonize ethnicity
sierra_leone %>% 
  frq(v131eth) 
sierra_leone %>% 
  flat_table(v131eth, cohort_cat,gender) 

sierra_leone %<>% 
  mutate(ethnicity = fct_collapse(v131eth,
                                  "Kono" = c("kono","Kono"),
                                  "Mende" = c("mende","Mende"),
                                  "Limba" = c("limba","Limba"),
                                  "Loko" = c("loko" ,"Loko"),
                                  "Temne" = c("temne" ,"Temne"),
                                  "Mandingo" = c("mandingo" ,"Mandingo"),
                                  "Sherbro" = c("sherbro","Sherbro"),
                                  "Creole" = c("kriole", "creole", "Creole"),
                                  "Fullah" = c("Fullah", "fullah"),
                                  "Other" = c("other sierra leone","other foreign","other non sierra leone", "Other Sierra Leone","Other Foreign",
                                              "koranko", "Koranko", "korankoh") 
  )
  ) 

sierra_leone %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
sierra_leone %>% 
  flat_table(v130rel, year)
sierra_leone %>% 
  flat_table(v130rel, cohort_cat)

sierra_leone %<>%
  mutate(religion = fct_collapse(droplevels(v130rel), 
                                 "Christian/other" = c("christian", "Christian", "bahai", "none", "other", "traditional", "Bahai", "Traditional", "None"), 
                                 "Muslim" = c("islam", "Islam")
  ))

sierra_leone %>% 
  flat_table(religion, cohort_cat)

# write data set
sierra_leone %<>% 
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
df <-  bind_rows(df, sierra_leone)
rm(meta, dfs, sierra_leone)


