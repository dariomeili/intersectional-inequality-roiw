
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/LB"))

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
liberia <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "liberia", 
         country_code = "LB")

liberia %>% 
  frq(v007year)

liberia %<>%
  mutate(year = v007year)
# gender
liberia %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
liberia %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>% 
  filter(age >= 18)

liberia %>% 
  frq(cohort_cat)
# harmonize ethnicity
liberia %>% 
  frq(v131eth) 
liberia %>% 
  flat_table(v131eth, year) 
liberia %>% 
  flat_table(v131eth, cohort_cat, gender)

liberia %<>% 
  mutate(ethnicity = fct_collapse(v131eth, 
                                  "Bassa" = c("bassa", "Bassa"),
                                  "Other Kru" = c("belle", "Belle", "dey", "Dey", "Kru", "kru", "Krahn", "krahn", "sarpo", "Sarpo", "sapro"),
                                  "Other Mande" = c("gio", "Gio", "Lorma", "lorma", "mano", "Mano", "Vai", "vai", "mandingo", "Mandingo", "Mende", "mende", "Gbandi", "gbandi"),
                                  "Grebo" = c("grebo", "Grebo"),
                                  "Kpelle" = c("kpelle", "kpelle"),
                                  "Other" = c("Other", "other", "None/Only English", "none/only english", "none / only english", "kissi", "Kissi", "Gola", "gola"),
                                  ))
liberia %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
liberia %>% 
  flat_table(v130rel, year)
liberia %>% 
  flat_table(v130rel, cohort_cat)

liberia %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Christian" = c("christian", "Christian"), 
                                 "Muslim" = c("muslim", "Muslim"), 
                                 "Traditional" = c("traditional religion", "Traditional religion"), 
                                 "Other/no religion" = c("no religion", "other", "No religion", "Other")
                              
                                
  ))

liberia %>% 
  flat_table(religion, cohort_cat)

# write data set
liberia %<>% 
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
df <-  bind_rows(df, liberia)
rm(meta, dfs, liberia)


