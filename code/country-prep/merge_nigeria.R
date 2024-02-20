
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/NG"))

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
dfs[["NGIR41FL"]]$v007year <- 1999
dfs[["NGMR41FL"]]$v007year <- 1999

nigeria <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "nigeria", 
         country_code = "NG")

nigeria %>% 
  frq(v007year)

nigeria %<>%
  mutate(year = v007year)
# gender
nigeria %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
nigeria %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>%
  filter(age >= 18)

nigeria %>% 
  frq(cohort_cat)
# harmonize ethnicity
nigeria %>% 
  frq(v131eth) 
nigeria %<>% 
  mutate(v131eth = droplevels(v131eth))

nigeria %>% 
  flat_table(v131eth, cohort_cat, gender) 
nigeria %<>% 
  mutate(ethnicity = fct_collapse(v131eth,
                     "Ekoi" = c("ekoi"), 
                     "Fulani" = c("fulani", "Fulani", "fulfulde", "Fulfulde"), 
                     "Hausa" = c("hausa", "Hausa"), 
                     "Ibibio/Efik/Anaang" = c("ibibio", "Ibibio", "Efik", "efik", "Annang", "annang"),
                     "Igala" = c("igala", "Igala"), 
                     "Igbo" = c("igbo", "igbo/ibo", "Igbo/Ibo", "delta ibo", "Delta Ibo"), 
                     "Ijaw/Izon" = c("ijaw/ izon", "ijaw/izon", "Ijaw/Izon"), 
                     "Kanuri/Beriberi" = c("kanuri/ beriberi", "kanuri/beriberi"), 
                     "Tiv" = c("tiv", "Tiv"), 
                     "Yoruba" = c("yoruba", "Yoruba"), 
                     "Urhobo et. al" = c("urhobo", "Urhobo", "Isoko", "isoko"),
                     other_level = "Other"
                     
  )
  ) 

nigeria %>% 
  flat_table(ethnicity, cohort_cat, gender)
# harmonize religion
nigeria %>% 
  flat_table(v130rel, year)
nigeria %>% 
  flat_table(v130rel, cohort_cat)

nigeria %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Catholic" = c("catholic", "Catholic"), 
                                 "Protestant/other christian" = c("protestant", "other christian", "Other Christian"),
                                 "Muslim" = c("islam", "islan", "Islam"),
                                 "Traditional" = c("traditionalist", "Tradicionalist"),
                                 "Other" = c("other","Other"), 
  ))

nigeria %>% 
  flat_table(religion, cohort_cat)

# write data set
nigeria %<>% 
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
df <-  bind_rows(df, nigeria)
rm(meta, dfs, nigeria)


