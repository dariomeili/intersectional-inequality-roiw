# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/BJ"))

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
dfs[["BJIR31FL"]]$v007year <- 1996
dfs[["BJMR31FL"]]$v007year <- 1996
dfs[["BJMR51FL"]]$v007year <- 2006

benin <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "benin", 
         country_code = "BJ")

# gender
benin %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))
# fix year
benin %>% 
  frq(v007year)

benin %<>%
  mutate(year = v007year)

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
benin %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>% 
  filter(age >= 18)

benin %>% 
  frq(cohort_cat)
# harmonize ethnicity
benin %>% 
  flat_table(v131eth, cohort_cat, gender) 

benin %<>% 
  mutate(ethnicity = fct_collapse(v131eth,
                                  "Adja" = c("adja", "adja & related", "adja and related", "Adja"),
                                  "Bariba" = c("bariba", "bariba & related", "bariba and related", "Bariba"),
                                  "Betamaribe" = c("betamaribe", "betamaribe & related", "bétamaribe and related", "Betamaribe", "bariba and related..", "betamaribe and related", "bétamaribe"),
                                  "Dendi" = c("dendi", "dendi & related", "dendi and related", "Dendi", "dendi and related.."),
                                  "Fon" = c("fon", "fon & related", "fon and related", "Fon", "fon and related.."),
                                  "Peulh"= c("peulh", "peulh & related", "peulh and related", "Peulh"),
                                  "Yoa/Lokpa" = c("yoa" , "yoa & lokpa", "yoa, lokpa & related" , "yoa and lokpa and related" , "Yoa", "yoa, lokpa and related.", "yoa et lokpa"),
                                  "Yoruba" = c("yoruba" , "yoruba & related", "yoruba and related", "Yoruba", "yoruba and related.."), 
                                  other_level = "Other"
  ))

benin %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
benin %>% 
  flat_table(v130rel, year)
benin %>% 
  flat_table(v130rel, cohort_cat)

benin %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Catholic" = c("Catholic",
                                                "catholic",
                                                "catholique"),
                                 "Protestant/other christian" = c("celestes",
                                                                  '"celestes"',
                                                                  "celestes (celestial church of christ)",
                                                                  "methodist",
                                                                  "other christian",
                                                                  "other christians",
                                                                  "other protestant",
                                                                  "other protestants",
                                                                  "protestant",
                                                                  "protestant methodist",
                                                                  "protestants",
                                                                  "Methodist",
                                                                  "Other Protestant",
                                                                  "Celestes (Celestial Church of Christ)",
                                                                  "Other Christian",
                                                                  "protestante methodiste",
                                                                  "autres protestants",
                                                                  "autres chretiens"),
                                 "Muslim" = c("islam",
                                              "islamic",
                                              "moslem",
                                              "muslim",
                                              "moslem",
                                              "Islam"), 
                                 "Traditional" = c("other traditional", 
                                                   "taditional",
                                                   "traditional",
                                                   "traditional (vodoun)",
                                                   "vodoun",
                                                   "Vodoun",
                                                   "Other traditional",
                                                   "other traditional religions",
                                                   "autre traditionnelles"),
                                 "Other" = c("no religion",
                                             "none",
                                             "other religion",
                                             "other religions",
                                             "other",
                                             "Other religion",
                                             "No religion",
                                             "autres religions",
                                             "aucune",
                                             "autre")
  ))

benin %>% 
  flat_table(religion, cohort_cat)

# write data set
benin %<>% 
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
df <-  bind_rows(df, benin)
rm(meta, dfs, benin)
