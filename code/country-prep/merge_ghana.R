
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/GH"))

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
dfs[["GHIR31FL"]]$v007year <- 1993
dfs[["GHIR41FL"]]$v007year <- 1998
dfs[["GHMR31FL"]]$v007year <- 1993
dfs[["GHMR41FL"]]$v007year <- 1998
ghana <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "ghana", 
         country_code = "GH")

ghana %>% 
  frq(v007year)

ghana %<>%
  mutate(year = v007year)
# gender
ghana %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
ghana %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>% 
  filter(age >= 18)

ghana %>% 
  frq(cohort_cat)
# harmonize ethnicity
ghana %>% 
  frq(v131eth) 
ghana %>% 
  flat_table(v131eth, year) # no ethnicity for the first round. put NA

ghana %>% 
  flat_table(v131eth, cohort_cat, gender)
ghana %<>% 
  mutate(ethnicity = fct_collapse(v131eth,
                                  "akan" = c("akwapim",  "akwapin", "asante" , "fante", "fanti", "other akan", "akan"),
                                  "ga/dangme" = c("ga.adangbe", "ga/adangbe", "ga/dangme", "ga /adangbe"), 
                                  "ewe" = c("ewe"), 
                                  "mole-dagbani" = c("mole-dagbani"), 
                                  "gruma" = c("gruma", "gurma"),
                                  "grussi" = c("grusi", "grussi"), 
                                  "guan" = c("guan"),
                                  "other" = c("hausa", "mande", "central togo tribes", 
                                              "other ghanaian tr.", "other non-gh.tribes", "dagarti")
  ))

ghana %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
ghana %>% 
  flat_table(v130rel, year)
ghana %>% 
  flat_table(v130rel, cohort_cat)

ghana %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Other/no religion" = c("no religion", "other", "no religion + other"),
                                 "Evangelical/protestant" = c("protestant", "methodist", "anglican", "presbyterian", "pentecostal/charismatic", "pentecostal"),
                                 "Catholic" = c("catholic"),
                                 "Other christian" = c("other christian"), 
                                 "Muslim" = c("muslim", "moslem", "islam"), 
                                 "Traditional" = c("traditional", "spiritualist", "traditional/spiritualist")
                                 
                                 
  ))

ghana %>% 
  flat_table(religion, cohort_cat)

# write data set
ghana %<>% 
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
df <-  bind_rows(df, ghana)
rm(meta, dfs, ghana)
