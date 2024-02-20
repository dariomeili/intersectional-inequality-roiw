# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/BF"))

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

names(dfs) <- meta %>% pull(file_name)

# harmonize year
dfs[["BFIR21FL"]]$v007year <- 1992
dfs[["BFIR31FL"]]$v007year <- 1999
dfs[["BFMR21FL"]]$v007year <- 1992
dfs[["BFMR31FL"]]$v007year <- 1999

burkina <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "burkina", 
         country_code = "BF")

# gender
burkina %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))
# fix year
burkina %>% 
  frq(v007year)

burkina %<>%
  mutate(year = v007year)

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
burkina %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>% 
  filter(age >= 18)

burkina %>% 
  frq(cohort_cat)
# harmonize ethnicity
burkina %>% 
  flat_table(v131eth, cohort_cat, gender) 

burkina %<>% 
  mutate(ethnicity = fct_collapse(v131eth,
                               "Mossi" = c("Mossi", "mossi"), 
                               "Fulfuldé/Peul" = c("fulfude (peul)", "fulfulde / peul", 
                                            "fulfulde/peul", "fulfuldé / peul", "Fulfuldé / Peul"),
                               "Gurma" = c("gourmantche", "gourmatche", "gourmantché", "gourmatché", "gourmantch‚", "Gourmatché"),
                               "Bobo" = c("Bobo", "bobo"),
                               "Gurunsi" = c("gourounsi", "Gourounsi", "gouroussi"),
                               "Senufo" = c("senoufo", "sénoufo", "s‚noufo", "Sénoufo"),
                               "Lobi" = c("lobi", "Lobi"),
                               "Dagara" = c("dagara", "Dagara"),
                               "Bissa" = c("bissa", "Bissa"), 
                               other_level = "Other"
  ))

burkina %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
burkina %>% 
  flat_table(v130rel, year)
burkina %>% 
  flat_table(v130rel, cohort_cat)

burkina %<>%
  mutate(religion = fct_collapse(v130rel, 
                            "Christian" = c("catholic", "Catholic", "protestant", "Protestant", "christian"),
                            "Muslim" = c("islam", "moslem", "muslim", "moslem", "Muslim", "moslim"), 
                            "Traditionalist/other" = c("traditional",  "traditional / animist", "traditionnal/animist", "Traditionnal/animist", "animist",
                                                       "no religion", "other", "No religion", "No religion", "Other"),
  ))

burkina %>% 
  flat_table(religion, cohort_cat)

# write data set
burkina %<>% 
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
df <-  bind_rows(df, burkina)
rm(meta, dfs, burkina)
