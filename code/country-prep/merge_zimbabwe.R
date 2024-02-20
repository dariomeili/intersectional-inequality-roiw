
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/ZW"))

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
dfs[["ZWIR31FL"]]$v007year <- 1994
dfs[["ZWIR42FL"]]$v007year <- 1999
dfs[["ZWMR31FL"]]$v007year <- 1994
dfs[["ZWMR41FL"]]$v007year <- 1999

zimbabwe <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "zimbabwe", 
         country_code = "ZW")

zimbabwe %>% 
  frq(v007year)

zimbabwe %<>%
  mutate(year = v007year)
# gender
zimbabwe %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
zimbabwe %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>% 
  filter(age >= 18)

zimbabwe %>% 
  frq(cohort_cat)
# harmonize ethnicity
zimbabwe %>% 
  frq(v131eth)
zimbabwe %>% 
  flat_table(v131eth, year)
zimbabwe %>% 
  flat_table(v131eth, cohort_cat, gender) 

zimbabwe %<>% 
  mutate(ethnicity = fct_collapse(v131eth,
                                  "black" = c("black"),
                                  "white" = c("white"), 
                                  "other" = c("coloured", "asian", "other")
  )
  ) 

zimbabwe %>% 
  flat_table(ethnicity, cohort_cat, gender)
# not enough people in other ethnic groups. insert NA

zimbabwe %>% 
  mutate(ethnicity = NA)
#have to split data sets because they are not comparable for religion and only ethnicity for first two rounds
zimbabwe_until1999 <- zimbabwe %>% 
  filter(year <= 1999)
zimbabwe_from2005 <- zimbabwe %>% 
  filter(year>=2005)

# harmonize religion
zimbabwe_until1999 %>% 
  flat_table(v130rel, year)
zimbabwe_until1999 %>% 
  flat_table(v130rel, cohort_cat)

zimbabwe_until1999 %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Christian" = c("christian"), 
                                 "Traditional" = c("traditional", "spiritual"),
                                 "Other" = c("other", "muslim"),
                                 "None" = c("none")
  ))

zimbabwe_until1999$religion <- droplevels(zimbabwe_until1999$religion)

zimbabwe_until1999 %>%
  flat_table(religion, cohort_cat)

zimbabwe_from2005 %>% 
  flat_table(v130rel, year)
zimbabwe_from2005 %>% 
  flat_table(v130rel, cohort_cat)

zimbabwe_from2005 %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Catholic" = c("roman catholic"), 
                                 "Protestant" = c("protestant"),
                                 "Pentecostal" = c("pentecostal"),
                                 "Apostolic sect" = c("apostolic sect"),
                                 "Other" = c("other christian"),
                                 "Traditional" = c("traditional"),
                                 "Other" = c("muslim", "other"),
                                 "None" = c("none")
  ))

zimbabwe_from2005$religion <- droplevels(zimbabwe_from2005$religion)

zimbabwe_from2005 %>%
  flat_table(religion, cohort_cat)

# write data set
zimbabwe_until1999 %<>% 
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

zimbabwe_from2005 %<>% 
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
df <-  bind_rows(df, zimbabwe_until1999, zimbabwe_from2005)
rm(meta, dfs, zimbabwe, zimbabwe_from2005, zimbabwe_until1999)


