# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/BD"))

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
dfs[["BDIR31FL"]]$v007year <- 1994
dfs[["BDIR3AFL"]]$v007year <- 1996
dfs[["BDIR41FL"]]$v007year <- 2000
dfs[["BDIR51FL"]]$v007year <- 2007
dfs[["BDMR31FL"]]$v007year <- 1994
dfs[["BDMR3AFL"]]$v007year <- 1996
dfs[["BDMR41FL"]]$v007year <- 2000
dfs[["BDMR51FL"]]$v007year <- 2007

#dfs <- list("AMIR42FL"=dfs[["AMIR42FL"]], "AMMR41FL"=dfs[["AMMR41FL"]]) # no ethnicity and religion in other years
bangladesh <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "bangladesh", 
         country_code = "BD")

# gender
bangladesh %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))
# fix year
bangladesh %>% 
  frq(v007year)

bangladesh %<>%
  mutate(year = v007year)

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
bangladesh %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(cohort < 1965 ~ "-1964",
                                cohort >= 1965 & cohort < 1975 ~ "1965-1974", 
                                cohort >= 1975 & cohort < 1985 ~ "1975-1984",
                                cohort >= 1985  ~ "1985-"),
         cohort_cat = fct_relevel(cohort_cat, "-1964", "1965-1974", "1975-1984", "1985-")) %>% 
  filter(age >= 18)

# harmonize ethnicity
bangladesh %>% 
  frq(v131eth) # no ethnicity
bangladesh %<>% 
  mutate(ethnicity = as_factor(v131eth))
#bangladesh %>% 
#  flat_table(ethnicity, cohort_cat)
# harmonize religion
bangladesh %>% 
  flat_table(v130rel, year)
bangladesh %>% 
  flat_table(v130rel, cohort_cat)

bangladesh %<>% 
  mutate(religion = fct_collapse(v130rel,
                                 "muslim" = "islam",
                                 "hindu" = "hinduism", 
                                 other_level = "other"))

bangladesh %>% 
  flat_table(religion, cohort_cat)

# write data set
bangladesh %<>% 
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
df <-  bind_rows(df, bangladesh)
rm(meta, dfs, bangladesh)
