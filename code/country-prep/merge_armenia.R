# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/AM"))

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
dfs <- list("AMIR42FL"=dfs[["AMIR42FL"]], "AMMR41FL"=dfs[["AMMR41FL"]]) # no ethnicity and religion in other years
armenia <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "armenia", 
         country_code = "AM")

# gender
armenia %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))
# fix year
armenia %>% 
  frq(v007year)

armenia %<>%
  mutate(year = 2000)

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
armenia %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort > 1970 & cohort <= 1980 ~ "1971-1980",
           cohort > 1980 ~ "1981-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1971-1980", "1981-")) %>% 
  filter(age >= 18)

# harmonize ethnicity
armenia %>% 
  flat_table(v131eth, cohort_cat, gender) 
armenia %<>% 
  mutate(ethnicity = fct_collapse(v131eth,
                                "armenian" = "armenian",
                                other_level = "other"))
armenia %>% 
  flat_table(ethnicity, cohort_cat)
# cannot use armenia for ethnicity (drop) bc of too few obs
armenia %<>% 
  mutate(ethnicity = NA)
# harmonize religion
armenia %>% 
  flat_table(v130rel, year)
armenia %>% 
  flat_table(v130rel, cohort_cat)

armenia %<>% 
  mutate(religion = fct_collapse(v130rel,
                                 "christian" = "christian",
                                 other_level = "other"))

armenia %>% 
  flat_table(religion, cohort_cat)

# write data set
armenia%<>% 
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
df <-  bind_rows(df, armenia)
rm(meta, dfs, armenia)
