# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/DR"))

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

dfs <- list("DRIR4AFL"=dfs$DRIR4AFL, "DRMR4AF"=dfs$DRMR4AFL, "DRIR61FL"=dfs$DRIR61FL, "DRMR61FL"=dfs$DRMR61FL, "DRIR6AFL"=dfs$DRIR6AFL, "DRMR6AFL"=dfs$DRMR6AFL)


dominican_republic <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "dominican_republic", 
         country_code = "DR")

dominican_republic %>% 
  frq(v007year)

dominican_republic %<>%
  mutate(year = v007year)
# gender
dominican_republic %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
dominican_republic %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(cohort < 1965 ~ "-1964",
                                cohort >= 1965 & cohort < 1975 ~ "1965-1974", 
                                cohort >= 1975 & cohort < 1985 ~ "1975-1984",
                                cohort >= 1985  ~ "1985-"),
         cohort_cat = fct_relevel(cohort_cat, "-1964", "1965-1974", "1975-1984", "1985-")) %>% 
  filter(age >= 18)

dominican_republic %>% 
  frq(cohort_cat)
# harmonize ethnicity
dominican_republic %>% 
  frq(v131eth) 
dominican_republic %>% 
  flat_table(v131eth, year) # no ethnicity

dominican_republic %<>% 
  mutate(ethnicity = as_factor(v131eth)) 

# harmonize religion
dominican_republic %>% 
  flat_table(v130rel, year)
dominican_republic %>% 
  flat_table(v130rel, cohort_cat)

dominican_republic %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Catholic" = c("catolica", "catholic"),
                                 "Evangelic" = c("evangelic", "adventist", "evangelica", "adventista"),
                                 "No religion/other" = c("no religion", "none", "no tiene", "other", "don't know", "otra", "no sabe")
  ))

dominican_republic %>% 
  flat_table(religion, cohort_cat)

# write data set
dominican_republic %<>% 
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
df <-  bind_rows(df, dominican_republic)
rm(meta, dfs, dominican_republic)


