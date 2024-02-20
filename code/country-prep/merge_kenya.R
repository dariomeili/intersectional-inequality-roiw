
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/KE"))

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
dfs[["KEIR33FL"]]$v007year <- 1999
dfs[["KEIR3AFL"]]$v007year <- 1998
dfs[["KEMR32FL"]]$v007year <- 1999
dfs[["KEMR3AFL"]]$v007year <- 1998

kenya <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "kenya", 
         country_code = "KE")

kenya %>% 
  frq(v007year)

kenya %<>%
  mutate(year = v007year)
# gender
kenya %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
kenya %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>%  
  filter(age >= 18)

kenya %>% 
  frq(cohort_cat)
# harmonize ethnicity
kenya %>% 
  frq(v131eth) 
kenya %>% 
  flat_table(v131eth, cohort_cat, gender) 

kenya %<>% 
  mutate(ethnicity = fct_collapse(v131eth,
                                  "kikuyu" = c("kikuyu", "kikuya"),
                                  "meru/embu" = c("meru/embu", "meru /embu", "meru", "embu", "mbere"),
                                  "taita/taveta" = c("taita/taveta", "taita /taveta", "taita/ taveta", "taita/tavate"),
                                  "mijikenda/swahili" = c("mijikenda/swahili" , "mijikenda /swahili", "mijikenda/ swahili"),
                                  "maasai" = c("masai", "maasai"),
                                  "oromo/gabbra/borana" = c("orma", "gabbra", "boran"),
                                  "other" = c("rendille", "iteso", "pokomo", "kuria", "samburu"), 
  )) 

kenya %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
kenya %>% 
  flat_table(v130rel, year)
kenya %>% 
  flat_table(v130rel, cohort_cat)

kenya %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Catholic" = c("catholic", "roman catholic"), 
                                 "Protestant/other christian" = c("protest /oth cristian", 
                                                                  "protestant/ other christian", 
                                                                  "protestant/other christian",
                                                                  "protestant /oth. chri"),
                                 "Muslim" = c("muslim"),
                                 "Other/no religion" = c("other", "other religion", "", "no religion"), 
  ))

kenya %>% 
  flat_table(religion, cohort_cat)

# write data set
kenya %<>% 
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
df <-  bind_rows(df, kenya)
rm(meta, dfs, kenya)


