
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/MZ"))

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
dfs[["MZIR31FL"]]$v007year <- 1997
dfs[["MZMR31FL"]]$v007year <- 1997

mozambique <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "mozambique", 
         country_code = "MZ")

mozambique %>% 
  frq(v007year)

mozambique %<>%
  mutate(year = v007year)
# gender
mozambique %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
mozambique %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>% 
  filter(age >= 18)

mozambique %>% 
  frq(cohort_cat)
# harmonize ethnicity
mozambique %>% 
  frq(v131eth) 
mozambique %>% 
  flat_table(v131eth, year) 
mozambique %>% 
  flat_table(v131eth, cohort_cat, gender) 

mozambique %<>% 
  mutate(ethnicity = fct_collapse(v131eth,                               
                                  "Makhuwa" = c("emakhuwa", "emakua & simili", "coti", "emakua", "Emakhuwa", "Coti", "echuwabo", "Echuwabo"),
                                  "Portuguese" =  c("portugu?s","portugues", "Portugu?s"),
                                  "Changana" = c("xichangana", "Xichangana"),
                                  "Chewa" = c("cicewa", "Cicewa"),
                                  "Sena" = c("cisena", "cisena & simili", "cibalke", "Cisena", "Cibalke", "Cinyungwe", "cinyungwe"),
                                  "Lomwe" = c("elomue & emarenjo", "elomwe", "Elomwe", "elomue"),
                                  "Tswa/Rhonga" =  c( "xirhonga","xitsonga & simili", "Xirhonga", "xitswa & simili", "xitswa", "Xitswa"),
                                  "Shona/Ndau" = c("chitewe","shona", "Shona", "Chitewe", "cindau", "Cindau"),
                                  "Chopi/Tonga" = c("Cichopi", "cichopi", "Bitonga", "bitonga"),
                                  "Other" =  c("missing", "other", "outra", "kimwane", "Kimwane", "Outra", "Ciyao", "ciyao", "shimakonde", "Shimakonde")
  )
  ) 

mozambique %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
mozambique %>% 
  flat_table(v130rel, year)
mozambique %>% 
  flat_table(v130rel, cohort_cat)

mozambique %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Catholic" = c("catholic (cat¢lica)", "catholic", "Catholic"), 
                                 "Protestant/other christian" = c("protestant (protestante)", "protestant / evangelic", 
                                                                  "evangelical/pentecostal", "anglican", "protestant", "Envagelic/petencostal",
                                                                  "Anglican", "Protestant"),
                                 "African Zionist" = c("zionist", "zion", "Zion/si?o"),
                                 "Muslim" = c("islamic (mu‡ulman)", "moslem", "islamic", "Islamic"),
                                 "Other" = c("other (outra)", "animist", "Other", "other"), 
                                 "No Religion" = c("no religion (sem religiao)", "no religion", "No religion")
  ))

mozambique %>% 
  flat_table(religion, cohort_cat)

# write data set
mozambique %<>% 
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
df <-  bind_rows(df, mozambique)
rm(meta, dfs, mozambique)


