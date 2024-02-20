
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/ML"))

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
dfs[["MLIR32FL"]] %<>% mutate(v007year = case_when(
  v007year == 95 ~ 1995, 
  v007year == 96 ~ 1996
))
  
dfs[["MLMR31FL"]]%<>% mutate(v007year = case_when(
  v007year == 95 ~ 1995, 
  v007year == 96 ~ 1996
))


mali <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "mali", 
         country_code = "ML")

mali %>% 
  frq(v007year)

mali %<>%
  mutate(year = v007year)
# gender
mali %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
mali %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>% 
  filter(age >= 18)

mali %>% 
  frq(cohort_cat)
# harmonize ethnicity
mali %>% 
  frq(v131eth) 
mali %>% 
  flat_table(v131eth, cohort_cat, gender) 

mali %<>% 
  mutate(ethnicity = fct_collapse(v131eth,
                                  "bambara" = c("bambara", "bambara." , "Bambara."), 
                                  "malinke" = c("malinke", "malinké" , "Malinke", "malink‚"),
                                  "sonrai" = c("sonrai",  "sonraï" , "Sonraï", "sonra‹"),
                                  "senoufo/minianka" = c("senoufo/minianka" ,  "senoufo/minianka", "sénoufo/minianka", "Sénoufo/minianka", "s‚noufo /minianka", "s‚noufo/minianka"),
                                  "sarkole/soninke/marka"  = c("sarakole/soninke/marka",  "sarkolé/soninké/marka", "Sarakole/soninke/marka",  "sarakol‚ /sonink‚", "sarakol‚/sonink‚/marka"),
                                  "bobo" = c("bobo", "Bobo"),
                                  "tamacheck" = c("tamacheck", "tamachek/bélla", "tanachek", "Tamachek/bélla", "touareg/bélla"),
                                  "peulh" = c("peulh" , "Peulh"),
                                  "dogon" = c("dogon" , "Dogon"),
                                  "other" = c("cdeao country" , "other nationalities", "ecowas countries", "other", "foreigner", "other african countries", "other african country", "other countries"  , "other ethny", "CDEAO Country" , "Other African Country", "Other nationalities", "Other", "other malian")
  )
  ) 

mali %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
mali %>% 
  flat_table(v130rel, year)
mali %>% 
  flat_table(v130rel, cohort_cat)

mali %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Catholic" = c("catholic", "Catholic"), 
                                 "Evangelic/protestant" = c("Methodist", "protestant", "evangelic", "evangelical", "methodist", "Evangelical"),
                                 "Other christian" = c("christian", "christrian", "other christian", "other christian religion", "Other Christian"),
                                 "Muslim" = c("muslim"),
                                 "Traditional" = c("animist", "Animist"),
                                 "Other/no religion" = c("no religion", "other", "No religion", "Other") 
  ))

mali %>% 
  flat_table(religion, cohort_cat)

# write data set
mali %<>% 
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
df <-  bind_rows(df, mali)
rm(meta, dfs, mali)


