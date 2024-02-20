
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/SN"))

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
  map(~sjlabelled::as_factor(., v130rel))

meta

names(dfs) <- meta %>% pull(file_name)

# harmonize year
dfs
dfs[["SNIR21FL"]]$v007year <- 1993
dfs[["SNIR32FL"]]$v007year <- 1997
dfs[["SNMR21FL"]]$v007year <- 1993
dfs[["SNMR31FL"]]$v007year <- 1997

senegal <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "senegal", 
         country_code = "SN")

senegal %>% 
  frq(v007year)

senegal %<>%
  mutate(year = v007year)
# gender
senegal %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
senegal %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>%
  filter(age >= 18)

senegal %>% 
  frq(cohort_cat)
# harmonize ethnicity
senegal %>% 
  frq(v131eth) 
senegal %>% 
  flat_table(v131eth, year)
senegal %>% 
  flat_table(v131eth, cohort_cat, gender) 

senegal %<>% 
  mutate(ethnicity = fct_collapse(v131eth,
                                  "Diola" = c("Diola" , "diola" , "diola"),
                                  "Soninke" = c("Soninke", "soninké", "sarakole /soninke", "sarakhol‚, soninke", "soninke", "sarakole/soninke", "Soninké"),
                                  "Wolof" = c("Wolof" , "wolof" , "wolof /lebou", "wolof/lebou"),
                                  "Mandingue" = c("mandingue", "Mandingue" , "mandingue /soce/malnk" , "Mandingue", "bambara", "mandingue/ socé", "mandingue/soce/malnk"),
                                  "Serer" = c("Serer", "serer"),
                                  "Poular" = c("Poular" , "poular"),
                                  "Other" = c("Other", "other","balant", "mancagne", "manjaak", "majaak", 
                                              "Not a Senegalese" , "not a senegalese" , "not senegalese" , "other senegalese" ,
                                              "other,non-senegalese", "other (senegalese)", "other (non-senegalese)", "other senegalese ethnicity")
  )
  ) 


senegal %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
senegal %>% 
  flat_table(v130rel, year)
senegal %>% 
  flat_table(v130rel, cohort_cat)

senegal %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Christian" = c("christian", "Christian", "chistiane", "chistian"), 
                                 "Muslim" = c("muslim", "Muslim"),
                                 "Other" = c("animist", "Animist", "animiste","no religion", "No religion", "Other", "other"), 
  ))

senegal %>% 
  flat_table(religion, cohort_cat)

# write data set
senegal %<>% 
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
df <-  bind_rows(df, senegal)
rm(meta, dfs, senegal)


