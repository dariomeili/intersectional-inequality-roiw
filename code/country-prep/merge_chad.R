
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/TD"))

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
dfs[["TDIR31FL"]]$v007year <- 1996
dfs[["TDMR31FL"]]$v007year <- 1996

chad <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "chad", 
         country_code = "TD")

chad %>% 
  frq(v007year)

chad %<>%
  mutate(year = v007year)
# gender
chad %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
chad %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>% 
  filter(age >= 18)

chad %>% 
  frq(cohort_cat)
# harmonize ethnicity
chad %>% 
  frq(v131eth)

chad %>% 
  flat_table(v131eth, year)
chad %>% 
  flat_table(v131eth, cohort_cat, gender) 

chad %<>% 
  mutate(ethnicity = fct_collapse(v131eth,
                                  "gorane" = c("gorane"),
                                  "arabic" = c("arab", "arabic", "arabe"),
                                  "kanembou / bornou / boudouma" = c("kanembou / bornou / boudouma" , "kanem-bornou"),
                                  "ouadaï / maba / massalit / mimi" = c("ouadaï / maba / massalit / mimi", "ouaddai", "ouadai"), 
                                  "dadajo / kibet / mouro" = c(), 
                                  "hadjarai" = c("bidio / migami / kenga / dangléat", "hadjarai"),
                                  "sara (ngambaye/sara madjin-gaye/mbaye)" = c("sara (ngambaye/sara madjin-gaye/mbaye)", "sara"),
                                  "marba / lélé / mesmé" = c(),
                                  "other" = c("other chad ethnic (achit/banda/kim)", "other chadien ethnos", "autres ethnies tchadiennes",
                                                          "moundang", "toupouri / kéra", "tama / assongori / mararit", "gabri / kabalaye / nangtchéré / soumraye",
                                                          "mesmedjé / massalat / kadjaksé", "karo / zimé / pévé", "fitri-batha", "lac iro", "tandjile", "kebbi", 
                                                          "other ethnic group", "not elsewhere class.", "mayo kebbi", "baguirmi / barma", "baguirmien", "boulala / médégo / kouka",
                                                          "peul / foulbé / bodoré", "peul", "zaghawa / bideyat / kobé", "dadajo / kibet / mouro", "massa / mousseye / mousgoume",
                                                          "marba / lélé / mesmé", "other ethnic groups of foreign origin (bambara/haoussa/tower)",
                                              "other nationalities", "alien", "etranger", "indéterminé", "indetermined")
                                  )
  ) 

chad %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
chad %>% 
  flat_table(v130rel, year)
chad %>% 
  flat_table(v130rel, cohort_cat)

chad %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Catholic" = c("catholic", "catholique"), 
                                 "Protestant/other christian" = c("protestant", "other chritians", "protestante", "prostestan", "other christian"),
                                 "Muslim" = c("muslim", "muslim/islam", "musulmane/islam", "moslem"),
                                 "Other/no religion" = c("no religion", "sans religion", "animist", "other", "animiste", "autre"), 
  ))

chad %>% 
  flat_table(religion, cohort_cat)

# write data set
chad %<>% 
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
df <-  bind_rows(df, chad)
rm(meta, dfs, chad)


