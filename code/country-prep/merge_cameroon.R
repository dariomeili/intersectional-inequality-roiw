
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/CM"))

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
dfs[["CMIR22FL"]]$v007year <- 1991
dfs[["CMIR31FL"]]$v007year <- 1998
dfs[["CMMR21FL"]]$v007year <- 1991
dfs[["CMMR31FL"]]$v007year <- 1998
cameroon <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "cameroon", 
         country_code = "CM")

cameroon %>% 
  frq(v007year)

cameroon %<>%
  mutate(year = v007year)
# gender
cameroon %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
cameroon %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>%  
  filter(age >= 18)

cameroon %>% 
  frq(cohort_cat)
# harmonize ethnicity
cameroon %>% 
  flat_table(v131eth, year) 
cameroon %>% 
  flat_table(v131eth, cohort_cat, gender)

cameroon %<>% 
  filter(year != 1991) %>% 
  mutate(ethnicity = fct_collapse(v131eth,
                                  "Adamaoua-Oubangui" = c("adamaoua-oubangui", "dourou", "mboum", "koutine", "fali-kangou", "yanguere", "moundang", "toupouri", "bakwa", "dama", 
                                                          "fali", "gbaya", "gbete", "mambay", "mono", "ndai", "niam-niam", "samba"),
                                  "Arab-choa/Peulh/Haoussa/Kanuri" = c("arab-choa/peulh/haoussa/kanuri", "peulh", "arabe choa", "kanuri", "haoussa",
                                                                       "arabe-choa", "bororo", "foulbe"),
                                  "Bamilike/Bamoun" = c("bamilike/bamoun", "bamilike-central", "bamoun", "dschang-cent", "dschang-est", "dschang-oues",
                                                        "dschang-sud", "dschang", "fontem / ngwe", "fefe / nufi-ce", "fefe / nufi-no", "fefe / nufi",
                                                        "ghomala-cent", "ghomala-nord", "ghomala-oues", "ghomala-sud", "megaka / bagam", "nda'nda-est",
                                                        "nda'nda-oues", "nda'nda-sud", "nda'nda", "ngomba", "ngombale", "bamileke", "fontem/ngwe", "fefe/nufi-ce",
                                                        "fefe/nufi-no", "fefe/nufi", "megaka/bagam", "ejagham"),
                                  "Bantoïde South-West" = c("bantoïde south-west", "bafia", "tikar", "bakweri", "balong", "bassossi", "mbo", "oroko", "balom",
                                                            "djanti", "kwakum", "kaka", "pol/pori", "kwassio", "kozime", "maka / makya", "mezime / mpo", "maka/makya", 
                                                            "mezime/mpo", "bafaw", "bakossi", "kenyang", "banyang", "batanga", "bebe", "douala", "mambila", "esimbi", "tiv", "voute", "yassa"),
                                  "Beti/Bassa/Mbam" = c("beti/bassa/mbam" , "beti", "bassa-bakoko", "bamenyan", "mungaka", "medumba", "mbam", "bakoko", "bassa",
                                                        "lombe", "eton", "eki", "fong", "yangben", "yambeta", "banen", "manguissa", "mvele", "sanaga", "yebekolo","yambassa",
                                                        "yezoum", "autre-beti", "boulou", "fang", "mvae", "ntoumou", "bandem", "banen-bandem","boulou-fang", "ewondo"),
                                  "Biu-Mandara" = c("biu-mandara", "wandala /mandara", "wandala/mandara", "bata", "daba", "guidar", "mafa", "margui",
                                                    "mousgoum", "djimi", "goude", "gavar", "mbedam", "guiziga", "mada", "mofou", "molko", "mefele", "mouyang",
                                                    "tchouvok", "zoulgo", "bana", "kapsiki", "moussey", "zime", "podoko", "wandala", "bouwal", "kotoko", "massa", 
                                                    "mbouko", "mouktele"),
                                  "Côtier/Ngoe/Oroko" = c("côtier/ngoe/oroko", "ngoe-oroko", "cotier"),
                                  "Grassfields" = c("grassfields", "ngemba", "doayo", "anyang/denya", "babasi", "noni", "befang", "metta",
                                                    "moghamo/widi", "mundani", "modele", "ambele", "mbu", "menka", "ngie", "ngwaw/ngwo", "oshie/ngishe", "awing", "bafut", 
                                                    "bambili", "mankon", "mundum", "mundum", "mundum", "pinyin", "ring", "aghem", "babanki","babungo", "bafmeng", "bamessing","bamunka",
                                                    "kom", "bansaw / banso", "wimbum", "yamba", "bansaw/banso", "oku/uku", "bum", "menchum", "momo", "nkwen", "oku / uku", "wimbum-yamba"),
                                  "Kako/Meka/Pygmé" = c("kako/meka/pygmé", "pygmee", "kako", "meka"),
                                  "Other" = c("other", "african", "stranger  / other", "foreigner", "dk", "not stated", "africain-voi", "africain-aut", "autre",
                                              "africain", "non-african", "kera", "kwang /kera", "kwang/kera", "sara", "sara / laka", "bendi", "efik-korop", "mbembe", "kobotchi", "nchanti/ncan", 
                                              "nguiembong", "nkonja")
         ),
         ethnicity = fct_drop(ethnicity)
  ) 




cameroon %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
cameroon %>% 
  flat_table(v130rel, year)
cameroon %>% 
  flat_table(v130rel, cohort_cat)

cameroon %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Catholic" = "catholic",
                                 "Protestant" = c("protestant", "prostestant"),
                                 "Other christian" = c("new religions (eglises rebeillees)", "other christian", "other christians"),
                                 "Muslim" = c("muslim", "moslem"), 
                                 "Other/traditional" = c("other", "boudist, hindu", "nature worship", "animist"), 
                                 "No religion" = c("no religion", "none")
  ))

cameroon %>% 
  flat_table(religion, cohort_cat)

# write data set
cameroon %<>% 
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
df <-  bind_rows(df, cameroon)
rm(meta, dfs, cameroon)
