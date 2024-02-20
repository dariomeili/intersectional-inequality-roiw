
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/ET"))

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
dfs[["ETIR41FL"]]$v007year <- 2000
dfs[["ETIR51FL"]]$v007year <- 2005
dfs[["ETIR61FL"]]$v007year <- 2011
dfs[["ETIR70FL"]]$v007year <- 2016
dfs[["ETMR41FL"]]$v007year <- 2000
dfs[["ETMR51FL"]]$v007year <- 2005
dfs[["ETMR61FL"]]$v007year <- 2011
dfs[["ETMR70FL"]]$v007year <- 2016
ethiopia <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "ethiopia", 
         country_code = "ET")

ethiopia %>% 
  frq(v007year)

ethiopia %<>%
  mutate(year = v007year)
# gender
ethiopia %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
ethiopia %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>% 
  filter(age >= 18)

ethiopia %>% 
  frq(cohort_cat)
# harmonize ethnicity
ethiopia %>% 
  frq(v131eth) 
ethiopia %>% 
  flat_table(v131eth, year) # no ethnicity for the first round. put NA

ethiopia %<>% 
  filter(!is.na(v131eth)) %>% 
  mutate(v131eth = fct_drop(v131eth))

ethiopia %>% 
  flat_table(v131eth, cohort_cat, gender)

ethiopia %<>% 
  mutate(ethnicity = fct_collapse(v131eth,
                                  "Affar" = c("affar", "affar / adal, danakil, denkel", "saho / shaho, irob"),
                                  "Amhara" = c("amara / gondere, gayente, semen, farte, gojjame, dawunte, wa",
                                               "amhara","amharra", "argobe", "argoba"),
                                  "Guragie" = c("guragie", "gurarie / cheha, ener, enemor, ezaya, gumer, gura, megareb,", "silte", "harari"),
                                  "Oromo" = c("oromo",
                                              "oromo / guji, borena, tulema, kereyu, gelan, lika, weredube,",
                                              "werji / tigrewerji", "werji"),
                                  "Sidama" = c("sidama"),
                                  "Somali" = c("somalie"),
                                  "Tigray" = c("tigraway / tigre",
                                               "tigray (tigraway)",
                                               "tigrie"),
                                  "Welaita" = c("welaita"), 
                                  "Berta" = c("jebelawi / berta, benshangul, wutawut, shogo, undu, meyu, ge", "fadashi", "gebato", "berta", "fedashe"),
                                  "Nuer" = c("nuwer / abigar", "nuwer"),
                                  "Hadiya" = c("hadiya", "mereko / libido"), 
                                  "Kembata" = c("kembata"),
                                  "Highlands"= c("burji / dashi, bambala, alga", "kembata", "alaba", "kebena", "timbaro / kambara", "burji",
                                                 "gedeo", "qebena", "timebaro", "gedeo / derassa"),
                                  "Gamo" = c("gamo"),
                                  "Ometo-Gimira/Basketo" = c("basketo / mesketo, anjila, dolo, tama", "bench", "she", "mer", "koyra / amaro, amaricho, kore, baditu",
                                                     "malie", "kechem / kacheno, gatsam, haruro", "oyda", "dorzie", "goffa / genu",
                                                     "konta", "kulo / omete, domete, dawuro", "zeysie", "basketo", "dawuro", "goffa", "kore", "zeyese"),
                                  "Agew" = c("agew-awinigi / fonfele", "agew-kamyr / kamtang", "agew-awi", "agew hamyra"),
                                  "Gumuz" = c("gumuz", "gumuz / ganza, ganzo, bega, baga, shankila, say, sese"),
                                  "Kefficho" = c("kefficho", "keffa / kefficho"),
                                  "Other" = c("anyiwak / yembo", "arborie / erbore, ulde, murle", "ari / bako, ara, shangamo",
                                              "hamer / bashada, bana, karo", "harerri / adere", "mocha / shekicho", "koma / komo, hayahaya, medin, akuwma",
                                              "konso / karate, komso, gerate", "bodi", "mao / anfilo, gewami, bambishi, koman",
                                              "messengo / mejenger, majang, ujang", "mursi / mun, mursu, murdi, murzu, nyicalabong", "nao / naho, tolo",
                                              "nyangatom / turkana, bume, men, bum, rogegeno, tobola", "shako", "shinasha", "shita / sita, lango", "suri / surma, tirma, dama, zilmam, chima, murle",
                                              "yemsa / yem, janjaro, yangaro", "other ethiopian national groups", "from different parents", "eritreans",
                                              "djebutians", "sudanese", "other foreigners", "anyiwak", "ari", "arborie",
                                              "bodi","burji", "bena", "dasenech", "derashe", "dizi", "donga", "guagu", "hamer", "komo", "koyego", "konso",
                                              "karo", "mao","me'enite", "mejenger","mossiye", "mursi", "murle", "nyangatom",
                                              "shekecho", "sheko", "shinasha", "upo", "yem", "other ethiopian ethinic group", "missing", "other ethiopian ethnic group", "other", "dasenech / geleb, gelaba, marle", "dizi / maji",
                                              "gewada / kule, gobeze, werzie, alie", "me'en / meken, daim, tishana, me'ent, manit, shuro", "shinasha / boro, shencho, dengebo",
                                              "tsamay / tsemay, dume, kuwele", "gidole / darashat, dirasha, gardula, draytat, durate")
         )
  ) 

ethiopia %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
ethiopia %>% 
  flat_table(v130rel, year)
ethiopia %>% 
  flat_table(v130rel, cohort_cat)

ethiopia %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Orthodox" = "orthodox",
                                 "Catholic/protestant" = c("catholic","protestant"),
                                 "Muslim" = c("moslem", "muslim", "muslim", "muslin"), 
                                 "Other/traditional" = c("other", "traditional")
  ))

ethiopia %>% 
  flat_table(religion, cohort_cat)

# write data set
ethiopia %<>% 
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
df <-  bind_rows(df, ethiopia)
rm(meta, dfs, ethiopia)
