# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/CG"))

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
dfs[["CGIR51FL"]]$v007year <- 2005
dfs[["CGMR51FL"]]$v007year <- 2005

congo_rep <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "congo, republic", 
         country_code = "CG")

# gender
congo_rep %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))
# fix year
congo_rep %>% 
  frq(v007year)

congo_rep %<>%
  mutate(year = v007year)

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
congo_rep %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>%
  filter(age >= 18)

congo_rep %>% 
  frq(cohort_cat)
# harmonize ethnicity
congo_rep %<>% 
  mutate(v131eth = fct_drop(v131eth))
congo_rep %>% 
  flat_table(v131eth, cohort_cat, gender) 

congo_rep %<>% 
  mutate(ethnicity = droplevels(fct_collapse(v131eth,
                                  "Other Congolese" = c("akwa","babwisi", "bahangala", "bahumbu", "bakaya", "bakotas",
                                              "bambama", "bandasa", "bandja", "bandzabi", "bangélé", 
                                              "bobangui", "bokita", "bomasa", "bomitaba", "bomwali", "bondongo", "bonga", "bongili", "bwéni",
                                              "congolais, ethnie non déclaré", "duma", "isongo", "kabinda", "kaka",
                                              "bongili", "mayanga", "mboko", "mbondjo", "minkengué", "moyi", "muzombo", "ngundi", "other",
                                              "pomo", "ngaré", "baboma", "bambamba", "likuba", "kota", "bakuni", "balumbu", "bayélé", "punu", "pygmée", 
                                              "autres kongo", "badondo", "bakamba", "basundi", "basundi-kimongo", "bavili", "bayombé"),
                                  "Other non-Congolese" = c("afrique centrale", "afrique de l'ouest", "afrique orientale et australe", "autres pays africains sans sp‚cifier",
                                                            "etranger", "europe et océanie"),
                                  "Teke" = c("autres batékés", "batéké", "batéké-alima", "téké", "bangangoulou", "ndziku", "bakukuya", "bapunu", "balali", "batsangui"),
                                  "Balari" = "balari",
                                  "Kongo" = c("kongo", "bakongo"),
                                  "Other Kongo" = c("autres kongo", "badondo", "bakamba", "basundi", "basundi-kimongo", "bavili", "bayombé", "babémbé"),
                                  "Mbeti" = c("autres mbétis", "mbéré", "mbéré/mbéti/kélé"),
                                  "Mbohhi" = c("autres mbosi", "mbochi", "mbosi"),
                                  "Sangha" = c("autres sangha-likwala", "sangha"),
                                  "Ubangi" = c("bangala", "makaa", "koyo", "bonga", "oubanguiens", "likwala", "autres makaa-djem")
  )))

congo_rep %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
congo_rep %>% 
  flat_table(v130rel, year)
congo_rep %>% 
  flat_table(v130rel, cohort_cat)

congo_rep %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Catholic" = c("catholic", "catholique"),
                                 "Protestant" = "protestant",
                                 "Other christian" = c("adventist/jehova", "adventiste/jehova", "arm‚e du salut", "eglise de réveil",
                                                                   "salvation army"),
                                 "Autochtonous religions" = c("kibanguist", "kimbanguiste", "zephirrin/matsouanist/ngunza", "zephirin/matsouaniste/ngunza"),
                                 "Other" = c("animist", "animiste", "other", "muslim", "musulman"), 
                                 "No religion" = c("no religion", "none")
  ))

congo_rep %>% 
  flat_table(religion, cohort_cat)

# write data set
congo_rep %<>% 
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
df <-  bind_rows(df, congo_rep)
rm(meta, dfs, congo_rep)

