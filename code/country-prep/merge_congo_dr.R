# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/CD"))

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
dfs[["CDIR50FL"]]$v007year <- 2007
dfs[["CDMR50FL"]]$v007year <- 2007

congo_dr <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "congo_dr", 
         country_code = "CD")

# gender
congo_dr %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))
# fix year
congo_dr %>% 
  frq(v007year)

congo_dr %<>%
  mutate(year = v007year)

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
congo_dr %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>% 
  filter(age >= 18)

congo_dr %>% 
  frq(cohort_cat)
# harmonize ethnicity
congo_dr %>% 
  flat_table(v131eth, cohort_cat, gender) 

congo_dr %<>% 
  mutate(ethnicity = fct_collapse(v131eth,
                                  "bakongo" = c("bakongo nord & sud", "bakongo north and south"),
                                  "bas-kasai and kwilu-kwngo" = c("bas-kasai and kwilu-kwngo", "bas-kasai et kwilu-kwngo"),
                                  "basele-komo, maniema, kivu" = c("basele-k , man. and kivu",  "basele-k , man. et kivu"), 
                                  "uele lac albert" = c("uele lac albert",  "uele lake albert"),
                                  "ubangi and itimbiri" = c( "ubangi et itimbiri", "ubangi and itimbiri"), 
                                  "cuvette central" = c("cuvette central"),
                                  "kasai, katanga, tanganika" = c("kasai, katanga, tanganika"), 
                                  "other" = c("other", "others", "foreign/non-congolese", "pygmy", "lunda")
  ))

congo_dr %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
congo_dr %>% 
  flat_table(v130rel, year)
congo_dr %>% 
  flat_table(v130rel, cohort_cat)

congo_dr %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Catholic" = c("catholic"),
                                 "Protestant/other christian" = c("armée de salut", 
                                                                  "kimbanguist",
                                                                  "kimbanguiste",
                                                                  "other christian",
                                                                  "other christians",
                                                                  "protestant",
                                                                  "salvation army"),
                                 "Muslim" = c("muslim"), 
                                 "Other" = c("no religion", "bundu dia kongo", "other", "vuvamu", "animist")
  ))

congo_dr %>% 
  flat_table(religion, cohort_cat)

# write data set
congo_dr %<>% 
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
df <-  bind_rows(df, congo_dr)
rm(meta, dfs, congo_dr)
