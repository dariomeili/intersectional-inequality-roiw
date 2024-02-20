
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/UG"))

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
dfs[["UGIR33FL"]]$v007year <- 1995
dfs[["UGMR33FL"]]$v007year <- 1995

dfs[["UGIR41FL"]]$v131eth <- as_factor(dfs[["UGIR41FL"]]$v131eth)
dfs[["UGIR52FL"]]$v131eth <- as_factor(dfs[["UGIR52FL"]]$v131eth)
dfs[["UGMR41FL"]]$v131eth <- as_factor(dfs[["UGMR41FL"]]$v131eth)
dfs[["UGMR52FL"]]$v131eth <- as_factor(dfs[["UGMR52FL"]]$v131eth)

uganda <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "uganda", 
         country_code = "UG")

uganda %>% 
  frq(v007year)

uganda %<>%
  mutate(year = v007year)
# gender
uganda %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))

# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
uganda %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>%
  filter(age >= 18)

uganda %>% 
  frq(cohort_cat)
# harmonize ethnicity
uganda %>% 
  frq(v131eth)
uganda %>% 
  flat_table(v131eth, year)
uganda %>% 
  flat_table(v131eth, cohort_cat, gender) 

uganda %<>% 
  mutate(ethnicity = fct_collapse(v131eth,
                                  "alur-acholi" = c("alur", "jonam", "acholi", "ethur", "bachope", "chope"), 
                                  "baganda" = "baganda", 
                                  "bagisu" = c("bagisu", "mugishu"),
                                  "chiga" = c("bakiga", "bahororo"), 
                                  "banyankore" = c("banyankole", "banyankore", "banyaruguru"), 
                                  "Ruanda-Rundi" = c("banyarwanda", "munyarwanda", "bafumbira", "mufumbira", "barundi", "batwa"),
                                  "masaba-luhya" = c("banyole", "bagwe", "samia", "basamia", "babukusu", "banyara"),
                                  "banyoro" = c("banyoro", "munyoro", "batagwenda"),
                                  "basoga" = c("basoga"), 
                                  "batoro" = c("batoro", "mutooro", "batuku"), 
                                  "teso-turakana" = c("iteso", "atesa", "mening", "karimojong", "ngakaramajong", "dodoth", "jie" ,"napore"),
                                  "langi" = c("langi", "lango"),
                                  "moru-madi" = c("lugbara", "madi", "aringa"),
                                  "Other Nyoro-Ganda" = c("barulli", "bagwere", "mugwere", "baruli", "babwisi", "bakenyi"),
                                  "other" = c("other", "badama", "jopadhola", 
                                              "bakonjo", "bakonzo", "mukonjo", "banyabindi", "banyabutumbi", "baamba", "kumam", 
                                              "lendu", "nubiam", "sebei", "aliba","bagungu", "bahehe", "basongora", "gimara", 
                                              "ik (teuso)", "kebu (okebu)", "mvuba", "ngikutio", "nubi", "nyangia", "pokot", "reli",
                                              "sabiny", "shana", "so (tepeth)", "vonoma", "kakwa", "kuku")
                                  
                                  )
         ) 

uganda %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
uganda %>% 
  flat_table(v130rel, year)
uganda %>% 
  flat_table(v130rel, cohort_cat)

uganda %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "Catholic" = c("catholic"), 
                                 "Protestant/other christian" = c("protestant", "seventh day advent.", "pentecostal",
                                                                  "sda", "anglican", "seventh day adventist", "orthodox", 
                                                                  "pentecostal/born again/evangelical", "baptist", "presbyterian",
                                                                  "mammon", "jehovah's witness", "salvation army"
                                                                  ),
                                 "Muslim" = c("muslim"),
                                 "Other/no religion" = c("other", "no religion", "baha'i", "jewish", "hindu", "buddhist", "traditional", "others"), 
  ))

uganda %>% 
  flat_table(religion, cohort_cat)

# write data set
uganda %<>% 
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
df <-  bind_rows(df, uganda)
rm(meta, dfs, uganda)


