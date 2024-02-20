
# prep and merge data

# define files to use
meta <- tibble(file_path = all_dfs, file_name = names(all_dfs)) %>% 
  filter(str_detect(all_dfs, "/ZM"))

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
dfs[["ZMIR31FL"]]$v007year <- 1996
dfs[["ZMMR31FL"]]$v007year <- 1996
dfs[["ZMMR51FL"]]$v007year <- as_numeric(2007)

dfs$ZMIR71FL$v131eth <- as_factor(dfs$ZMIR71FL$v131eth)
dfs$ZMMR71FL$v131eth <- as_factor(dfs$ZMMR71FL$v131eth)
dfs$ZMMR31FL$v131eth <- as_factor(dfs$ZMMR31FL$v131eth)


zambia <- bind_rows(dfs, .id = "data_name") %>% 
  mutate(country = "zambia", 
         country_code = "ZM")

zambia %>% 
  frq(v007year)

zambia %<>%
  mutate(year = v007year)
# gender
zambia %<>% 
  mutate(gender = case_when(str_detect(data_name, "IR") ~ "female",
                            str_detect(data_name, "MR") ~ "male"))
# age, cohort and binned cohort
# filter by age (only working age population between 18 and 65)
zambia %<>% 
  mutate(age = v012age,
         cohort = year-age, 
         cohort_cat = case_when(
           cohort <= 1969 ~ "-1969",
           cohort >= 1970 & cohort < 1980 ~ "1970-1979",
           cohort >= 1980 ~ "1980-"),
         cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>% 
  filter(age >= 18)

zambia %>% 
  frq(cohort_cat)
# harmonize ethnicity
zambia %>% 
  frq(v131eth)

zambia %<>% 
  mutate(v131eth = fct_collapse(v131eth,
    "bemba"="1", "lunda (luapula)"="2", "lala"="3", "bisa"="4",
    "ushi"="5","chishinga"="6", "ngumbo"="7", "lamba"="8", 
    "kabende"="9", "tabwa"="10", "swawka"="11", "mukulo"="12",   
    "ambo"="13", "lima"="14", "shila"="15", "unga"="16", 
    "bwile"="17", "luano"="18", "tonga"="19", "lenje"="20",
    "soli"="21", "ila"="22", "toka-leya"="23", "sala"="24", 
    "gowa"="25", "luvale"="26", "lunda (north-western)"="27", "mbunda"="28",   
    "luchazi"="29", "ndembu"="30", "mbowe"="31", "chokwe"="32",   
    "kaonde sub-group"="33", "luana sub-group"="34", "kwanga" = "35", "kwandi"="36",   
    "koma"="37", "nyengo"="38", "simaa"="39", "nwenyi"="40",   
    "imilangu"="41", "mashi"="42", "lozi"="43", "totela"="44",   
    "subiya"="45", "nkoya"="46", "mashasha"="47", "chewa"="48", 
    "nsenga"="49", "ngoni"="50", "nyanja"="51", "kunda"="52", 
    "chicunda"="53", "lungu"="54", "mambwe"="55", "namwanga"="56", 
    "wina"="57", "tambo"="58", "tumbuka"="59", "senga"="60", 
    "yombe"="61", "other african"="62", "american"="63",  "asian"="64", 
    "european"="65",  "other zambian"="66", "dk"="67"  
  ))
zambia %>% 
  flat_table(v131eth, year)
zambia %>% 
  flat_table(v131eth, cohort_cat, gender)  

zambia %<>% 
  mutate(ethnicity = fct_collapse(v131eth,
                                  "bemba" = c("bemba", "chishinga", "ngumbo", "kabende", "unga", "mukulu", "mukulo", "tabwa", "shila",
                                              "bwile"),
                                  "lunda" = c("lunda (luapula)", "lunda (north-western)","ndembu", "lunda (northwestern)"),
                                  "lala-bisa" = c("lala", "bisa", "swawka", "ambo", "luano", "swaka"),
                                  "ushi" = c("ushi"), 
                                  "lamba"= c("lamba", "lima"), 
                                  "lenje-tonga" = c("tonga", "lenje", "soli", "ila", "toka-leya", "sala"),
                                  "luvale" = c("luvale"),
                                  "mbunda" = c("mbunda"),
                                  "chokwe-luchazi" = c("luchazi", "chokwe"),
                                  "kaonde-nkoya" = c("kaonde sub-group", "kaonde", "nkoya", "mashasha"),
                                  "lozi" = c("lozi"),
                                  "chewa" = c("chewa", "nyanja", "kunda", "chicunda", "chikunda"), 
                                  "nsenga" = c("nsenga", "senga"),
                                  "ngoni" = c("ngoni"),
                                  "mambwe-lungu" = c("lungu", "mambwe"),
                                  "namwanga" = c("namwanga", "tambo"),
                                  "tumbuka" = c("tumbuka", "yombe"),
                                  "other" = c("wina", "other zambian", "other zambia", "gowa", "subiya", "totela", "mashi","luyana", 
                                              "koma", "nyengo", "simaa", "nwenyi", "imilangu", "mbowe", 
                                              "luana sub-group", "kwanga", "kwandi", "kwangwa", "mwenyi", "other african", "missing",
                                              "african", "not applicable",
                                              "other language", "american", "asian", "dk", "english", "european", "other")
                                  )
         
  ) 

zambia %>% 
  flat_table(ethnicity, cohort_cat, gender)

# harmonize religion
zambia %>% 
  flat_table(v130rel, year)
zambia %>% 
  flat_table(v130rel, cohort_cat)

zambia %<>%
  mutate(religion = fct_collapse(v130rel, 
                                 "other/no religion" = c("other", "none", "no religion"), 
  ))

zambia %>% 
  flat_table(religion, cohort_cat)

# write data set
zambia %<>% 
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
df <-  bind_rows(df, zambia)
rm(meta, dfs, zambia)


