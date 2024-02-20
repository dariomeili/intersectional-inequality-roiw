# read ipums data
usa <- read_csv("raw-data/usa_00001.csv")
usa %<>% 
  filter(YEAR == 2019)

usa %<>% 
  mutate(
    data_name = "USIPUMS",
    country = "usa",
    country_code = "US",
    "id_hh" = as.double(paste0(CLUSTER, SERIAL)),
    "id_respondent" = as.double(paste0(CLUSTER, SERIAL, SEX, AGE, BIRTHYR)),
    residence = NA, 
    religion = NA, 
  ) %>% 
  select(data_name,
         country,
         country_code,
         "id_cluster" = CLUSTER,
         id_hh,
         id_respondent,
         "wgt" = PERWT,
         "year" = YEAR,
         "gender" = SEX,
         "age" = AGE,
         "cohort" = BIRTHYR,
         residence, 
         "ethnicity" = RACE, 
         religion,
         "education" = EDUC)

usa %<>% 
  mutate(cohort_cat = case_when(
      cohort <= 1969 ~ "-1969",
      cohort >= 1970 & cohort < 1980 ~ "1970-1979",
      cohort >= 1980 ~ "1980-"),
    cohort_cat = fct_relevel(cohort_cat, "-1969", "1970-1979", "1980-")) %>%
  filter(age >= 18)

usa %<>%
  mutate(ethnicity = fct_recode(as_factor(ethnicity),
                                "White" = "1", 
                                "Black" = "2", 
                                "American Indian" = "3",
                                "Chinese" = "4",
                                "Japanese" = "5",
                                "Other Asian" = "6", 
                                "Other race" = "7", 
                                "Two major races" = "8", 
                                "Three or more races" = "9"
                            ),
         gender = fct_recode(as_factor(gender), 
                             "male" = "1", 
                             "female" = "2"))
usa %>% frq(ethnicity)  

usa %<>% 
  mutate(ethnicity = fct_collapse(ethnicity,
    "Two or more races" = c("Two major races", "Three or more races")
  ))

usa %>% flat_table(ethnicity, cohort_cat, gender)

# recode education data
usa %<>% 
  mutate(education = as.double(recode(education, 
                            "0" = "0",
                            "1" = "4",
                            "2" = "8", 
                            "3" = "9", 
                            "4" = "10", 
                            "5" = "11", 
                            "6" = "12", 
                            "7" = "13", 
                            "8" = "14", 
                            "9" = "15", 
                            "10" = "16", 
                            "11" = "17")) )
  
df <-  bind_rows(df, usa)
rm(usa)
