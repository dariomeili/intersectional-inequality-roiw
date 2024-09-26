p_load(kableExtra, gtsummary, modelsummary, tinytable)

## Modelsummary presets
options("modelsummary_format_numeric_latex" = "plain") # To disable `siunitx` and prevent `modelsummary` from wrapping numeric entries in `\num{}`
options("modelsummary_stars_note" = FALSE)

glance_custom.fixest <- function(x, ...) {
  dv <- insight::get_response(x)
  dv <- sprintf("%.4f", mean(dv, na.rm = TRUE))
data.table::data.table(`Mean of DV` = dv)}

stars = c("*"=0.1, "**" = 0.05, "***"= 0.01)

gm <- tribble(
  ~raw, ~clean, ~fmt, 
  "adj.r.squared", "Adj. R2", 3,
  "nobs", "Num.Obs", 0,
  "dv", "Mean of DV",  4)

##################
# TABLE ONE 
##################
complete_cohorts <-  cohort %>% 
  count(country_long) %>% 
  filter(n ==3) %>% 
  pull(country_long)

cohort %>%
  filter(country_long %in% complete_cohorts) %>% 
  select("Cohort bracket"=cohort_cat, 
         "Education (yrs)"=educ_w, 
         "IR(gender)"=r_gen, 
         "IR(ethnicity)"=r_eth, 
         "IR(gender*ethnicity)"= r_gen.eth, 
         "Female (%)"=pct_female, 
         "No. of ethnic groups" = n_ethnicity,
         "Sample size"= sample_size, 
         "Group size low" = l_gen.eth_n, 
         "Group size high" = h_gen.eth_n) %>% 
  tbl_summary(by="Cohort bracket", 
              missing = "no",
              statistic = all_continuous() ~ "{mean} ({sd})") %>% 
  modify_footnote(all_stat_cols() ~ NA) %>% 
  as_kable_extra(format = "latex",
                 booktabs = TRUE,
                 linesep = ""
                 ) %>% 
  save_kable(file = "latex/tables/tableone.tex")

####################################################
# Regression table of inequality ratio on correlates
####################################################

# Generate the table 
modelsummary(list(
  "Panel A: Education Variables" = list(
    "(A)"=m_educ[[1]], 
    "(B)"=m_educ[[2]],
    "(C)"=m_educ[[3]],
    "(D)"=m_educ[[4]],
    "(E)"=m_educ[[5]],
    "(F)"=m_educ[[6]]), 
  "Panel B: Sample Size" = list(
    "(A)"=m_samp[[1]], 
    "(B)"=m_samp[[2]],
    "(C)"=m_samp[[3]],
    "(D)"=m_samp[[4]],
    "(E)"=m_samp[[5]],
    "(F)"=m_samp[[6]]), 
  "Panel C: Group Size" = list(
    "(A)"=m_group[[1]], 
    "(B)"=m_group[[2]],
    "(C)"=m_group[[3]],
    "(D)"=m_group[[4]],
    "(E)"=m_group[[5]],
    "(F)"=m_group[[6]])),
  coef_map = c("educ_w" = "Mean education (yrs)$^a$",
               "n_ethnicity" = "No. of ethnic groups",
               "n_sample" = "Sample size$^b$",
               "l_gen.eth_n" = "Group size lowest",
               "h_gen.eth_n" = "Group size highest",  
               "africaAfrica" = "Africa",
               "cohort_cat1970-1979" = "Cohort 1970-1979$^c$",
               "cohort_cat1980-" = "Cohort 1980-$^c$", 
               "(Intercept)" = "(Intercept)"),
  gof_map = gm,
  estimate="{estimate}{stars}",
  stars = stars, 
  shape = "rcollapse",
  fmt = 5, 
  escape = FALSE) %>%
  group_tt(j= list("Outcome:"=1,
                     "IR(gen:eth)"=2:3,
                     "Counterfactual"=4:5,
                     "Differential"=6:7)) %>%
  theme_tt("tabular") %>% 
  save_tt("latex/tables/reg_inequalityratio.tex", overwrite = TRUE)

################################################
# Regression table of  Theil index on correlates
################################################

modelsummary(list("(A)"=m_theil[[1]], 
                  "(B)"=m_theil[[2]],
                  "(C)"=m_theil[[3]]),
                  coef_map = c("educ_w" = "Mean education (yrs)$^a$",
                               "n_ethnicity" = "No. of ethnic groups",
                               "n_sample" = "Sample size$^b$",
                               "l_gen.eth_n" = "Group size lowest",
                               "h_gen.eth_n" = "Group size highest",  
                               "africaAfrica" = "Africa",
                               "cohort_cat1970-1979" = "Cohort 1970-1979$^c$",
                               "cohort_cat1980-" = "Cohort 1980-$^c$", 
                               "(Intercept)" = "(Intercept)"),
             estimate = "{estimate}{stars}",
             stars = stars,
             gof_map = gm,
             fmt = 5,
             escape = FALSE) %>% 
  group_tt(j = list("Group Theil Index" = 2:4)) %>% 
  theme_tt("tabular") %>% 
  save_tt(output = "latex/tables/reg_theil.tex", overwrite = TRUE)

####################################
# Table of Aggregated Data (Results)
####################################

cohort %>% 
  mutate(sd_educ = paste0("(", round(sd_educ, 2), ")"), 
         educ_w = round(educ_w, 2), 
         across(where(is.numeric), ~round(.x, 2))) %>% 
  unite("educ_mean_sd", educ_w, sd_educ, sep=" ") %>% 
  select("Cohort" = cohort_cat,
         "Education" = educ_mean_sd,
         "IR(gender)" = r_gen, 
         "IR(eth)" = r_eth, 
         "IR(gen:eth)" = r_gen.eth,
         "IR(gen:eth)'" = r_mech, 
         "Differential" = diff_mech,
         "obs."= sample_size) %>%
  kbl(format = "latex", 
      longtable = T,
      digits = 2,
      row.names = F,
      booktabs = T,
      linesep = "",
      caption = "Education Inequality Ratios by Birth Cohort and Country for Gender and Ethnicity", 
      label = "aggregated",
      align = c("lccccccc")) %>% 
  kable_styling(latex_options = c("repeat_header"), 
                font_size = 7) %>% 
  pack_rows(index = table(fct_inorder(cohort$country_long))) %>% 
  footnote(general = "Education reports mean (sd); IR(G) reports inequality ratios between the group with the highest and lowest average education.",
           general_title = "Notes: ",
           threeparttable = T) %>% 
  save_kable(file = "latex/tables/aggregated-data.tex")
  
####################################
# Survey Descriptives
####################################
survey_summary_a <- dhs_main %>%
  distinct(country_long, year) %>%
  group_by(country_long) %>%
  summarise("Survey years" = toString(year)) 

survey_summary_b <- dhs_main %>%
  distinct(country_long, ethnicity) %>%
  group_by(country_long) %>%
  summarise("Ethnic groups" = toString(ethnicity))

survey_summary_c <- dhs_main %>% 
  distinct(country_long, ethnicity) %>%
  count(country_long, ethnicity) %>% 
  group_by(country_long) %>% 
  summarise(n_ethnicity = sum(n))

survey_summary <- 
  left_join(survey_summary_a, survey_summary_b) %>% 
  left_join(survey_summary_c)

survey_summary %>% 
  rename("Country" = country_long, "N groups"=n_ethnicity) %>% 
  kbl(format = "latex", 
      longtable = T,
      row.names = F,
      booktabs = T,
      linesep = "",
      caption = "Survey Years and Ethnic Groups", 
      label = "years-ethnicity") %>% 
  kable_styling(latex_options = c("repeat_header"), 
                font_size = 7) %>% 
  column_spec(1, width = "7em") %>% 
  column_spec(2, width = "6em") %>% 
  column_spec(3, width = "18em") %>% 
  save_kable(file = "latex/tables/years-ethnicity.tex")

####################################
# Names of Highest and Lowest groups
####################################
cohort %>% 
  select("Country"=country_long, 
         "Cohort"=cohort_cat, 
         "Gender low"=l_gen_name, 
         "Gender high"=h_gen_name, 
         "Ethnicity low"=l_eth_name,
         "Ethnicity high"=h_eth_name, 
         "Intersect. low"=l_gen.eth_name,
         "Intersect. high"=h_gen.eth_name) %>% 
  mutate(across(.cols = everything(), 
                .fns = ~str_replace(.x, "female", "F")),
         across(.cols = everything(),
                .fns = ~str_replace(.x, "male", "M")),
         across(.cols = everything(),
                .fns = ~str_trunc(.x, 16, "right"))
  ) %>% 
  select(-Country) %>% 
  kbl(format = "latex", 
      longtable = T,
      row.names = F,
      booktabs = T,
      linesep = "",
      caption = "Names of Groups with the Lowest and Highest Education by Birth Cohort", 
      label = "groupnames",
      align = c("llllllll")) %>% 
  kable_styling(latex_options = c("repeat_header"), 
                font_size = 7) %>%  
  pack_rows(index = table(fct_inorder(cohort$country_long))) %>% 
  footnote(general = "Table reports the names of the groups with the lowest and the highest average education that were used to calculate inequality ratios.", 
           general_title = "Notes: ",
           threeparttable = T) %>% 
  save_kable(file = "latex/tables/groupnames.tex")


  
