# Preamble and Setup ----

## Load Required Data and Functions ----
# Load processed cohort and DHS data
load("processed-data/cohort.Rda") 
load("processed-data/dhs_main.Rda")

# Load custom functions
source("code/functions/functions.R")

## Define Region and Group Variables ----
# Fix regions and generate Africa dummy
cohort %<>% 
  mutate(
    sigi_region = case_when(
      country_long == "Congo" ~ "Africa", 
      country_long == "Niger" ~ "Africa", 
      country_long == "Guyana" ~ "Americas", 
      TRUE ~ sigi_region
    ),
    africa = fct_collapse(
      sigi_region, 
      "Not Africa" = c("Americas", "Asia", "Europe"),
      "Africa" = c("Africa")
    ), 
    africa = fct_relevel(africa, "Not Africa", "Africa")
  )

## Filter Data for Group Size ----
# Ensure intersecting groups have at least 40 observations per cohort
temp <- cohort %>% 
  select(country_long, cohort_cat, l_gen.eth_n, l_gen.eth_name, h_gen.eth_n, h_gen.eth_name) %>% 
  filter(l_gen.eth_n < 40 | h_gen.eth_n < 40)

# Regression Analysis on Inequality Ratios ----

## Prepare Data for Regressions ----
df_reg <- cohort %>% 
  mutate(
    log_gdp_pc = log(gdp_pc),
    l_share = l_share * 100, 
    h_share = h_share * 100, 
    n_sample = sample_size / 1000
  ) %>%
  filter(!(country_long == "Ethiopia" & cohort_cat == "-1969")) # Exclude Ethiopia -1969 cohort

## Education Variables ----
# Regression for education variables
m_educ <- df_reg %>% 
  feols(c(r_gen.eth, r_mech, diff_mech) ~ csw(educ_w, africa + cohort_cat), 
        cluster = ~alpha_3)

## Africa Dummy Significance ----
# Assess significance of Africa dummy in regression
m_educ_1 <- df_reg %>% 
  feols(c(r_gen.eth, r_mech, diff_mech) ~ educ_w + africa + cohort_cat, 
        cluster = ~alpha_3)

etable(m_educ_1)

## Sample Size Variables ----
# Regress inequality ratio on sample size
m_samp <- df_reg %>% 
  feols(c(r_gen.eth, r_mech, diff_mech) ~ csw(n_sample + n_ethnicity, 
                                              africa + cohort_cat), 
        cluster = ~alpha_3)

## Group Size Variables ----
# Regression for group size effects
m_group <- df_reg %>% 
  feols(c(r_gen.eth, r_mech, diff_mech) ~ csw(l_gen.eth_n + h_gen.eth_n, 
                                              africa + cohort_cat), 
        cluster = ~alpha_3)

# Theil Index Regressions ----

## Regression on Theil Index ----
m_theil <- df_reg %>% 
  feols(theil_gen.eth ~ sw(educ_w, n_sample + n_ethnicity, 
                           l_gen.eth_n + h_gen.eth_n) + africa + cohort_cat, 
        cluster = ~ alpha_3)

# Table Generation and Summary Statistics ----

## Modelsummary Presets ----
# Set modelsummary options for output formatting
options("modelsummary_format_numeric_latex" = "plain") # Disable `siunitx`
options("modelsummary_stars_note" = FALSE)

## Customize Glance Function ----
glance_custom.fixest <- function(x, ...) {
  dv <- insight::get_response(x)
  dv <- sprintf("%.4f", mean(dv, na.rm = TRUE))
  data.table::data.table(`Mean of DV` = dv)
}

# Set significance stars
stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01)
# Define GOF map for model output
gm <- tribble(
  ~raw, ~clean, ~fmt, 
  "adj.r.squared", "Adj. R2", 3,
  "nobs", "Num.Obs", 0,
  "dv", "Mean of DV",  4
)

## Generate Table One ----
# Generate summary statistics for complete cohorts
complete_cohorts <- cohort %>% 
  count(country_long) %>% 
  filter(n == 3) %>% 
  pull(country_long)

cohort %>%
  filter(country_long %in% complete_cohorts) %>% 
  select(
    "Cohort bracket" = cohort_cat, 
    "Education (yrs)" = educ_w, 
    "IR(gender)" = r_gen, 
    "IR(ethnicity)" = r_eth, 
    "IR(gender*ethnicity)" = r_gen.eth, 
    "Female (%)" = pct_female, 
    "No. of ethnic groups" = n_ethnicity,
    "Sample size" = sample_size, 
    "Group size low" = l_gen.eth_n, 
    "Group size high" = h_gen.eth_n
  ) %>% 
  tbl_summary(by = "Cohort bracket", 
              missing = "no",
              statistic = all_continuous() ~ "{mean} ({sd})") %>% 
  modify_footnote(all_stat_cols() ~ NA) %>% 
  as_kable_extra(format = "latex", booktabs = TRUE, linesep = "") %>% 
  save_kable(file = "latex/tables/tableone.tex")

# Regression Tables for Inequality Ratio on Correlates ----

## Generate Regression Tables ----
# Create summary tables for the regression models
modelsummary(
  list(
    "Panel A: Education Variables" = list(
      "(A)" = m_educ[[1]], "(B)" = m_educ[[2]], "(C)" = m_educ[[3]], 
      "(D)" = m_educ[[4]], "(E)" = m_educ[[5]], "(F)" = m_educ[[6]]
    ), 
    "Panel B: Sample Size" = list(
      "(A)" = m_samp[[1]], "(B)" = m_samp[[2]], "(C)" = m_samp[[3]], 
      "(D)" = m_samp[[4]], "(E)" = m_samp[[5]], "(F)" = m_samp[[6]]
    ), 
    "Panel C: Group Size" = list(
      "(A)" = m_group[[1]], "(B)" = m_group[[2]], "(C)" = m_group[[3]], 
      "(D)" = m_group[[4]], "(E)" = m_group[[5]], "(F)" = m_group[[6]]
    )
  ),
  coef_map = c(
    "educ_w" = "Mean education (yrs)$^a$", 
    "n_ethnicity" = "No. of ethnic groups",
    "n_sample" = "Sample size$^b$", 
    "l_gen.eth_n" = "Group size lowest",
    "h_gen.eth_n" = "Group size highest",  
    "africaAfrica" = "Africa",
    "cohort_cat1970-1979" = "Cohort 1970-1979$^c$", 
    "cohort_cat1980-" = "Cohort 1980-$^c$", 
    "(Intercept)" = "(Intercept)"
  ),
  gof_map = gm, 
  estimate = "{estimate}{stars}", 
  stars = stars, 
  fmt = 5, 
  escape = FALSE, 
  shape = "rcollapse"
) %>% group_tt(j = list(
  "Outcome:" = 1,
  "IR(gen:eth)" = 2:3,
  "Counterfactual" = 4:5,
  "Differential" = 6:7
)) %>% theme_tt("tabular") %>% 
  save_tt("latex/tables/reg_inequalityratio.tex", overwrite = TRUE)

## Generate Theil Regression Table ----
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

# Aggregated Data and Survey Descriptives ----

## Table of Aggregated Data ----
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

## Survey Descriptives ----

### Summary of Survey Years and Ethnic Groups ----
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
  rename("Country" = country_long, "N groups" = n_ethnicity) %>% 
  kbl(
    format = "latex", 
    longtable = TRUE, 
    row.names = FALSE,
    booktabs = TRUE, 
    linesep = "", 
    caption = "Survey Years and Ethnic Groups", 
    label = "years-ethnicity"
  ) %>% 
  kable_styling(latex_options = c("repeat_header"), font_size = 7) %>% 
  column_spec(1, width = "7em") %>% 
  column_spec(2, width = "6em") %>% 
  column_spec(3, width = "18em") %>% 
  save_kable(file = "latex/tables/years-ethnicity.tex")

## Names of Groups with Lowest and Highest Education ----
cohort %>% 
  select(
    "Country" = country_long, 
    "Cohort" = cohort_cat, 
    "Gender low" = l_gen_name, 
    "Gender high" = h_gen_name, 
    "Ethnicity low" = l_eth_name,
    "Ethnicity high" = h_eth_name, 
    "Intersect. low" = l_gen.eth_name,
    "Intersect. high" = h_gen.eth_name
  ) %>% 
  mutate(
    across(.cols = everything(), .fns = ~str_replace(.x, "female", "F")),
    across(.cols = everything(), .fns = ~str_replace(.x, "male", "M")),
    across(.cols = everything(), .fns = ~str_trunc(.x, 16, "right"))
  ) %>% 
  select(-Country) %>% 
  kbl(
    format = "latex", 
    longtable = TRUE, 
    row.names = FALSE,
    booktabs = TRUE, 
    linesep = "", 
    caption = "Names of Groups with the Lowest and Highest Education by Birth Cohort", 
    label = "groupnames", 
    align = c("llllllll")
  ) %>% 
  kable_styling(latex_options = c("repeat_header"), font_size = 7) %>%  
  pack_rows(index = table(fct_inorder(cohort$country_long))) %>% 
  footnote(
    general = "Table reports the names of the groups with the lowest and highest average education used for inequality ratios.", 
    general_title = "Notes: ", 
    threeparttable = TRUE
  ) %>% 
  save_kable(file = "latex/tables/groupnames.tex")

# Visualizations and Plots ----

## Plot Mean Education for All Countries ----
pool %>% 
  select(
    country_long, educ_w, l_gen_mean, h_gen_mean, l_eth_mean, h_eth_mean, 
    l_gen.eth_mean, h_gen.eth_mean
  ) %>% 
  filter(!is.na(l_gen.eth_mean)) %>% 
  pivot_longer(cols = c("educ_w", "l_gen_mean", "h_gen_mean", "l_eth_mean", 
                        "h_eth_mean", "l_gen.eth_mean", "h_gen.eth_mean")) %>% 
  mutate(Grouping = fct_collapse(
    name, 
    "Gender" = c("l_gen_mean", "h_gen_mean"),
    "Ethnicity" = c("l_eth_mean", "h_eth_mean"),
    "Intersectional" = c("l_gen.eth_mean", "h_gen.eth_mean"), 
    "Overall mean" = c("educ_w")
  )) %>% 
  ggplot(aes(x = value, y = fct_reorder(country_long, value))) +
  geom_point(aes(colour = Grouping, shape = Grouping, fill = Grouping), 
             alpha = 0.8) + 
  scale_color_viridis_d(option = "mako", end = 0.85) +
  scale_fill_viridis_d(option = "mako", end = 0.85) +
  scale_shape_manual(values = c(23, 21, 21, 21)) +
  theme_light() +
  theme(
    panel.border = element_blank(),
    axis.line = element_blank(), 
    axis.text.y = element_text(size = 7),
    legend.position = "bottom"
  ) +
  labs(x = "Education, yrs", y = "")

ggsave("latex/figures/mean-educ.pdf", width = 7, height = 7)

## Plot Inequality Ratios by Groups ----

plot_ir <- cohort %>%
  arrange(country_long, desc(cohort_cat)) %>% 
  group_by(country_long) %>% 
  slice_head(n=1) %>% 
  ungroup() %>% 
  mutate(country_long = fct_recode(country_long,
                                   "Bolivia*" = "Bolivia", 
                                   "Brazil*"="Brazil", 
                                   "Central African Rep.°" = "Central African Republic", 
                                   "Ivory Coast°" = "Ivory Coast", 
                                   "Kazakhstan*" = "Kazakhstan", 
                                   "Namibia*" = "Namibia",
                                   "Philippines*" = "Philippines", 
                                   "Rwanda°" = "Rwanda", 
                                   "Zimbabwe°" = "Zimbabwe")) %>% 
  ggplot(aes(x=fct_reorder(country_long, r_gen.eth, .desc = TRUE))) +
  geom_point(aes(y=r_gen, colour = "Gender"), size=2, shape = 16) +
  geom_point(aes(y=r_eth, colour = "Ethnicity"), size=2, shape = 17,) +
  geom_point(aes(y=r_gen.eth, colour = "Intersectional"), size=2, shape = 18) +
  annotate("rect", 
           xmin = "South Africa", 
           xmax = "Afghanistan", 
           ymin = Inf, 
           ymax = 1, 
           alpha = 0.2) +
  annotate("text", x="South Africa", y=1-0.125, label="Mean Education:", size = 3) +
  geom_text(aes(y=1, label=round(gw_educ, digits = 2)), nudge_y = +0.05, size = 3) +
  scale_colour_viridis_d(option = "mako", 
                         begin = 0.2, 
                         end = 0.85,
                         breaks = c("Gender", "Ethnicity", "Intersectional")) +
  #scale_y_reverse() +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.text.y = element_text(size=9),
        legend.position = "bottom") +
  labs(x="", y="Inequality ratio", colour = "Grouping") +
  coord_flip()

ggsave(file="latex/figures/world.pdf",  
       plot = plot_ir ,width = 7, height = 7)

## Plot Excess Intersectionality for All Countries ----
plot_diff <- cohort %>% 
  arrange(country_long, desc(cohort_cat)) %>% 
  group_by(country_long) %>% 
  slice_head(n=1) %>% 
  ungroup() %>% 
  mutate(country_long = fct_recode(country_long, 
                                   "Bolivia*" = "Bolivia", 
                                   "Brazil*"="Brazil", 
                                   "C.African Rep.°" = "Central African Republic", 
                                   "Ivory Coast°" = "Ivory Coast", 
                                   "Kazakhstan*" = "Kazakhstan", 
                                   "Namibia*" = "Namibia",
                                   "Philippines*" = "Philippines", 
                                   "Rwanda°" = "Rwanda", 
                                   "Zimbabwe°" = "Zimbabwe")) %>% 
  filter(!is.na(diff_mech)) %>% 
  mutate(diff_mech = diff_mech*100) %>% 
  select(country_long, diff_mech, r_gen.eth) %>% 
  ggplot(aes(x=fct_reorder(country_long, diff_mech), y= diff_mech, colour = r_gen.eth)) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha=0.5) + 
  geom_point() +
  geom_text(data=. %>% filter(
    diff_mech>3 | (diff_mech<0 & diff_mech>-5)), # Filter data first
    aes(label=country_long, y=diff_mech),
    nudge_y = -1.2,
    size=3) +
  geom_text(data=. %>% filter(
    (diff_mech<3 & diff_mech>0) | diff_mech < -5), # Filter data first
    aes(label=country_long, y=diff_mech),
    nudge_y = 1.2,
    size=3) +
  coord_flip() +
  scale_colour_viridis_b(option = "mako", direction = -1, end = 0.85)+
  labs(colour = "Inequality ratio", x="", 
       y="Differential intersectionality") +
  theme_light() +
  theme(panel.border = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom")

ggsave("latex/figures/diff-mech.pdf", 
       plot = plot_diff, width = 7, height = 7)





