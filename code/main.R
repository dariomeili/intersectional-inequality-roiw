library(pacman)
p_load(tidyverse, magrittr, knitr, kableExtra, gtsummary, wesanderson, 
       GGally, ggrepel, ggpubr, modelsummary, stringr, tidytext, patchwork)

ggplot2::theme_set(ggplot2::theme_bw()) 

# load custom functions
source("code/functions/functions.R")

# load or run merging of dhs data
if (!file.exists("processed-data/merged.Rda")) {
  source("code/main_merge.R")
}
# load or run calculations to aggregate form
if (file.exists("processed-data/cohort.Rda") & 
    file.exists("processed-data/pool.Rda")) {
  load("processed-data/cohort.Rda")
  load("processed-data/pool.Rda")
  load("processed-data/dhs_main.Rda")
} else {
  source("code/main_calc.R")
}
#run analyses for table 1 and merge with main cohort data
source("code/main_analysis.R")

# load/run simulations
if (!file.exists("processed-data/sim_all.Rda")) {
  source("code/main_simulations.R")
} else {
  load("processed-data/sim_all.Rda")}

# Produce outputs
source("code/analysis_tables.R")
source("code/analysis_figures.R")


