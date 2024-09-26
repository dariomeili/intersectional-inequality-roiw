# Master Script for Running All Analyses ----

# Preamble ----

## Install and Load Required Packages ----
# Installs the pacman package if it is not already installed, and then loads it
if (!require("pacman")) {
  install.packages("pacman")
}

# Load required libraries
library(pacman)
p_load(
  tidyverse, magrittr, vroom, janitor, knitr, haven, kableExtra, sjmisc, 
  gtsummary, wesanderson, GGally, ggrepel, ggpubr, modelsummary, 
  stringr, tidytext, patchwork, countrycode, sjlabelled, DescTools, fixest, 
  kableExtra, gtsummary, modelsummary, tinytable
)

## Set ggplot Theme ----
# Use a preset theme for all ggplot visualizations
ggplot2::theme_set(ggplot2::theme_bw()) 

## Load Custom Functions ----
# Source all custom functions from the functions script
source("code/functions/functions.R")

# Data Preparation and Loading ----

## Load or Run Merging of DHS Data ----
# Check if the merged dataset exists, otherwise run the merging script
if (!file.exists("processed-data/merged.Rda")) {
  source("code/merge.R")
}

## Load or Run Data Aggregation ----
# Check if aggregated cohort and pool datasets exist, otherwise run the aggregation script
if (file.exists("processed-data/cohort.Rda") & file.exists("processed-data/pool.Rda")) {
  load("processed-data/cohort.Rda")
  load("processed-data/pool.Rda")
  load("processed-data/dhs_main.Rda")
} else {
  source("code/aggregate.R")
}

# Analysis ----

## Run Main Analysis for Table 1 ----
# Perform analysis and merge with main cohort data
source("code/analysis.R")

## Load or Run Simulations ----
# Check if the simulation results exist, otherwise run the simulations script
if (!file.exists("processed-data/sim_all.Rda")) {
  source("code/simulations.R")
  source("code/analysis_sim.R")
} else {
  load("processed-data/sim_all.Rda")
  source("code/analysis_sim.R")
}

