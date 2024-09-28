# Replication Package for Intersecting Inequality Analysis Using DHS and Other Data

## Project Overview

This repository contains the replication package for the paper analyzing intersecting inequality using DHS (Demographic and Health Survey) data, combined with other relevant datasets (such as the SIGI index, GDP per capita, and world population data).
This README outlines the steps required to replicate the analysis and provides details about the individual scripts.

## Requirements

### Software:

-   **R** version 4.0 or later
-   **renv** package (for package management and dependency isolation)

### Libraries:

The project uses a variety of R packages, which are automatically managed by `renv`.
To restore the environment, simply run the following in R:

`renv::restore()`

## Replication Steps

### Step 1

Download the data: Some datasets are included in this repository, while others (such as DHS data) must be downloaded separately.

DHS Data:

Download the necessary DHS files for specific countries listed below from the [DHS program website.](https://dhsprogram.com/data/) with the ".DTA" file extension

-   usa_00001.csv: US Census data from [IPUMS](https://usa.ipums.org/) (2019 APS Data with the ".csv" file extension

Place the downloaded .DTA/csv. files into the "raw-data/dhs/" folder.
Other Datasets (already included in raw-data/):
 
-   sigi.csv: Data from the SIGI (Social Institutions and Gender Index) homepage (OECD).

-   gdp-per-capita-worldbank.csv: World Bank GDP per capita data.

-   wb_worldregions.csv: World Bank world regions.

### Step 2

Place the DHS Data: Once downloaded, the DHS data should be placed in the following paths within the raw-data/dhs/ folder:

raw-data/dhs/AFIR70FL.DTA

raw-data/dhs/AFMR70FL.DTA ...

A full list of the necessary DHS Data files can found below.

### Step 3

After setting up the data, you can run the entire analysis pipeline by executing the `main.R` script.
This will load the necessary data, run simulations, and perform the final analysis.

`source("main.R")`

## Overview of Scripts

### `main.R`

This is the main script that ties all other components together.
It runs the entire analysis and simulation process, ensuring that all required data are loaded and the necessary scripts are sourced.

### `analysis_sim.R`

This script handles the inequality simulations, generating results for different group sizes and sample sizes.
It saves the processed simulation results to `processed-data/sim_all.Rda`.

### `aggregate.R`

This script aggregates the processed data for further analysis or visualization.

### `simulations.R`

Handles the simulation of inequality indices across different groups and sample sizes.
Results are processed into a compact form and stored for analysis.

### `analysis.R`

Performs the main analysis on the DHS and cohort data, including filtering, cleaning, and descriptive statistics generation.

### `merge.R`

Merges DHS data with country-level information and produces a combined dataset for further analysis.

### `worldpopulation.R`

Processes and merges world population data with cohort data to provide context for sample populations in relation to the global and African populations.

## Output

Processed data files and results are stored in the `processed-data/` folder:

-   `sim_all.Rda`: Simulation results.

-   `merged.Rda`: Combined DHS and country-level data.

-   Additional output files from the analysis.

## Contact

For any issues or questions regarding the replication package, please contact Dario Meili at [dario.meili\@nadel.ethz.ch].

### Required DHS Files for Replication

To replicate the analysis, you must download specific DHS files from the [DHS program website](https://dhsprogram.com/data/).
These files are not included in the replication package due to licensing restrictions.
The following list contains the required `.DTA` files, which should be placed in the `raw-data/dhs/` folder.

#### List of Required DHS Files:

| Country                               | DHS Files                                                                                                                                                                                              |
|---------------------------------|---------------------------------------|
| Afghanistan                           | AFIR70FL, AFMR70FL                                                                                                                                                                                     |
| Albania                               | ALIR50FL, ALIR71FL, ALMR50FL, ALMR71FL                                                                                                                                                                 |
| Burkina Faso                          | BFIR21FL, BFIR31FL, BFIR43FL, BFIR62FL, BFMR21FL, BFMR31FL, BFMR41FL, BFMR62FL                                                                                                                         |
| Benin                                 | BJIR31FL, BJIR41FL, BJIR51FL, BJIR61FL, BJIR71FL, BJMR31FL, BJMR41FL, BJMR51FL, BJMR61FL, BJMR71FL                                                                                                     |
| Bolivia                               | BOIR3BFL, BOIR41FL, BOIR51FL, BOMR3BFL, BOMR41FL, BOMR51FL                                                                                                                                             |
| Brazil                                | BRIR31FL, BRMR31FL                                                                                                                                                                                     |
| Congo, the Democratic Republic of the | CDIR50FL, CDIR61FL, CDMR50FL, CDMR61FL                                                                                                                                                                 |
| Central African Republic              | CFIR31FL, CFMR31FL                                                                                                                                                                                     |
| Congo                                 | CGIR51FL, CGIR60FL, CGMR51FL, CGMR60FL                                                                                                                                                                 |
| Ivory Coast                           | CIIR35FL, CIMR33FL                                                                                                                                                                                     |
| Cameroon                              | CMIR22FL, CMIR31FL, CMIR44FL, CMIR60FL, CMIR71FL, CMMR21FL, CMMR31FL, CMMR44FL, CMMR60FL, CMMR71FL                                                                                                     |
| Ethiopia                              | ETIR41FL, ETIR51FL, ETIR61FL, ETIR70FL, ETMR41FL, ETMR51FL, ETMR61FL, ETMR70FL                                                                                                                         |
| Gabon                                 | GAIR60FL, GAMR60FL                                                                                                                                                                                     |
| Ghana                                 | GHIR31FL, GHIR41FL, GHIR5AFL, GHIR70FL, GHMR31FL, GHMR41FL, GHMR5AFL, GHMR70FL                                                                                                                         |
| Guinea                                | GNIR41FL, GNIR71FL, GNMR41FL, GNMR71FL                                                                                                                                                                 |
| Guatemala                             | GUIR71FL, GUMR71FL                                                                                                                                                                                     |
| Guyana                                | GYIR5IFL, GYMR5IFL                                                                                                                                                                                     |
| Honduras                              | HNIR62FL, HNMR62FL                                                                                                                                                                                     |
| Kenya                                 | KEIR33FL, KEIR3AFL, KEIR42FL, KEIR52FL, KEIR70FL, KEMR32FL, KEMR3AFL, KEMR42FL, KEMR52FL, KEMR70FL                                                                                                     |
| Kazakhstan                            | KKIR42FL, KKMR41FL                                                                                                                                                                                     |
| Liberia                               | LBIR51FL, LBIR6AFL, LBIR7AFL, LBMR51FL, LBMR6AFL, LBMR7AFL                                                                                                                                             |
| Moldova, Republic of                  | MBIR53FL, MBMR52FL                                                                                                                                                                                     |
| Mali                                  | MLIR32FL, MLIR41FL, MLIR53FL, MLIR6HFL, MLIR7HFL, MLMR31FL, MLMR41FL, MLMR53FL, MLMR6HFL, MLMR7HFL                                                                                                     |
| Malawi                                | MWIR41FL, MWIR4DFL, MWIR61FL, MWIR7HFL, MWMR41FL, MWMR4DFL, MWMR61FL, MWMR7HFL                                                                                                                         |
| Mozambique                            | MZIR31FL, MZIR41FL, MZIR62FL, MZIR71FL, MZMR31FL, MZMR41FL, MZMR62FL, MZMR71FL                                                                                                                         |
| Nigeria                               | NGIR41FL, NGIR4BFL, NGIR53FL, NGIR6AFL, NGIR7AFL, NGMR41FL, NGMR4AFL, NGMR52FL, NGMR6AFL, NGMR7AFL                                                                                                     |
| Niger                                 | NIIR22FL, NIIR31FL, NIIR51FL, NIIR61FL, NIMR21FL, NIMR31FL, NIMR51FL, NIMR61FL                                                                                                                         |
| Namibia                               | NMIR41FL, NMIR51FL, NMIR61FL, NMMR41FL, NMMR51FL, NMMR61FL                                                                                                                                             |
| Philippines                           | PHIR41FL, PHMR41FL                                                                                                                                                                                     |
| Pakistan                              | PKIR61FL, PKMR61FL                                                                                                                                                                                     |
| Rwanda                                | RWIR21FL, RWIR41FL, RWIR53FL, RWIR5AFL, RWIR61FL, RWIR70FL, RWMR21FL, RWMR41FL, RWMR53FL, RWMR5AFL, RWMR61FL, RWMR70FL                                                                                 |
| Sierra Leone                          | SLIR51FL, SLIR61FL, SLIR7AFL, SLMR51FL, SLMR61FL, SLMR7AFL                                                                                                                                             |
| Senegal                               | SNIR21FL, SNIR32FL, SNIR4HFL, SNIR61FL, SNIR70FL, SNIR7HFL, SNIR7QFL, SNIR7ZFL, SNIR81FL, SNIR8AFL, SNMR21FL, SNMR31FL, SNMR4HFL, SNMR61FL, SNMR70FL, SNMR7HFL, SNMR7QFL, SNMR7ZFL, SNMR81FL, SNMR8AFL |
| Chad                                  | TDIR31FL, TDIR41FL, TDIR71FL, TDMR31FL, TDMR41FL, TDMR71FL                                                                                                                                             |
| Togo                                  | TGIR31FL, TGIR61FL, TGMR31FL, TGMR61FL                                                                                                                                                                 |
| Uganda                                | UGIR33FL, UGIR41FL, UGIR52FL, UGIR60FL, UGIR7HFL, UGMR33FL, UGMR41FL, UGMR52FL, UGMR60FL, UGMR7HFL                                                                                                     |
| South Africa                          | ZAIR71FL, ZAMR71FL                                                                                                                                                                                     |
| Zambia                                | ZMIR31FL, ZMIR42FL, ZMIR51FL, ZMIR61FL, ZMIR71FL, ZMMR31FL, ZMMR41FL, ZMMR51FL, ZMMR61FL, ZMMR71FL                                                                                                     |
| Zimbabwe                              | ZWIR31FL, ZWIR42FL, ZWIR52FL, ZWIR62FL, ZWIR70FL, ZWMR31FL, ZWMR41FL, ZWMR52FL, ZWMR62FL, ZWMR70FL                                                                                                     |
