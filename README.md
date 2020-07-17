# Read me

This repository contains data and code necessary to reproduce analysis from the article: Jarosław Chilimoniuk, Alicja Gosiewska, Jadwiga Słowik, Romano Weiss, Stefan Rödiger, and Michał Burdukiewicz. countfitteR: count data analysis for precision medicine.

The analysis conducted in this article resulted in a framework for the automatic selection of the appropriate count distribution CountfitteR, available as an *R* package (https://CRAN.R-project.org/package=countfitteR) and a web server (TO DO).

## How to reproduce the main part of the analysis?

Run `scripts/countfitter_simulations.R` and `scripts/two_step_simulations.R`. The first script performs simulations for countfitteR, the second script performs simulations for Two-Step-Procedure.

## How to generate results and plots for publication?

Run `scripts/plots.R` to generate plots included in the article and supplementary articles.

Run `scripts/supplementary_tables.R` to generate tables included in the supplementary materials.

Run `scripts/mean_power_table.R` to generate Table 2.

## Repository strcture

All scripts are in the folder `scripts`.
Partial results saved as binary R data files are in the folder `data`.
Data and plots are in the folder `files`.

## R Session information

All scripts used in this study are compatible with following version of R:

- R version 3.6.1 (2019-07-05)

- x86_64-w64-mingw32/x64 (64-bit)

Necessary packages and their versions used in the analyses are listed in renv.lock

