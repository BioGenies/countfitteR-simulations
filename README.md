# Read me

This repository contains data and code necessary to reproduce analysis from the article: Jarosław Chilimoniuk, Alicja Gosiewska, Jadwiga Słowik, Romano Weiss, Stefan Rödiger, and Michał Burdukiewicz. countfitteR: count data analysis for precision medicine.

The analysis conducted in this article resulted in a framework for the automatic selection of the appropriate count distribution CountfitteR, available as an *R* package (https://CRAN.R-project.org/package=countfitteR) and a web server (TO DO).

## How to reproduce the main part of the analysis?

Run `scripts/countfitter_simulations.R` and `scripts/two_step_simulations.R`. The first script performs simulations for CountFitter, the second script performs simulations for Two-Step-Procedure.

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

**R version 3.6.1 (2019-07-05)**

**Platform:** x86_64-w64-mingw32/x64 (64-bit) 

**locale:**
_LC_COLLATE=Polish_Poland.1250_, _LC_CTYPE=Polish_Poland.1250_, _LC_MONETARY=Polish_Poland.1250_, _LC_NUMERIC=C_ and _LC_TIME=Polish_Poland.1250_

**attached base packages:** 
_stats_, _graphics_, _grDevices_, _datasets_, _utils_, _methods_ and _base_

**other attached packages:** 
_MASS(v.7.3-51.6)_, _pscl(v.1.5.5)_, _pbapply(v.1.4-2)_, _countfitteR(v.1.2)_, _gt(v.0.2.1)_, _xtable(v.1.8-4)_, _patchwork(v.1.0.1)_, _latex2exp(v.0.4.0)_, _tidyr(v.1.0.2)_, _dplyr(v.0.8.5)_, _ggh4x(v.0.1.0.9000)_, _ggplot2(v.3.3.2)_ and _readr(v.1.3.1)_

**loaded via a namespace (and not attached):** 
_Rcpp(v.1.0.4.6)_, _later(v.1.0.0)_, _pillar(v.1.4.4)_, _compiler(v.3.6.1)_, _tools(v.3.6.1)_, _digest(v.0.6.25)_, _lifecycle(v.0.2.0)_, _tibble(v.3.0.1)_, _gtable(v.0.3.0)_, _pkgconfig(v.2.0.3)_, _rlang(v.0.4.6)_, _shiny(v.1.4.0.2)_, _rstudioapi(v.0.11)_, _parallel(v.3.6.1)_, _fastmap(v.1.0.1)_, _withr(v.2.2.0)_, _stringr(v.1.4.0)_, _vctrs(v.0.3.1)_, _hms(v.0.5.3)_, _grid(v.3.6.1)_, _tidyselect(v.1.0.0)_, _glue(v.1.4.1)_, _R6(v.2.4.1)_, _pander(v.0.6.3)_, _purrr(v.0.3.3)_, _magrittr(v.1.5)_, _promises(v.1.1.0)_, _htmltools(v.0.4.0)_, _scales(v.1.1.1)_, _ellipsis(v.0.3.1)_, _assertthat(v.0.2.1)_, _mime(v.0.9)_, _colorspace(v.1.4-1)_, _httpuv(v.1.5.2)_, _renv(v.0.11.0)_, _stringi(v.1.4.6)_, _munsell(v.0.5.0)_ and _crayon(v.1.3.4)_

