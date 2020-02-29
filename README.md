
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rchitex

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/bdempe18/rchitex.svg?branch=master)](https://travis-ci.org/bdempe18/rchitex)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/bdempe18/rchitex?branch=master&svg=true)](https://ci.appveyor.com/project/bdempe18/rchitex)
<!-- badges: end -->

Rchitex provides allows users to generate elegantly formatted text
tables for summary statistics and regression models while simultaneously
writing the equivalent LaTeX code for individual tables or full
appendices to a local file. Rchitex is likewise capable of producing
Latex and HTML tables for markdown files. Rchitex is intended to bridge
the agp between statistical exploration in R and article writing.

## Installation

You will be able to install the released version of rchitex from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rchitex")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bdempe18/rchitex")
```

Until development exceeds the experimental phase, rchitex will only be
available on github.

## Overview

Rchitex’s central goal is to ease the gap between statistical
exploration and the writing process. Rchitex accomplishes this goal by
providing tools that simplify the construction of appendixes and
publication-ready tables. Dataframes and models are readily converted to
LateX, html, and text output with a large number of available
customizations. These table outputs can be stored as objects and
deployed whenever so desired. Groupings of outputs (tables and Ggplot
graphics) can then converted into into a LaTeX appendix without
extraneous time spent formatting.

Rchitex also includes a hodgepodge of extra features, most notably a
Robust Standard Error wrapper than simplifies standard error
transformations.

## Supported models

Rchitex currently supports a growing number of model types. Currently,
the list includes - Linear and binominal regression (`lm` and `glm` in
the stats package) - Panel regression (contained in the `plm` package) -
Instrumental variables and Tobit regression (within the `AER` package)
Supported models are every-growing. Please feel free to request new
models by making a pull request.

## Future development

The development branch of rchitex is currently working on providing
statistical analysis of bibliographies. This help prevent authors from
missing key pieces of literatures and help focus research.
