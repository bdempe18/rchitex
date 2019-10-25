
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

RCHITEX provides an extensive set of options allowing users to generate
and output nicely formatted text tables wiles simultaneously outputting
the equivalent Latex code to a provided path. rchitex is intended to
bridge the gap between statistical exploration in R and article writing.

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

Until development exceeds the experimental phase, the package will only
be available on github.

## Usage

Rchitex was designed to produce underlying Latex code for regression
tables and summary statistics while also providing an informative text
display in the console. Rchitex can also directly output Latex and html
which can be formatted with knitr and RMarkdown. The `build` method
allows for the construction of heavily customizable regression tables.
The `describe` method similarly allows for the construction of
descriptive statistics tables originating from either a data frame,
matrix, matrix, or tibble.

``` r
data('freeny')
mod <- lm(y ~ lag.quarterly.revenue + price.index + income.level + 
            market.potential, data = freeny)
mod2 <- lm(y ~ lag.quarterly.revenue + price.index + income.level, 
           data = freeny)

# regression table
b <- build(mod1, mod, md='latex')

# summary statistics
df <- data.frame("first" = c(4,5,6), "second" = c(7,5,3))
d <- describe(df, md='html')
```

A full Latex appendix can be generated from any number of rchitex and
ggplot object. The `appendize` function will save each table/plot in an
organized tree structure and generate an *appendix.tex* file which
presents each table/plot on a separate page all under an Appendix
section label.

``` r
fp <- paste(getwd(), 'rchitEx', sep='/')
appendize(b, d, dir=fp)
```

## Tutorial

A full tutorial of all features and available customization is available
in the Usage vignette that comes with the package.
