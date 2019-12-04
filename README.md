
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

Until development exceeds the experimental phase, the package will only
be available on github.

## Text summary tables

Summary statistics can be simply produced by passing a data frame or
tibble to `describe`.

``` r
suppressPackageStartupMessages(library(tidyverse))
state_df <- as_tibble(datasets::state.x77)
tbl_title <- 'State summary statistics'
rchitex::describe(state_df, title = tbl_title)
#>                 STATE SUMMARY STATISTICS                      
#> =========================================================
#> Statistic    N    Mean        St. Dev     Min     Max       
#> ---------------------------------------------------------
#> Population   50   4,246.42    4,464.491    365    21,198    
#> Income       50    4,435.8     614.47     3,098    6,315    
#> Illiteracy   50     1.17        0.61       0.5      2.8     
#> Life Exp     50    70.879       1.342     67.96    73.6     
#> Murder       50     7.378       3.692      1.4     15.1     
#> HS Grad      50    53.108       8.077     37.8     67.3     
#> Frost        50    104.46      51.981       0       188     
#> Area         50   70,735.88   85,327.3    1,049   566,432
```

By default, describe will calculate the number of observations, mean,
standard deviation, min, and max of the data. The summary functions
applied can be altered and reordered by passing a named vector or list
of functions to the `statistics` argument. The label of each function
should be a string expressing how the function should be expressed in
the table.

Note that `describe` supports user-defined functions as long as the
function maps a vector of data to a single value,
\(f: \mathbb{R}^d \mapsto \mathbb{R}\).

``` r

stat_funcs <- c('Average' = mean, 'St.D.' = sd, 
                'random value' = function(v) sample(v, 1))
rchitex::describe(state_df, title = tbl_title, statistics = stat_funcs)
#>             STATE SUMMARY STATISTICS                  
#> =================================================
#> Statistic    Average     St.D.       random value   
#> -------------------------------------------------
#> Population   4,246.42    4,464.491      3,615       
#> Income        4,435.8     614.47        4,815       
#> Illiteracy     1.17        0.61          1.5        
#> Life Exp      70.879       1.342        70.29       
#> Murder         7.378       3.692         12.5       
#> HS Grad       53.108       8.077         38.5       
#> Frost         104.46      51.981         127        
#> Area         70,735.88   85,327.3       30,225
```

Futher aesthetical modifications are available. Users can adjust the
level of precision by overriding the default value for `max_precision`.

The whole table can be inverted by setting `flip` to True.

``` r
rchitex::describe(state_df[,2:5], title = tbl_title, statistics = stat_funcs, 
         max_precision = 0, flip = TRUE)
#>                STATE SUMMARY STATISTICS                    
#> ======================================================
#> Statistic      Income   Illiteracy   Life Exp   Murder   
#> ------------------------------------------------------
#> Average        4,436        1           71        7      
#> St.D.           614         1           1         4      
#> random value   4,167        1           70        6
```

### Latex and HTML output

Rchitex is likewise capable of producing the code necessary to output
tables in LaTeX and HTML formats. By specifying a `path`, Rchitex will
write a .tex file to the inputted path while also outputting the text
table to the standard consule. The text output can be supressed by
setting `silent` to TRUE.

Rather than simultaneous output, rchitex can directly write LaTeX and
HTML to the consule. This is particularly important if using a Markdown
file (like this document). The `md` argument should either be set to
‘latex’ or ‘html’. Note that you may need to set the chunk option
`results` to ‘asis’

``` r
rchitex::describe(state_df[,2:5], title = tbl_title, statistics = stat_funcs, 
         max_precision = 0, flip = TRUE, md = 'html')
```

<table style="text-align: center;" cellingpadding="100px">

<caption>

State summary statistics

</caption>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td style="text-align:left">

</td>

</tr>

<tr>

<td>

</td>

<td>

Income

</td>

<td>

Illiteracy

</td>

<td>

Life Exp

</td>

<td>

Murder

</td>

</tr>

<tr>

<td colspan="5" style="border-bottom: 1px solid black">

</td>

</tr>

<tr>

<td>

Average

</td>

<td>

4,436

</td>

<td>

1

</td>

<td>

71

</td>

<td>

7

</td>

</tr>

<tr>

<td>

St.D.

</td>

<td>

614

</td>

<td>

1

</td>

<td>

1

</td>

<td>

4

</td>

</tr>

<tr>

<td>

random value

</td>

<td>

3,617

</td>

<td>

1

</td>

<td>

71

</td>

<td>

2

</td>

</tr>

</table>
