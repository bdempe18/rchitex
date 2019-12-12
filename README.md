
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
#> Population   4,246.42    4,464.491      11,197      
#> Income        4,435.8     614.47        4,091       
#> Illiteracy     1.17        0.61          0.9        
#> Life Exp      70.879       1.342        70.29       
#> Murder         7.378       3.692         11.1       
#> HS Grad       53.108       8.077         50.2       
#> Frost         104.46      51.981         161        
#> Area         70,735.88   85,327.3      566,432
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
#> random value   4,809        0           72        5
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
#> <table style = "line-height: 1.6"><caption>State summary statistics</caption>
#> 
#> <tr> <th style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">  </th>
#> <th style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; "> Income </th>
#> <th style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; "> Illiteracy </th>
#> <th style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; "> Life Exp </th>
#> <th style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; "> Murder </th>
#>  </tr>
#> 
#> 
#> <tr> <td style="padding: 5px 0px 0px 10px; border: 1px; text-align: left; "> Average </td>
#>  <td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; "> 4,436 </td>
#> <td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; "> 1 </td>
#> <td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; "> 71 </td>
#> <td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; "> 7 </td>
#>  </tr>
#> 
#> <tr> <td style="padding: 5px 0px 0px 10px; border: 1px; text-align: left; "> St.D. </td>
#>  <td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; "> 614 </td>
#> <td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; "> 1 </td>
#> <td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; "> 1 </td>
#> <td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; "> 4 </td>
#>  </tr>
#> 
#> <tr> <td style="padding: 5px 0px 0px 10px; border: 1px; text-align: left; "> random value </td>
#>  <td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; "> 4,675 </td>
#> <td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; "> 2 </td>
#> <td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; "> 71 </td>
#> <td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; "> 9 </td>
#>  </tr>
#> 
#> 
#> </table>
```

``` r
rchitex::describe(state_df[,2:5], title = tbl_title, statistics = stat_funcs, 
         max_precision = 0, flip = TRUE, md = 'html')
```

<table style="line-height: 1.6">

<caption>

State summary statistics

</caption>

<tr>

<th style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

</th>

<th style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

Income

</th>

<th style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

Illiteracy

</th>

<th style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

Life Exp

</th>

<th style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

Murder

</th>

</tr>

<tr>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: left; ">

Average

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; ">

4,436

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; ">

1

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; ">

71

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; ">

7

</td>

</tr>

<tr>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: left; ">

St.D.

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; ">

614

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; ">

1

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; ">

1

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; ">

4

</td>

</tr>

<tr>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: left; ">

random value

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; ">

4,167

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; ">

2

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; ">

71

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: right; ">

11

</td>

</tr>

</table>

``` r
data(swiss)
mod1 <- lm(data=swiss, Fertility ~ Agriculture + Education)
mod2 <- lm(data=swiss, Fertility ~ Agriculture + Education + Infant.Mortality + Catholic + Examination)

rchitex::build(mod1, mod2, md = 'html')
```

<!-- Table generated by rchitex (Ben Dempe, 2019) -->

<table style = "text-align: center;">

<caption>

Model results

</caption>

<tr style="border-bottom: 1px solid #ccc">

<th style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

</th>

<th style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

1)  
    
    </th>
    
    <th style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">
    
    2)  
        
        </th>
        
        </tr>

<tr>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: left; ">

Agriculture

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

\-0.066<sup></sup>

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

\-0.172<sup>\*\*</sup>

</td>

</tr>

<tr>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

(0.411)

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

(0.019)

</td>

</tr>

<tr>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: left; ">

Education

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

\-0.963<sup>\*\*\*</sup>

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

\-0.871<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

(0.0)

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

(0.0)

</td>

</tr>

<tr>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: left; ">

Infant.Mortality

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

1.077<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

(0.007)

</td>

</tr>

<tr>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: left; ">

Catholic

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

0.104<sup>\*\*\*</sup>

</td>

</tr>

<tr>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

(0.005)

</td>

</tr>

<tr>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: left; ">

Examination

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

\-0.258<sup></sup>

</td>

</tr>

<tr>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

(0.315)

</td>

</tr>

<tr>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: left; ">

(Intercept)

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

84.08<sup>\*\*\*</sup>

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

66.915<sup>\*\*\*</sup>

</td>

</tr>

<tr style="border-bottom: 1px solid #ccc">

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

(0.0)

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

(0.0)

</td>

</tr>

<tr style="border-bottom: 0px solid #ccc">

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: left; ">

Observations

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

47

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

47

</td>

</tr>

<tr style="border-bottom: 0px solid #ccc">

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: left; ">

R2

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

0.449

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

0.707

</td>

</tr>

<tr style="border-bottom: 0px solid #ccc">

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: left; ">

Adjusted R2

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

0.424

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

0.671

</td>

</tr>

<tr style="border-bottom: 0px solid #ccc">

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: left; ">

F Statistic

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

17.945

</td>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; ">

19.761

</td>

</tr>

<tr>

<td style="padding: 5px 0px 0px 10px; border: 1px; text-align: center; " colspan="colspan: 3">

</td>

</tr>

<tr style="border-bottom: 1px solid #ccc">

<td style="padding: left; border: 1px; text-align: center; ">

<em>Note: </em>

</td>

<td style="padding: right; border: 1px; text-align: center; " colspan="colspan: 2">

<sup>***</sup>p\<0.01 <sup>**</sup>p\<0.05 <sup>*</sup>p\<0.1

</td>

</tr>

</table>

### Standard errors

Standard errors can be specified in a number of ways. In most cases, you
may need to apply robust standard errors. The `rse` class converts a
model’s standard errors to robust standard errors similar to STATA’s
robust option (HC1).
