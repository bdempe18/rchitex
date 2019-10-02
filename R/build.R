#' Formats Tables to Text, LaTeX, and HTML
#'
#' Given a linear regression model, \code{build} constructs a formatted
#'   text table for standard output as well as a Latex table that is
#'   outputted to an provided file path.
#'
#' @param ... Regression models to be included in Table.  Table will display models in the same order as provided.
#' @param dep_names List associating dependant variable names with desired names in table.
#' @param indep_names List of names associating independant variable names with desired names in table.
#' @param note Optional note displayed in bottom row of the table.
#' @param title Title of the table.
#' @param max_precision Maximum number of digits in a table cell.
#' @param report Test statistic to be provided along with the coefficient.
#'   Must be either "p", "t", or "ste". Default value is "p".
#' @param path Output path for the tex file. NA for no tex output.
#' @param rse Transforms to robust standard errors if true. Uses HC1 like STATA.
#' @param silent No text output if true.
#' @param landscape Formats tex table as landscape if true. If landscape is true, make sure to add to include
#'   \code{\documentclass{lscape}} in your LaTeX code.
#' @param stats Fit characteristics to be provided at bottom of table. Must be a string of reporters.
#'   Options include "o" for number of observations, "r" for R2, "a" for adjusted R2 and "f" for f-statistic.
#'   The inputted string determines the order of outputted reporters. Ex \code{stats='oraf'}.
#' @param pre_stats List of optional reporters to be provided above normal fit characteristics.
#'   The list must be formatted as the name of the reporter followed by a a vector of values.
#'   Ex \code{pre_stats=list('R.St.E' = c('True', 'True'))}.
#' @param md The markdown parameter allows for outputting in either latex ("latex") or html ("html") for markdown formatting.
#'   The Markdown chunk must be set to \code{results = "asis"}.
#' @param header Outputs RCHITEX header if true.
#' @param label Latex label.
#'
#' @return Invisible return containing a list of model attributes.
#'
#' @examples
#' data('freeny')
#' mod <- lm(y ~ lag.quarterly.revenue + price.index + income.level +
#'            market.potential, data = freeny)
#' mod2 <- lm(y ~ lag.quarterly.revenue + price.index + income.level,
#'           data = freeny)
#' d <- list('price.index' = 'Price index', 'income.level' = 'Income level',
#'          'market.potential' = 'Market potential',
#'          'lag.quarterly.revenue' = 'Lagged rev')
#' pre <- list('R. St. E' = c('True', 'True'))
#' build(mod, mod2, rse = T, indep_names = d, pre_stats = pre)
#'
#' @export
build <- function(..., dep_names = NA, indep_names = NA, note='', title = 'Model results',
                  max_precision = 3, path = NA, rse = FALSE,
                  silent = FALSE, landscape = FALSE, report = 'p', stats='oraf', pre_stats=NA,
                  md = NA, header = TRUE, label='table') {
  ## checks
  if (rse && !requireNamespace("lmtest", quietly = TRUE) &&
      ! requireNamespace('sandwich', quietly = TRUE)) {
    stop('Packages lmtest and sandwich are required to report robust standard errors')
  }

  mods <- list(...)
  fit_stats <- list(ste = 2, t = 3, p = 4)
  # assigns the independent variable in the correct order with intercept at end by default
  idn <- format_indep_names(mods, indep_names)

  # Iterates over a model fit and returns the coefficients and test statistics
  extract <- function(vn) {
    coefs <- c()
    errors <- c()
    pvals <- c()
    for (m in mods) {
      tryCatch({
        if(rse) {
          m_rse <- lmtest::coeftest(m, vcov = sandwich::vcovHC(m, "HC1"))
          coefs <- c(coefs, m_rse[vn, 1])
          pvals <- c(pvals, m_rse[vn, 4])
          errors <- c(errors, m_rse[vn, fit_stats[[report]]])
        }
        else {
          coefs <- c(coefs, m$coefficients[[vn]])
          errors <- c(errors, summary(m)$coefficients[vn, fit_stats[[report]]])
          pvals <- c(pvals, summary(m)$coefficients[vn, 4])
        }
      }, error = function(e) NA)
    }

    list(coef = coefs, error = errors, pval = pvals)
  }

  line_data <- lapply(names(idn), extract)
  names(line_data) <- idn

  fit_char <- gen_stats(stats, mods, pre_stats)
  #print(line_data)
  sig <- list('***' = 0.01,
              '**'  = 0.05,
              '*'   = 0.1)

  # TODO change writeLines to return strings. Deal with output here
  out <- NA

  if (!is.na(md) && md == 'latex') { out <- to_tex_m(reg_data=line_data, max_precision=max_precision, fit_char=fit_char, sig=sig, path=stdout(), title=title)}
  else if(!is.na(md) && md == 'html') { out <- to_html_m(reg_data=line_data, max_precision=max_precision, fit_char=fit_char, sig=sig, path=stdout(), title=title) }
  else {
    if  (!silent) to_text_m(reg_data=line_data, max_precision=max_precision, fit_char=fit_char, sig=sig, title=title)
    out <- to_tex_m(reg_data=line_data, max_precision=max_precision, fit_char=fit_char, sig=sig, path=path, title=title)
    if (!is.na(path) && landscape) {
      out <- gen_land_table(out, title, label)
    } else if (!is.na(path)) {
      out <- gen_table_header(out, title, label)
    }
  }

  if (!is.na(md)) {
    writeLines(out)
  }

  if (!is.na(path)) {
    writeLines(out, con=path)
  }
  invisible(list(label=label, caption=title, tex=out, options=list(landscape=landscape)))
}
