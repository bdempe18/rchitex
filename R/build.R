#' Formats Regression Tables to Text, LaTeX, and HTML
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
#' @param landscape If true, the Latex table will be landscaped.
#' @param stats Fit characteristics to be provided at bottom of table. Must be a string of reporters.
#'   Options include "o" for number of observations, "r" for R2, "a" for adjusted R2 and "f" for f-statistic.
#'   The inputted string determines the order of outputted reporters. See vignette for full list of
#'   possible values. Ex \code{stats='oraf'}.
#' @param pre_stats List of optional reporters to be provided above normal fit characteristics.
#'   The list must be formatted as the name of the reporter followed by a a vector of values.
#'   Ex \code{pre_stats=list('R.St.E' = c('True', 'True'))}.
#' @param md Allows for outputting in either latex ("latex") or html ("html") for markdown formatting.
#'   The Markdown chunk must be set to \code{results = "asis"}.
#' @param header Includes RCHITEX header as a Latex comment if true.
#' @param label Latex label.
#' @param sig List containing the associations between significance symbols and cut-off p values.
#'   List must be in increasing order of cutoff values (ex \code{list('***' = 0.01, '**' = 0.05, '*' = 0.1)}).
#' @param as_table True values wrap the underlying Latex Tabular object in a table.
#'
#' @return Invisible return containing rchitex object.
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
#' @export
build <- function(..., dep_names = NA, indep_names = NULL, note='', title = 'Model results',
         max_precision = 3, path = NULL, rse = FALSE,
         silent = FALSE, landscape = FALSE, report = 'p', stats='oraf', pre_stats=NA,
         md = NA, header = TRUE, label='table', sig = NULL, as_table=TRUE) {
  UseMethod("build")
}

#' @export
build.default <- function(..., dep_names = NULL, indep_names = NULL, note='', title = 'Model results',
                          max_precision = 3, path = NULL, rse = FALSE,
                          silent = FALSE, landscape = FALSE, report = 'p', stats='all', pre_stats=NA,
                          md = NULL, header = TRUE, label='table', sig = NULL,
                          as_table=TRUE) {
  ## Add validations
  validate(md=md, max_precision=max_precision)
  mods <- list(...)
  idn <- format_indep_names(mods, indep_names)
  round_n <- roundr_fac(max_precision=max_precision, min_digs=1)

  # converts reporters to list indices in a lm mod summary
  fit_stats <- list(ste = 2, t = 3, p = 4)
  if (is.null(sig)) {
    sig <- list("***" = 0.01,
                "**"  = 0.05,
                "*"   = 0.1)
  }

  # helps extract coefs
  extract_coefs <- function(var_name) {
    lapply(mods, function(m) {
      tryCatch({
        # CHECK: ROBUST STANDARD ERRORS DONT MESS WITH COEFFICIENTS....
        round_n(m$coefficients[[var_name]])
      }, error = function(e) NA)
    })
  }

  # violates DRY ... a bit sloppy
  extract_reporter <- function(var_name, r) {
    lapply(mods, function(m) {
      tryCatch({
        if (rse) {
          se <- lmtest::coeftest(m, vcov = sandwich::vcovHC(m, "HC1"))
          round_n(se[var_name, fit_stats[[r]]])
        } else {
          round_n(summary(m)$coefficients[var_name, fit_stats[[r]]])
        }
      }, error = function(e) NA)
    })
  }

  b <- structure(list(i_names  = NA,
                      fits     = NA,
                      coefs    = NA,
                      reporter = NA,
                      sig      = NA,
                      text     = NA,
                      options = list(caption=title, label=label,
                                     table = as_table, landscape=landscape)),
                 class=c("rchitex", "rtreg"))

  if (!is.null(md)) md <- tolower(md)
  b$i_names <- idn
  b$fits <- get_fits(mods, stats=stats, pre_stats=pre_stats, round_n)
  b$coefs <- lapply(names(idn), function(var_name) {
    unlist(extract_coefs(var_name))})
  names(b$coefs) <- names(idn)
  b$reporter <- lapply(names(idn), function(var_name)  {
    unlist(extract_reporter(var_name, report))})
  names(b$reporter) <- names(idn)
  if (report == "p") pvals <- b$reporter
  else  {
    pvals <- lapply(names(idn), function(var_name)  {
      unlist(extract_reporter(var_name, report))})
  }
  b$sig <- lapply(pvals, function(p) sig_at(p, sig))
  names(b$sig) <- names(idn)
  x <- model2text(b$coefs, b$reporter, fits=b$fits, sigs=b$sig, idvn=b$i_names,
                  max_precision=max_precision, note=note,
                  title=title, sig_levels=sig)
  b$text <- x
  if (!silent & is.null(md)) writeLines(x, con=stdout())
  if (is.null(md) || (!is.null(md) && md == 'latex')) {
    b$code <- to_tex_m(reg_data = b$coefs, max_precision = max_precision,
                       fit_char = b$fits, reporter=b$reporter,
                       sig = b$sig, note = note,
                       title = title, idn=b$i_names, sig_levels = sig)
  } else if (!is.null(md) && md == 'html') {
    b$code <- to_html_m(reg_data = b$coefs, max_precision = max_precision,
                        fit_char = b$fits, reporter=b$reporter,
                        sig = b$sig, note = note,
                        title = title, idn=b$i_names, sig_levels = sig)
  }

  #TODO add header
  # dealing with landscaping and tables
  code <- b$code
  if ((is.null(md) || md=='latex' || mod == 'tex') &&  landscape) code <- lan_wrap(table_wrap(code))
  else if ((is.null(md) || md=='latex' || mod == 'tex') && as_table) code <- table_wrap(code)

  if (!is.null(md)) writeLines(code, con=stdout())
  if (!is.null(path)) writeLines(code, con=path)
  invisible(b)
}

#' builder function
build.function <- function(..., dep_names = NA, indep_names = NA, note='', title = 'Model results',
                           max_precision = 3, path = NA, rse = FALSE,
                           silent = FALSE, landscape = FALSE, report = 'p', stats='oraf', pre_stats=NA,
                           md = NA, header = TRUE, label='table', sig = NA) {
  stop('How did we get here?!')
}



