#' Formats Regression Tables to Text, LaTeX, and HTML
#'
#' Given a linear regression model, \code{build} constructs a formatted
#'   text table for standard output as well as a Latex table that is
#'   outputted to an provided file path.
#'
#' @param ... Regression models to be included in Table.  Table will display models in the same order as provided.
#' @param dep_names Character vector of dependent variable names. Vector should be listed in the same order
#'   that the models will be displayed (from left to right). If NULL, the columns will be numbers (e.g. (1), (2)....)
#' @param indep_names List of names associating independent variable names with desired names in table.
#' @param note Optional note displayed in bottom row of the table.
#' @param title Title of the table.
#' @param max_precision Maximum number of digits in a table cell.
#' @param report Test statistic to be provided along with the coefficient.
#'   Must be either "p", "t", or "ste". Default value is "p".
#' @param path Output path for the tex file. NA for no tex output.
#' @param silent No text output if true.
#' @param landscape If true, the Latex table will be landscaped.
#' @param stats Fit characteristics to be provided at bottom of table. Must be a string of reporters.
#'   Options include "o" for number of observations, "r" for R2, "a" for adjusted R2 and "f" for f-statistic.
#'   The inputted string determines the order of outputted reporters. See vignette for full list of
#'   possible values. Ex \code{stats='oraf'}.
#' @param annotations List of optional reporters to be provided above normal fit characteristics.
#'   The list must be formatted as the name of the reporter followed by a a vector of values.
#'   Ex \code{annotations=list('R.St.E' = c('True', 'True'))}.
#' @param md Allows for outputting in either latex ("latex") or html ("html") for markdown formatting.
#'   The Markdown chunk must be set to \code{results = "asis"}.
#' @param header Includes RCHITEX header as a Latex comment if true.
#' @param label Latex label.
#' @param sig List containing the associations between significance symbols and cut-off p values.
#'   List must be in increasing order of cutoff values (ex \code{list('***' = 0.01, '**' = 0.05, '*' = 0.1)}).
#' @param as_table True values wrap the underlying Latex Tabular object in a table.
#' @param grouped_label Optional label printed above model names that group models together.
#'   Expected format is a list of the name and a vector of the start and end columns.
#'
#' @return Invisible return containing rchitex object.
#'
#' @examples
#' library(rchitex)
#' data(swiss)
#'
#' mod1 <- lm(data=swiss, Fertility ~ Agriculture + Education)
#' mod2 <- lm(data=swiss, Fertility ~ Agriculture + Education + Infant.Mortality + Catholic + Examination)
#' lmod <- glm(data=swiss, I(Fertility > mean(Fertility)) ~ Agriculture +
#'               Education + Infant.Mortality + Catholic + Examination,
#'             family=binomial(link='logit'))
#'
#' indep_names <- list('Agriculture' = "Agriculture share", 'Education' = 'Total education',
#'                     'Infant.Mortality' = 'Infant Mortality',
#'                     'Catholic' = 'Catholic share', 'Examination' = 'Exam')
#' dep_names <- c('Fert.', 'Fert.', 'Fert.')
#' grouped_label <- list('OLS' = c(1,2), 'logit' = 3)
#' annotations <- list('Full dataset' = c('No', 'Yes', 'Yes'))
#' sig <- list('***' = 0.001, '**' = 0.025, "'"=0.15)
#'
#' build(mod1, mod2, lmod, indep_names=indep_names, dep_names=dep_names,
#' grouped_label=grouped_label, annotations=annotations, sig=sig,
#' title='Example regression from Swiss dataset', report='t',
#' stats='orc')
#'
#' @export
build <- function(..., dep_names = NULL, indep_names = NULL, note='', title = 'Model results',
         max_precision = 3, path = NULL, silent = FALSE, landscape = FALSE, report = 'p',
         stats='oraf', annotations=NULL, md = NULL, header = TRUE, label='table', sig = NULL,
         as_table=TRUE, grouped_label=NULL) {
  UseMethod("build")
}

#' @export
build.default <- function(..., dep_names = NULL, indep_names = NULL, note='', title = 'Model results',
                          max_precision = 3, path = NULL, silent = FALSE, landscape = FALSE,
                          report = 'p', stats='all', annotations=NULL,
                          md = NULL, header = TRUE, label='table', sig = NULL,
                          as_table=TRUE, grouped_label = NULL) {
  ## Add validations
  # TODO, ensure that every element of ... is valid
  validate(md=md, max_precision=max_precision)
  mods <- list(...)
  idn <- format_indep_names(mods, indep_names)
  # handles the case that no dep names are provided
  if (is.null(dep_names)) dep_names <- paste0('(', seq(1,length(mods)),')')
  # handles the case that dep names length exceeds # of models
  else if (length(dep_names) > length(mods)) {
    stop("Too many dependent variable names provided", call.=FALSE)
    # handles the case that there are NAs in dep names or
    # not enough dep names are provided
  } else if (length(dep_names) < length(mods) | any(is.na(dep_names))) {
    temp <- c(dep_names, rep(NA, length(mods) - length(dep_names)))
    dep_names <- unlist(lapply(seq_along(temp), function(i) {
      if (is.na(temp[i])) mods[[i]]$terms[[2]]
      else temp[i]
    }))
  }

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
        round_n(summary(m)$coefficients[var_name, fit_stats[[r]]])
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
  b$fits <- get_fits(mods, stats=stats, annotations=annotations, roundr=round_n,
                     sig=sig)
  b$coefs <- lapply(names(idn), function(var_name) {
    unlist(extract_coefs(var_name))})
  names(b$coefs) <- names(idn)
  b$dep_names <- dep_names
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
                  title=title, sig_levels=sig, dn=b$dep_names,
                  grouped_label=grouped_label)
  b$text <- x
  if (!silent & is.null(md)) writeLines(x, con=stdout())
  if (is.null(md) || (!is.null(md) && md == 'latex')) {
    b$code <- to_tex_m(reg_data = b$coefs, max_precision = max_precision,
                       fit_char = b$fits, reporter=b$reporter,
                       sig = b$sig, note = note,
                       title = title, idn=b$i_names, sig_levels = sig,
                       dn=b$dep_names, grouped_label=grouped_label)
  } else if (!is.null(md) && md == 'html') {
    b$code <- to_html_m(reg_data = b$coefs, max_precision = max_precision,
                        fit_char = b$fits, reporter=b$reporter,
                        sig = b$sig, note = note,
                        title = title, idn=b$i_names, sig_levels = sig,
                        col_names = b$dep_names, grouped_label=grouped_label)
    b$code <- paste0(b$code, collapse='')
  }

  #TODO add header
  # dealing with landscaping and tables
  code <- b$code
  if ((is.null(md) || md=='latex' || md == 'tex') &&  landscape) code <- lan_wrap(table_wrap(code))
  else if ((is.null(md) || md=='latex' || md == 'tex') && as_table) code <- table_wrap(code)

  if (header) code <- gen_header(code, md)
  if (!is.null(md)) writeLines(code, con=stdout())
  if (!is.null(path)) writeLines(code, con=path)
  invisible(b)
}

#' builder function
#' @param ... Shouldn't be used so arguments not specified
build.function <- function(...) {
  stop('How did we get here?!')
}



