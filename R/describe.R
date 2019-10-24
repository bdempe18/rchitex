#' Formats Summary Tables to Text, LaTeX, and HTML
#'
#' Given a dataframe and a list of summary statistics, \code{describe} outputs
#' a text table summarizing the data while simultaneously outputting the
#' equivalent LateX code.
#'
#' @param data Dataframe of variables.
#' @param note Optional note displayed at bottom of table.
#' @param silent No text is outputted if true.
#' @param path File path to write tex file.
#' @param max_precision Maximum number of digits in each table cell.
#' @param statistics A list of functions to be applied to each column of the data frame or tibble.
#'   Each function must be vectorized.
#' @param md Allows for outputting in either latex ("latex") or html ("html") for markdown formatting.
#'   The Markdown chunk must be set to \code{results = "asis"}.
#' @param landscape If true, the Latex table will be landscaped.
#' @param label Latex label.
#' @param title Table title.
#' @param header Includes RCHITEX header as a Latex comment if true.
#' @param as_table True values wrap the underlying Latex Tabular object in a table.
#'
#' @examples
#' df <- data.frame("first" = c(4,5,6), "second" = c(7,5,3))
#' describe(df)
#'
#' stats <- list(N=length, Average = mean, foo = function(x) x[1]+2)
#' describe(df, statistics=stats)
#'
#' @return None
#' @export
describe <- function(data, note='', silent = F, path = NA, max_precision = 6,
                     statistics = list('N' = length,
                                      'Mean' = mean,
                                      'St. Dev' = stats::sd,
                                      'Min' = min,
                                      'Max' = max),
                     md = NA, landscape=FALSE, label='sumStats', title="Summary statistics",
                     header=TRUE, as_table=FALSE) {
  UseMethod("describe")
}

#' @export
describe.default <- function(data, note='', silent = F, path = NULL, max_precision = 3,
                     statistics = list('N' = length,
                                       'Mean' = mean,
                                       'St. Dev' = stats::sd,
                                       'Min' = min,
                                       'Max' = max),
                     md = NULL, landscape=FALSE, label='sumStats', title="Summary statistics",
                     header=TRUE, as_table=FALSE) {

  validate(md=md, max_precision=max_precision)

  d <- structure(list(data    = NA,
                      text    = NA,
                      options = list(caption=title, label=label,
                                     table = as_table, landscape=landscape)),
                 class=c("rchitex", "rtsummary"))
  round_n <- roundr_fac(max_precision=max_precision, min_digs=0)

  # handles statistics as a vector
  if (is.null(names(statistics))) {
    names(statistics) <- paste0('S', as.character(1:length(statistics)))
  }

  col_stats <- function(data, fs) {
    lapply(fs, function(f) round_n(f(data)))
  }

  d$data <- lapply(colnames(data), function(column) {
    col_stats(data[[column]], statistics)
  })

  d$data <- t(matrix(unlist(d$data), ncol=length(data)))
  rownames(d$data) <- colnames(data)
  #if (ncol(d$data) != length(names(statistics)))  stop('Descriptive functions must be vectorized')
  colnames(d$data) <- names(statistics)

  d$options$table <- as_table
  d$options$landscape <- landscape
  d$options$type <- ifelse(is.null(md), "latex", md)
  d$text <- data2text(d$data, title=title)
  if (is.null(md) || tolower(md) == 'latex' || tolower(md) == 'tex') {
    d$code <- summary2tex(stats_mat = d$data, note=note)
    if (landscape) d$code <- lan_wrap(table_wrap(d$code))
    else if (as_table) d$code <- table_wrap(d$code)
    if (header) d$code <- header_wrap(d$code)
  } else if (!is.null(md) || tolower(md) == 'html') {
    d$code <- to_html(d$data, title=title, header=header)
  }

  # dealing with landscaping and tables
  code <- d$code
  if (landscape) code <- lan_wrap(table_wrap(d$code))
  else if (as_table) code <- table_wrap(d$code)

  if (!silent) {
    if (is.null(md)) writeLines(d$text, con=stdout())
    if (!is.null(path)) writeLines(code, con=path)
    if (!is.null(md)) writeLines(code, con=stdout())
  }

  invisible(d)
}

describer.function <- function(data, note='', silent = F, path = NA, max_precision = 6,
                              statistics = list('N' = length,
                                                'Mean' = mean,
                                                'St. Dev' = stats::sd,
                                                'Min' = min,
                                                'Max' = max),
                              md = NA, landscape=FALSE, label='sumStats', title="Summary statistics",
                              header=TRUE) {
  stop('How did we get here?!')
}

validate <- function(md, max_precision) {
  if(!is.null(md) && !(tolower(md) %in% c('html', 'text', 'latex', 'tex'))) {
    stop('output type ', md, ' is not supported. Must be html, latex, or text',
         call. = FALSE)
  }

  if (max_precision < 0) stop('Max precision must be greater than 0.', call. = FALSE)
}
