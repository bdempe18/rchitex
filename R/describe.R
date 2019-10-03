#' Formats Summary Tables to Text, LaTeX, and HTML
#'
#' Given a dataframe and a list of sumamry statistics, \code{describe} outputs
#' a text table summarizing the data while simultaneously outputting the
#' equivalent LateX code.
#'
#' @param df Dataframe of variables.
#' @param note Optional note displayed at bottom of table.
#' @param silent No text is outputted if true.
#' @param path File path to write tex file.
#' @param max_precision Maximum number of digits in each table cell.
#' @param statistics A list of functions to be applied to each column of the data frame or tibble.
#'   Each function must be vectorized.
#'
#' @examples
#' df <- data.frame("first" = c(4,5,6), "second" = c(7,5,3))
#' describe(df)
#'
#' statistics <- list(N=length, Average = mean,
#' foo = function(x) x+2)
#' describe(df, statistics=stats)
#'
#' @return None
#'
#' @export
describe <- function(df, note='', silent = F, path = NA, max_precision = 6,
                     statistics = list('N' = length,
                                      'Mean' = mean,
                                      'St. Dev' = stats::sd,
                                      'Min' = min,
                                      'Max' = max),
                     md = NA, landscape=FALSE, label='sumStats', title="Summary statistics") {
  col_stats <- function(data, fs) {
    lapply(fs, function(f) f(data))
  }

  data  <- lapply(colnames(df), function(column) {
    col_stats(df[[column]], statistics)
  })

  data <- round(t(matrix(unlist(data), ncol=length(df))), max_precision)
  rownames(data) <- colnames(df)
  if (ncol(data) != length(names(statistics)))  stop('Descriptive functions must be vectorized')
  colnames(data) <- names(statistics)

  out <- ''
  if(!silent && is.na(md)) to_text(data)
  path <- ifelse(is.na(md), path, stdout())
  if ((!is.na(md) && md == 'latex') || (is.na(md) && is.na(path))) {
    out <- to_tex(data)
    if (!is.na(path) && landscape) {
      out <- gen_land_table(out, title, label)
    } else if (!is.na(path)) {
      out <- gen_table_header(out, title, label)
    }

  } else if (!is.na(md) && md == 'html') {
    out <- to_html(data)
  }



  if (!(is.na(path) && is.na(md))) writeLines(out)


  invisible(list(label=label, caption=title, tex=out, options=list(landscape=landscape)))
}
