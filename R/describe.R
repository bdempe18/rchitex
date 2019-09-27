describe <- function(df, note='', silent = F, path = NA, max_precision = 6) {
  statistics <- list('N' = length, 'Mean' = mean, 'St. Dev' = sd, 'Min' = min, 'Max' = max)

  gen_stats <- function(data, fs) {
    lapply(fs, function(f) f(data))
  }

  data  <- lapply(colnames(df), function(column) {
    gen_stats(df[[column]], statistics)
  })

  data <- round(t(matrix(unlist(data), ncol=length(df))), max_precision)
  rownames(data) <- colnames(df)
  colnames(data) <- names(statistics)
  if(!is.na(path)) to_tex(data, path)
  if(!silent) to_text(data)
  invisible(NULL)
}
