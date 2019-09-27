to_tex <- function(stats_mat, path = NA, note='', col_max=7) {
  # TODO Ensure that path is a valid .tex path
  citation <- '% Table generating using RCHITEX by Ben Dempe (2019)'
  preamble <- gen_preamble('Summary Statistics', ncol(stat_mat))
  col_names <- paste0(lapply(colnames(stats_mat),
                             function(x) paste0('\\multicolumn{1}{c}{',x,'} & ')),
                      collapse='')
  col_names <- paste0( 'Statistic & ', substr(col_names, 1, nchar(col_names)-3),'\\\\\n\\hline\\\\[-1.8ex]')

  col_len <- apply(stats_mat, 2, function(x) max(nchar(x)))
  body <- paste0(lapply(rownames(stats_mat), function(r) {
    paste0(r, ' & ',
           paste0(Map(function(el,d)  paste0(format(el, nsmall=max(d-1,0)), collapse=''),
                      stats_mat[r,], col_len), collapse=' & '),
           '\\\\',collapse='')
  }))
  post <- paste0('\\hline \\\\[-1.8ex] \n \\textit{Note}', note,
                 '\n\\end{tabular}\n\\end{table}')
  writeLines(c(citation, preamble, col_names, body, post), path)

}
