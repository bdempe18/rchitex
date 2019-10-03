to_tex <- function(stats_mat, note='', col_max=7) {
  # TODO Ensure that path is a valid .tex path
  citation <- '% Table generating using RCHITEX by Ben Dempe (2019)\n'
  preamble <- gen_preamble('Summary Statistics', ncol(stats_mat))
  print(preamble)
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
                 '\n\\end{tabular}')
  c(citation, preamble, col_names, body, post)
}

to_tex_m <- function(reg_data, max_precision, fit_char, sig = list(), path = NA, note='', title='',
                     header = FALSE) {
  citation <- '% Table generating using RCHITEX by Ben Dempe (2019)'
  n_mods <- length(reg_data[[1]]$coef)
  col_names <- paste0('(', 1:n_mods,')', sep=' & ', collapse='')
  col_names <- substr(col_names, 1, nchar(col_names) - 3)

  # Preamble
  preamble <- paste0(gen_preamble(title, n_mods),
                     '& \\multicolumn{', n_mods, '}{c}{\\textit{Dependent variable:}}\\\\\n\t',
                     '\\cline{2-', n_mods+1, '}\n\\\\[-1.8ex] & ', col_names, '\\\\',
                     '\\hline \\\\[-1.8ex]\n', sep='')

  ## Estimation
  body <- unlist(lapply(names(reg_data), function(r) {
    ests <- ifelse(is.na(reg_data[[r]]$coef), '', format(reg_data[[r]]$coef, nsmall=1, big.mark = ','))
    ests <- paste0(ests, '$^{', sig_at(reg_data[[r]]$pval, sig), '}$')
    errs <- ifelse(is.na(reg_data[[r]]$error), '',
                   paste('(',format(round(reg_data[[r]]$error, 4),
                                    nsmall=3, big.mark = ', ', scientific = FALSE),') ', sep=''))
    paste(r, ' & ', paste0(ests, collapse=' & '), '\\\\\n',
          ' & ', paste0(errs, collapse='&'), '\\\\\n',
          strrep(' & ', n_mods), '\\\\\n')
  }))

  fit <- unlist(lapply(names(fit_char), function(fc) {
    paste(paste0(c(fc, fit_char[[fc]]),collapse = ' & '),'\\\\', collapse='')
  }))

  p_post <- unlist(lapply(names(sig), function(s) {
    paste0('$^{', s, '}$p$<$', sig[[s]], ' ', collapse='')
  }))

  fit <- paste('\\hline \\\\[-1.8ex]\n', paste(fit, collapse=''), collapse='')
  post <- paste0('\\hline\\hline \\\\[-1.8ex] \n \\textit{Note: } & \\multicolumn{',
                 n_mods,'}{r}{', paste0(p_post, collapse=''), '} \\\\\n',
                 '\\end{tabular}', collapse='')

  #writeLines(c(citation, preamble, body, fit, post), path)
  c(citation, preamble, body, fit, post)

}

gen_table_header <- function(tex, cap, lbl) {
  c('\\begin{table}[!htb]\n',
        '\t\\centering\n',
        '\t\\caption{', cap, '}\n',
        '\t\\label{', lbl, '}\n',
        tex,
        '\\end{table}\n',
        sep = '')
}

gen_land_table <- function(tex, cap, lbl) {
  c('\\begin{landscape}', gen_table_header(tex, cap=cap, lbl=lbl), '\\end{landscape}')
}

gen_preamble <- function(title, n_cols) {
  paste0('\\begin{tabular}{@{\\extracolsep{5pt}}l ',
         strrep('c',n_cols),'}',
         '\\\\[-1.8ex]\\hline\\hline \\\\[-1.8ex]\n')
}
