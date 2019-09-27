to_tex_m <- function(reg_data, max_precision, fit_char, sig = list(), path = NA, note='', title='',
                     header = FALSE) {
  citation <- '% Table generating using RCHITEX by Ben Dempe (2019)'
  n_mods <- length(reg_data[[1]]$coef)
  col_names <- paste0('(', 1:n_mods,')', sep=' & ', collapse='')
  col_names <- substr(col_names, 1, nchar(col_names) - 3)

  # Preamble
  preamble <- paste0(ifelse(header,gen_preamble(title, n_mods), ''),
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

  #fit <- paste0('\\hline \\\\[-1.8ex]\n',                )
  fit <- unlist(lapply(names(fit_char), function(fc) {
    paste(paste0(c(fc, fit_char[[fc]]),collapse = ' & '),'\\\\', collapse='')
  }))

  p_post <- unlist(lapply(names(sig), function(s) {
    paste0('$^{', s, '}$p$<$', sig[[s]], ' ', collapse='')
  }))

  fit <- paste('\\hline \\\\[-1.8ex]\n', paste(fit, collapse=''), collapse='')
  post <- paste0('\\hline\\hline \\\\[-1.8ex] \n \\textit{Note: } & \\multicolumn{',
                 n_mods,'}{r}{', paste0(p_post, collapse=''), '} \\\\\n',
                 '\\end{tabular}\n\\end{table}', collapse='')

  writeLines(c(citation, preamble, body, fit, post), path)

}
