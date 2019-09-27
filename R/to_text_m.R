to_text_m <- function(reg_data, max_precision, fit_char, sig = list(), title='') {
  idn <- names(reg_data)
  long_var_len <- max(nchar(names(reg_data)))
  line_width <- long_var_len + (12 * (length(reg_data[[1]])-1)) + 2

  n_mods <- length(reg_data[[1]]$coef)
  dn <- paste0('(', 1:n_mods,')')

  ## Preamble
  writeLines(strrep('=', line_width))
  writeLines(strrep(' ', long_var_len), sep=' ')
  writeLines(center_text(dn, 12), sep = ' ')
  writeLines('')
  writeLines(strrep('~', line_width))

  ## Estimation
  temp <- lapply(names(reg_data), function(v) {
    writeLines(v, sep = strrep(' ',long_var_len - nchar(v) + 2))
    est <- round(reg_data[[v]]$coef, max_precision)
    est <- ifelse(is.na(reg_data[[v]]$coef), '', paste0(est,sig_at(reg_data[[v]]$pval, sig)))
    est <- paste0(est, strrep(' ', max(12-nchar(as.character(est)),0)))
    writeLines(as.character(est), sep = strrep(' ', max(12 - nchar(as.character(eval(est))),0)))
    writeLines('', sep='\n')
    ep <- paste('(',format(round(reg_data[[v]]$error,3), nsmall=3),')', sep='')
    ep[ep == '(   NA)'] <- ''
    ep <- paste0(ep, strrep(' ', 12-nchar(ep)-2))
    ep <- c(strrep(' ',long_var_len + 2), ep)
    writeLines(ep, sep = ' ')
    writeLines('', sep = ifelse(v==tail(names(reg_data), 1), '\n', '\n\n'))
  })

  ## Fit characterization
  writeLines(strrep('~', line_width))
  temp <- lapply(names(fit_char), function(fc) {
    writeLines(fc, sep = strrep(' ', long_var_len + 2 - nchar(fc)))
    val <- paste0(fit_char[[fc]], strrep(' ', 12 - nchar(as.character(fit_char[[fc]]))))
    writeLines(val , sep=' ')
    writeLines('', sep='\n')
  })
  writeLines(strrep('=', line_width))

}
