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
    writeLines('', sep = ifelse(v==utils::tail(names(reg_data), 1), '\n', '\n\n'))
  })

  ## Fit characterization
  writeLines(strrep('~', line_width))
  temp <- lapply(names(fit_char), function(fc) {
    writeLines(fc, sep = strrep(' ', max(long_var_len + 2 - nchar(fc),0)))
    val <- paste0(fit_char[[fc]], strrep(' ', 12 - nchar(as.character(fit_char[[fc]]))))
    writeLines(val , sep=' ')
    writeLines('', sep='\n')
  })
  writeLines(strrep('=', line_width))

}

to_text <- function(stat_mat, colmax = 7) {
  tbl_len<- apply(stat_mat, 2, function(x) max(nchar(x))) # TODO min max goes here
  tbl_len <- unlist(Map(function(a,b) max(a, nchar(b)), tbl_len, colnames(stat_mat)))
  lbls <- c('Statistic', colnames(stat_mat))
  tbl_len<- c(max(nchar(c('Statistic', rownames(stat_mat)))), tbl_len)
  line_width =  sum(tbl_len) + 2 * length(lbls) - 1
  writeLines(strrep('=', line_width))

  fix_width <- function(text, max_width) {
    paste(strrep(' ', max_width - nchar(text)), text, ' ', sep='')
  }

  # what the fuck is this
  center_align <- function(text, max_width, digits) {
    out <- Map(function(s, w, d) {
      n <- as.character(format(as.numeric(s), nsmall=max(d-2,0)))
      paste(strrep(' ', max((w-nchar(n))%/%2,0)),n,
            strrep(' ', max((w-nchar(n))%/%2,0)), sep=' ')
    }, text[2:length(text)], max_width[2:length(max_width)], digits)
    c(paste(text[1], strrep(' ', max_width[1] - nchar(text[1]))),out, sep='')
  }

  digits <- apply(stat_mat, 2, function(x) max(nchar(x)))

  first_row <- paste0(Map(fix_width, lbls, tbl_len), collapse=' ')
  writeLines(first_row, sep=' ')
  writeLines('\n', sep='')
  writeLines(strrep('-', line_width))
  centered <- paste0(lapply(rownames(stat_mat), function(r) {
    paste0(center_align(c(r, stat_mat[r,]), tbl_len, digits), collapse='')
  }), collapse='\n')
  writeLines(centered)
  writeLines(strrep('-', line_width))

}
